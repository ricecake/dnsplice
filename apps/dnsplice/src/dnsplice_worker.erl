-module(dnsplice_worker).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/1,
	handle/2
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3,
	pairwise_diff/1
]).

%% ------------------------------------------------------------------
%% Defines and includes
%% ------------------------------------------------------------------

-include_lib("dnsplice/src/records.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-define(SERVER, ?MODULE).
-define(record_to_list(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-define(record_to_map(Rec, Ref), maps:from_list(?record_to_list(Rec, Ref))).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

handle(Packet, Sender) ->
	dnsplice_worker_sup:start_worker(Packet, Sender).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Packet, Sender}) ->
	{ok, Servers} = application:get_env(servers),
	Sockets = [ forward_packet(Backend, Packet) || Backend <- Servers ],
	State = #{
		packet => Packet,
		sender => Sender,
		replies => #{},
		sockets => maps:from_list(Sockets)
	},
	gen_server:cast(self(), determine_route),
	Timeout = application:get_env(dnsplice, packet_timeout, timer:seconds(1)),
	erlang:send_after(Timeout, self(), timeout),
	{ok, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(determine_route, #{ packet := Packet } = State) ->
	{ok, Default} = application:get_env(default_backend),
	Choice = try
		{ok, #dns_rec{ qdlist = [#dns_query{domain = Domain}] }} = inet_dns:decode(Packet),
		DomainNames = build_subdomains(Domain),
		mnesia:activity(async_dirty, fun
			FindRoute([]) -> Default;
			FindRoute([Next |Rest]) ->
				case mnesia:read(route, Next) of
					[] -> FindRoute(Rest);
					[#route{ backend = Backend }] -> Backend
				end
		end, [DomainNames])
	catch
		_:_ -> Default
	end,
	{noreply, State#{ route => Choice }};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp, Socket, _IP, _InPortNo, ReplyPacket}, #{ replies := Replies, sockets := Sockets } = State) ->
	#{ Socket := SockName } = Sockets,
	Done = SockName =:= maps:get(route, State),
	ok = if
		not Done -> ok;
		Done -> dnsplice_listener:send_reply(ReplyPacket, maps:get(sender, State))
	end,
	NewReplies = Replies#{ SockName => ReplyPacket },
	RemainingReplies = maps:size(Sockets) - maps:size(NewReplies),
	NewState = State#{ replies := NewReplies },
	if
		RemainingReplies >  0 -> {noreply, NewState};
		RemainingReplies =< 0 ->
			ok = diff_analyze(maps:to_list(NewReplies)),
			{stop, normal, NewState}
	end;
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_subdomains(Domain) when is_list(Domain)-> build_subdomains(list_to_binary(Domain));
build_subdomains(Domain) when is_binary(Domain)->
	Labels = binary:split(Domain, <<".">>, [global]),
	do_subdomain_build(lists:reverse(Labels), []).

do_subdomain_build([], Acc) -> Acc;
do_subdomain_build([Chunk |Rest], []) ->
	do_subdomain_build(Rest, [Chunk]);
do_subdomain_build([Chunk |Rest], [Last |_] = Acc) ->
	do_subdomain_build(Rest, [<<Chunk/binary, $., Last/binary>> |Acc]).

forward_packet({Label, Address}, Packet) ->
	{ok, Socket} = gen_udp:open(0, [binary]),
	ok = gen_udp:send(Socket, Address, 53, Packet),
	{Socket, Label}.

diff_analyze(List) ->
	DiffList = pairwise_diff(List),
	Vals = lists:foldl(fun
		({_, _, true},  Acc)-> Acc;
		({{AName, AVal}, {BName, BVal}, false}, Acc)->
			AHolders = maps:get(AVal, Acc, []),
			BHolders = maps:get(BVal, Acc, []),
			Acc#{ AVal => [AName |AHolders], BVal => [BName |BHolders]}
	end, #{}, DiffList),
	[ io:format("~p~n", [{lists:usort(Holders), normalize_dns_record(Val)}]) || {Val, Holders} <- maps:to_list(Vals) ],
	ok.


pairwise_diff(List) when is_list(List) ->
	do_pairwise_diff(List, []).

do_pairwise_diff([_], Acc) ->
	lists:sort(lists:flatten(Acc));
do_pairwise_diff([This |Rest], Acc) ->
	Comparisons = [compare_items(This, Other) || Other <- Rest],
	do_pairwise_diff(Rest, [Comparisons |Acc]).

compare_items({AName, AVal} = A, {BName, BVal} = B) when AName < BName -> {A, B, AVal =:= BVal};
compare_items({AName, AVal} = A, {BName, BVal} = B) when AName > BName -> {B, A, AVal =:= BVal};
compare_items({_AName, AVal} = A, {_BName, BVal} = B) -> {A, B, AVal =:= BVal}.

normalize_dns_record(Packet) when is_binary(Packet) ->
	{ok, Record} = inet_dns:decode(Packet),
	normalize_dns_record(Record);
normalize_dns_record(#dns_rec{} = Record) ->
	maps:from_list([ expand_dns_rec_field(Field) || Field <- ?record_to_list(dns_rec, Record)]).

expand_dns_rec_field({header, Header}) -> {header, ?record_to_map(dns_header, Header)};
expand_dns_rec_field({qdlist, QDs}) -> {qdlist, [ all_rr_cleanup(?record_to_map(dns_query, Q)) || Q <- QDs]};
expand_dns_rec_field({arlist, OPTs}) -> {arlist, [ all_rr_cleanup(?record_to_map(dns_rr_opt, OPT)) || OPT <- OPTs]};
expand_dns_rec_field({Section, RRs}) -> {Section, [ clean_rr(?record_to_map(dns_rr, RR)) || RR <- RRs]}.

clean_rr(#{ type := a, data := Data } = RR) ->
	all_rr_cleanup(RR#{ data := list_to_binary(inet_parse:ntoa(Data)) });
clean_rr(#{ type := aaaa, data := Data } = RR) ->
	all_rr_cleanup(RR#{ data := list_to_binary(inet_parse:ntoa(Data)) });
clean_rr(#{ type := mx, data := {Prio, Exchange} } = RR) ->
	all_rr_cleanup(RR#{ data := #{
		priority => Prio,
		exchange => list_to_binary(Exchange)
	} });
clean_rr(#{ type := soa,  data := {MName,RName,Serial,Refresh,Retry,Expiry,Minimum} } = RR) ->
	all_rr_cleanup(RR#{ data := #{
		mname   => list_to_binary(MName),
		rname   => list_to_binary(RName),
		serial  => Serial,
		refresh => Refresh,
		retry   => Retry,
		expiry  => Expiry,
		minimum => Minimum
	} });
clean_rr(RR) -> all_rr_cleanup(RR).

all_rr_cleanup(#{ bm := BitMap } = RR) when is_list(BitMap) -> all_rr_cleanup(RR#{ bm := list_to_binary(BitMap) });
all_rr_cleanup(#{ func := _ } = RR) -> all_rr_cleanup(maps:without([func], RR));
all_rr_cleanup(#{ domain := Domain } = RR) when is_list(Domain) -> all_rr_cleanup(RR#{ domain := list_to_binary(Domain) });
all_rr_cleanup(RR) -> RR.
