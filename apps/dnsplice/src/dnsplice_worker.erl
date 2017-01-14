%%@doc DNSplice worker
%%
%% This module is responsible for processing DNS requests,
%% handing them off to a worker, and routing the replys back to
%% the client.
%%
%%@end
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
	code_change/3
]).

%% ------------------------------------------------------------------
%% Defines and includes
%% ------------------------------------------------------------------

-include_lib("dnsplice/src/records.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-define(SERVER, ?MODULE).
-define(record_to_list(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-define(record_to_map(Rec, Ref), maps:from_list(?record_to_list(Rec, Ref))).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%%@doc starts a worker process
%%
%% function that starts a worker process.  Should be called by the
%% dnsplice worker supervisor.
%%
%%@end

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%%@doc Starts the process of forwarding DNS requests
%%
%% Starts up a DNS request worker, and starts the process
%% of forwarding packets to the various backends and selecting
%% which response should be returned to the client.
%%
%%@end

-spec handle(Packet :: binary(), Sender :: {IP :: inet:ip_address(), Port :: inet:port_number()}) -> ok.

handle(Packet, Sender) ->
	{ok, _Pid} = dnsplice_worker_sup:start_worker(Packet, Sender),
	ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({Packet, Sender}) ->
	gen_server:cast(self(), setup_route),
	State = #{
		packet => Packet,
		sender => Sender,
		replies => #{}
	},
	Timeout = application:get_env(dnsplice, packet_timeout, timer:seconds(1)),
	erlang:send_after(Timeout, self(), timeout),
	{ok, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(setup_route, #{ packet := Packet } = State) ->
	{ok, Servers} = application:get_env(servers),
	Sockets = maps:from_list([ forward_packet(Backend, Packet) || Backend <- Servers ]),
	{Route, Alerts} = try
		{ok, #dns_rec{ qdlist = [#dns_query{domain = Domain}] }} = inet_dns:decode(Packet),
		dnsplice:get_domain_route(to_lower(Domain))
	catch
		Type:Error ->
			lager:error("Encountered ~w:~w finding route", [Type, Error]),
			{ok, DefaultBackend} = application:get_env(default_backend),
			{ok, DefaultAlerts} = application:get_env(default_alerts),
			{DefaultBackend, DefaultAlerts}
	end,
	{noreply, State#{ route => Route, alerts => Alerts, sockets => Sockets }};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp, Socket, _IP, _InPortNo, ReplyPacket}, #{ replies := Replies, sockets := Sockets } = State) ->
	ok = gen_udp:close(Socket),
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
			Alerts = maps:get(alerts, State),
			if
				not Alerts -> ok;
				Alerts ->
					lager:info("Entering Analysis Phase"),
					{ok, Diffs} = diff_analyze(maps:to_list(NewReplies)),
					case Diffs of
						[] -> ok;
						DiffList ->
							lager:warning("Found Difference: ~p", [DiffList]),
							ok
					end
			end,
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

-spec forward_packet({Label :: binary(), Address :: inet:ip_address()}, Packet :: binary()) ->
	{Socket :: port(), Label :: binary()}.

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
	{ok, [ {lists:usort(Holders), normalize_dns_record(Val)} || {Val, Holders} <- maps:to_list(Vals) ]}.

-type comparison_item() :: {Name :: any(), Value :: any()}.
-type comparison_item(_A) :: comparison_item().
-type comparison_result() :: {A :: comparison_item(), B :: comparison_item(), Result :: boolean()}.
-type comparison_result(A, B) :: {comparison_item(A), comparison_item(B), Result :: boolean()}.

-spec pairwise_diff(Items :: []|list(comparison_item())) -> list(comparison_result()).

pairwise_diff([]) -> [];
pairwise_diff(List) when is_list(List) ->
	do_pairwise_diff(List, []).

do_pairwise_diff([_], Acc) ->
	lists:sort(lists:flatten(Acc));
do_pairwise_diff([This |Rest], Acc) ->
	Comparisons = [compare_items(This, Other) || Other <- Rest],
	do_pairwise_diff(Rest, [Comparisons |Acc]).

-spec compare_items(comparison_item(A), comparison_item(B)) -> comparison_result(A, B).

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
expand_dns_rec_field({arlist, OPTs}) ->
	RRs    = [clean_rr(?record_to_map(dns_rr, RR)) || RR <- OPTs, dns_rr == element(1, RR) ],
	RROpts = [all_rr_cleanup(?record_to_map(dns_rr_opt, OPT)) || OPT <- OPTs, dns_rr_opt == element(1, OPT)],
	{arlist, RRs ++ RROpts};
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
clean_rr(#{ data := Data } = RR) when is_list(Data)  -> all_rr_cleanup(RR#{ data := list_to_binary(Data) });
clean_rr(#{ data := Data } = RR) when is_tuple(Data) -> all_rr_cleanup(RR#{ data := tuple_to_list(Data) }).

all_rr_cleanup(#{ bm := BitMap } = RR) when is_list(BitMap) -> all_rr_cleanup(RR#{ bm := list_to_binary(BitMap) });
all_rr_cleanup(#{ func := _ } = RR) -> all_rr_cleanup(maps:without([func], RR));
all_rr_cleanup(#{ domain := Domain } = RR) when is_list(Domain) -> all_rr_cleanup(RR#{ domain := list_to_binary(Domain) });
all_rr_cleanup(RR) -> RR.

to_lower(String) when is_list(String) -> string:to_lower(String);
to_lower(String) when is_binary(String) ->
	<< <<(do_lower(Char)):8>> || <<Char:8>> <= String >>.

do_lower(Char) when Char >= 65 andalso Char =< 90 -> Char + 32;
do_lower(Char) -> Char.

-ifdef(TEST).

basic_test_() ->
	{"DNSplice worker Tests", [
		{"Utility functions", [
			{"lowercase helper", [
				{"works on binary", ?_assertMatch(<<"az!1az">>, to_lower(<<"AZ!1az">>))},
				{"works on list", ?_assertMatch("az!1az", to_lower("AZ!1az"))}
			]},
			{"item comparison", [
				{"values differ", [
					{"First key greater", ?_assertMatch({{1, a},{2, b},false}, compare_items({2, b},{1, a}))},
					{"second key greater", ?_assertMatch({{1, a},{2, b},false}, compare_items({1, a},{2, b}))},
					{"keys equal", ?_assertMatch({{test, b},{test, a},false}, compare_items({test, b},{test, a}))}
				]},
				{"values match", [
					{"First key greater", ?_assertMatch({{1, b},{2, b},true}, compare_items({2, b},{1, b}))},
					{"second key greater", ?_assertMatch({{1, a},{2, a},true}, compare_items({1, a},{2, a}))},
					{"keys equal", ?_assertMatch({{test, b},{test, b},true}, compare_items({test, b},{test, b}))}
				]}
			]}
		]}
	]}.

-endif.
