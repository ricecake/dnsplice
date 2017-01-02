-module(dnsplice_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("kernel/src/inet_dns.hrl").
-include_lib("dnsplice/src/records.hrl").

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

handle_info({udp, Socket, _IP, _InPortNo, ReplyPacket}, #{ route := Choice, sender := Sender, replies := Replies, sockets := Sockets } = State) ->
	#{ Socket := SockName } = Sockets,
	Done = Choice =:= SockName,
	ok = if
		not Done -> ok;
		Done -> dnsplice_listener:send_reply(ReplyPacket, Sender)
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
	%io:format("~p~n", [State]),
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
	[ io:format("~p~n", [{lists:usort(Holders), Val, inet_dns:decode(Val)}]) || {Val, Holders} <- maps:to_list(Vals) ],
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
