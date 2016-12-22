-module(dnsplice_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("kernel/src/inet_dns.hrl").

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
	{ok, SocketA} = gen_udp:open(0, [binary]),
	ok = gen_udp:send(SocketA, {8, 8, 8, 8}, 53, Packet),
	{ok, SocketB} = gen_udp:open(0, [binary]),
	ok = gen_udp:send(SocketB, {8, 8, 4, 4}, 53, Packet),
	State = #{
		start  => erlang:monotonic_time(),
		packet => Packet,
		sender => Sender,
		done => false,
		replies => #{},
		sockets => #{
			SocketA => sockA,
			SocketB => sockB
		}
	},
	gen_server:cast(self(), determine_route),
	erlang:send_after(timer:seconds(1), self(), timeout),
	{ok, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(determine_route, #{ packet := Packet } = State) ->
	{ok, #dns_rec{ qdlist = [#dns_query{domain = Domain}] }} = inet_dns:decode(Packet),
	io:format("~p~n", [build_subdomains(Domain)]),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp, Socket, _IP, _InPortNo, ReplyPacket}, #{ done := Done, sender := Sender, replies := Replies, sockets := Sockets } = State) ->
	ok = if
		not Done -> dnsplice_listener:send_reply(ReplyPacket, Sender);
		Done -> ok
	end,
	#{ Socket := SockName } = Sockets,
	NewReplies = Replies#{ SockName => ReplyPacket },
	RemainingReplies = maps:size(Sockets) - maps:size(NewReplies),
	NewState = State#{ done := true, replies := NewReplies },
	if
		RemainingReplies >  0 -> {noreply, NewState};
		RemainingReplies =< 0 -> {stop, normal, NewState}
	end;
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	io:format("~p~n", [State#{ stop => erlang:monotonic_time() }]),
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

