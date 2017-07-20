-module(dnsplice_worker_tcp).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
%init([]) -> {ok, undefined}.

init({Ref, Socket, Transport, _Opts = []}) ->
	ok = ranch:accept_ack(Ref),
	ok = Transport:setopts(Socket, [{active, 10}, {packet, 2}]),
	State = #{
		socket    => Socket,
		transport => Transport
	},
	gen_server:enter_loop(?MODULE, [], State).

handle_info({tcp, Socket, Data}, #{ socket := Socket, transport := Transport } = State ) ->
	{ok, Servers} = application:get_env(dnsplice, servers),
	Sockets = maps:from_list([ forward_packet(Backend, Data) || Backend <- Servers ]),

	Transport:send(Socket, Data),
	io:format("~p~n", [#{ dns => inet_dns:decode(Data), data => Data, pid => self(), socket => Socket, s => Sockets }]),
%	NewState = case inet_dns:decode(Data) of
%		{error
	Transport:setopts(Socket, [{active, 1}]),
	{noreply, State};
handle_info({tcp, Socket, Data}, State) ->
	io:format("~p~n", [{Socket, Data}]),
	{noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
	{noreply, State};
%	{stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};
handle_info(timeout, State) ->
	{stop, normal, State};
handle_info(_Info, State) ->
	{stop, normal, State}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


-spec forward_packet({Label :: binary(), Address :: inet:ip_address()}, Packet :: binary()) ->
        {Socket :: port(), Label :: binary()}.

forward_packet({Label, Address}, Packet) ->
	{ok, Socket} = gen_tcp:connect(Address, 53, [binary, {packet, 2}]),
	ok = gen_tcp:send(Socket, Packet),
	{Socket, Label}.

