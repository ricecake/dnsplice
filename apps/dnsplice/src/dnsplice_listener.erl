-module(dnsplice_listener).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start_link/0,
	send_reply/2
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

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, #{}, []).

send_reply(Packet, {IP, Port}) ->
	gen_server:cast(?SERVER, {reply, IP, Port, Packet}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	Port = application:get_env(dnsplice, listen_port, 5300),
	Opts = application:get_env(dnsplice, listen_opts, []),
	{ok, Socket} = gen_udp:open(Port, [binary |Opts]),
	{ok, Args#{ socket => Socket }}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({reply, IP, Port, Packet}, #{ socket := Socket } = State) ->
	ok = gen_udp:send(Socket, IP, Port, Packet),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp, Socket, IP, InPortNo, Packet}, #{ socket := Socket } = State) ->
	{ok, _} = dnsplice_worker:handle(Packet, {IP, InPortNo}),
	{noreply, State};
handle_info(Info, State) ->
	io:format("Unhandled: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
