%%@doc DNSplice request listener
%%
%% This module is responsible for handling incoming DNS requests,
%% handing them off to a worker, and routing the replys back to
%% the client.
%%
%%@end
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

%%@doc starts the listener process
%%
%% function that starts the listener process.  Should be called by the
%% dnsplice top level supervison.
%%
%%@end

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, #{}, []).

%%@doc sends a reply back to the client
%%
%% Handles routing a reply back from the workers to the requesting
%% client.
%%
%%@end

-spec send_reply(Response :: binary(), {IP :: inet:ip_address(), Port :: inet:port_number()}) -> 'ok'.

send_reply(Packet, {IP, Port}) ->
	gen_server:cast(?SERVER, {reply, IP, Port, Packet}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	Port = application:get_env(dnsplice, listen_port, 5300),
	Opts = application:get_env(dnsplice, listen_opts, []),
	{ok, Socket} = gen_udp:open(Port, [binary, {active, 100}, {read_packets, 1000}, {recbuf, 1024*1024} |Opts]),
	{ok, Args#{ socket => Socket }}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({reply, IP, Port, Packet}, #{ socket := Socket } = State) ->
	ok = gen_udp:send(Socket, IP, Port, Packet),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({udp, Socket, IP, InPortNo, Packet}, #{ socket := Socket } = State) ->
	ok = try
		dnsplice_worker:handle(Packet, {IP, InPortNo})
        catch
                Type:Error ->
                        lager:error("Encountered ~w:~w while routing", [Type, Error])
	end,
	ok = inet:setopts(Socket, [{active, 1}]),
	{noreply, State};
handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
