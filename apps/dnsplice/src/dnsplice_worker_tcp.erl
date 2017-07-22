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
		req_socket    => Socket,
		transport     => Transport,
		proxy_sockets => #{},
		replies       => #{},
		pending       => false
	},
	gen_server:enter_loop(?MODULE, [], State).

handle_info({tcp, Socket, Packet}, #{ req_socket := Socket, transport := Transport, proxy_sockets := Proxy, replies := Replies } = State ) ->
	{ok, Servers} = application:get_env(dnsplice, servers),
	ReqRef = erlang:make_ref(),
	Sockets = maps:from_list([ { Sock, {ReqRef, Name}} || { Sock, Name } <- [ forward_packet(Backend, Packet) || Backend <- Servers ]]),
	

%	Transport:send(Socket, Packet),
        {Route, Alerts} = try
                {ok, #dns_rec{ qdlist = [#dns_query{domain = Domain}] }} = inet_dns:decode(Packet),
                {FoundRoute, FoundAlert} = dnsplice:get_domain_route(to_lower(Domain)),
                lager:md([
                        {domain, Domain},
                        {backend, FoundRoute},
                        {alerts, FoundAlert}
                ]),
                {FoundRoute, FoundAlert}
        catch
                Type:Error ->
                        lager:error("Encountered ~w:~w finding route", [Type, Error]),
                        {ok, DefaultBackend} = application:get_env(default_backend),
                        {ok, DefaultAlerts} = application:get_env(default_alerts),
                        lager:md([
                                {domain, 'unparsable domain'},
                                {backend, DefaultBackend},
                                {alerts, DefaultAlerts}
                        ]),
                        {DefaultBackend, DefaultAlerts}
        end,
	Transport:setopts(Socket, [{active, 1}]),
        NewState = State#{
		route => Route,
		alerts => Alerts,
		replies := Replies#{ ReqRef => #{} },
		proxy_sockets := maps:merge(Proxy, Sockets),
		pending := true
	},

	io:format("REQ: ~p~n", [#{ state => NewState }]),
	{noreply, NewState};
handle_info({tcp, Socket, ReplyPacket}, #{ proxy_sockets := Sockets, req_socket := Requester, transport := Transport, replies := Replies } = State) ->
        ok = gen_tcp:close(Socket),
        #{ Socket := { ReqRef, SockName} } = Sockets,
        Done = SockName =:= maps:get(route, State),
        ok = if
                not Done -> ok;
                Done -> Transport:send(Requester, ReplyPacket)
        end,
	#{ ReqRef := ReqReplies } = Replies,
        NewReqReplies = ReqReplies#{ SockName => ReplyPacket },
        RemainingReplies = length([ SockRef || {SockRef, _} <- maps:values(Sockets), SockRef =:= ReqRef ]) - maps:size(NewReqReplies),
        NewReplies = if
                RemainingReplies >  0 -> Replies#{ ReqRef := NewReqReplies };
                RemainingReplies =< 0 ->
			io:format("Effectively Done~n"),
                        Alerts = maps:get(alerts, State),
                        if
                                not Alerts -> ok;
                                Alerts ->
                                        lager:info("Entering Analysis Phase")
%                                        {ok, Diffs} = diff_analyze(maps:to_list(NewReplies)),
 %                                       case Diffs of
  %                                              [] -> ok;
   %                                             DiffList ->
    %                                                    lager:warning("Found Difference: ~p", [DiffList]),
     %                                                   ok
      %                                  end
                        end,
                        maps:without([ReqRef], Replies)
        end,
        NewState = State#{ replies :=  NewReplies },
	io:format("REP: ~p~n", [#{ state => NewState }]),
	{noreply, NewState};
handle_info({tcp_closed, Socket}, #{ req_socket := Socket, replies := #{} } = State) ->
	io:format("Request Ended~n"),
	{noreply, State};
handle_info({tcp_closed, Socket}, #{ proxy_sockets := Backends } = State) ->
	#{ Socket := Backend } = Backends,
	io:format("Proxy (~p) Ended~n", [Backend]),
	{noreply, State};
handle_info({tcp_closed, Socket}, State) ->
	io:format("Proxy (~p) Ended?~n", [{Socket, State}]),
	{noreply, State};
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

to_lower(String) when is_list(String) -> string:to_lower(String);
to_lower(String) when is_binary(String) ->
        << <<(do_lower(Char)):8>> || <<Char:8>> <= String >>.

do_lower(Char) when Char >= 65 andalso Char =< 90 -> Char + 32;
do_lower(Char) -> Char.


