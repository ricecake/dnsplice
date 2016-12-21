%%%-------------------------------------------------------------------
%% @doc dnsplice worker supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dnsplice_worker_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_worker/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), #{ id => I, type => Type, start => {I, start_link, []} }).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker(Packet, Sender) ->
	supervisor:start_child(?SERVER, [{Packet, Sender}]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, {#{ strategy => simple_one_for_one }, [
		?CHILD(dnsplice_worker, worker)
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
