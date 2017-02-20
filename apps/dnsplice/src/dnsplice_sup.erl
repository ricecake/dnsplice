%%%-------------------------------------------------------------------
%% @doc dnsplice top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dnsplice_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, {#{ intensity => 5, period => 5 }, [
		?CHILD(dnsplice_worker_sup, supervisor),
		?CHILD(dnsplice_listener, worker)
	]}}.

%%====================================================================
%% Internal functions
%%====================================================================
