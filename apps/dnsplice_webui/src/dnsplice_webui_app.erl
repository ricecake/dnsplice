%%%-------------------------------------------------------------------
%% @doc dnsplice_webui public API
%% @end
%%%-------------------------------------------------------------------

-module(dnsplice_webui_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    dnsplice_webui_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
