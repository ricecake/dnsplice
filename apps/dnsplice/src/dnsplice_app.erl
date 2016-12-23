%%%-------------------------------------------------------------------
%% @doc dnsplice public API
%% @end
%%%-------------------------------------------------------------------

-module(dnsplice_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	ok = application:set_env(dnsplice, servers, [
		normalize_backend(Entry) || Entry <- application:get_env(dnsplice, backends, [])
	]),
	dnsplice_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%====================================================================
%% Internal functions
%%====================================================================

normalize_backend({Label, IP}) when is_list(IP) ->
	{ok, IpTuple} = inet_parse:address(IP),
	{Label, IpTuple};
normalize_backend({Label, IP}) when is_tuple(IP) ->
	{Label, IP}.
