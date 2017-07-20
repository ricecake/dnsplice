%%%-------------------------------------------------------------------
%% @doc dnsplice public API
%%
%% Application callbacks required to start the dnsplice app
%%
%% @end
%%%-------------------------------------------------------------------

-module(dnsplice_app).

-behaviour(application).
-include_lib("dnsplice/src/records.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
	start/2,
	stop/1
]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	ok = application:set_env(dnsplice, servers, [
		normalize_backend(Entry) || Entry <- application:get_env(dnsplice, backends, [])
	]),
	ok = setup_mnesia(),
	ok = setup_tcp_listener(),
	dnsplice_sup:start_link().

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

setup_mnesia() ->
	Nodes = application:get_env(dnsplice, peers, [node()]),
	mnesia:change_config(extra_db_nodes, Nodes),
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	case mnesia:system_info(tables) -- [schema] of
		[] ->
			{atomic, ok} = mnesia:create_table(route, [
				{disc_copies, [node()]},
				{attributes, record_info(fields, route)}
			]),
			ok;
		SystemTables ->
			[mnesia:add_table_copy(Table, node(), disc_copies) || Table <- SystemTables],
			ok = mnesia:wait_for_tables([schema |SystemTables], 10000)
	end,
	ok.
setup_tcp_listener() ->
	Port = application:get_env(dnsplice, listen_port, 5300),
	Opts = application:get_env(dnsplice, listen_opts, []),
	{ok, _} = ranch:start_listener(dnsplice_dns, ranch_tcp, [{port, Port} |Opts], dnsplice_worker_tcp, []),
	ok.
