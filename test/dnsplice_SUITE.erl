-module(dnsplice_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-export([
         all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2,
         %% group: boot
         test_dnsplice_boot/1
        ]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
          {group, boot}
         ].

groups() -> [
		{boot, [], [
			test_dnsplice_boot
             	]}
	].


init_per_suite(Config) ->
	stop_system(),
	reset_configs(),
	start_system(),
	timer:sleep(1000),
	io:format("~p~n", [Config]),
	Config.

end_per_suite(_Config) ->
	stop_system(),
	ok.

init_per_testcase(_Suite, Config) ->
	Config.

end_per_testcase(_Suite, _Config) ->
	ok.

%%%===================================================================
%%% TESTS
%%%===================================================================

%%--------------------------------------------------------------------
%% Group : boot
%%--------------------------------------------------------------------

test_dnsplice_boot(_Config) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_system() ->
	{ok, _} = dnsplice:start(),
	{ok, _} = dnsplice_web:start(),
	ok.

stop_system() ->
	_ = dnsplice:stop(),
	_ = dnsplice_web:stop(),
	ok.

reset_configs() ->
	application:set_env(dnsplice, listen_port, 15353),
	application:set_env(dnsplice_web, port, 18080),
	ok.
