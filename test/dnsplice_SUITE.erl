-module(dnsplice_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-export([
	all/0, groups/0,
	init_per_suite/1, end_per_suite/1,
	init_per_testcase/2, end_per_testcase/2,
	%% group: boot
	test_dnsplice_boot/1,
	%% group: basics
	test_dnsplice_basics/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() -> [
	{group, boot},
	{group, basic}
].

groups() -> [
		{boot, [], [
			test_dnsplice_boot
		]},
		{basic, [], [
			test_dnsplice_basics
		]}
	].


init_per_suite(Config) ->
	stop_system(),
	reset_configs(),
	start_system(),
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

test_dnsplice_basics(_Config) ->
	{ok, Socket} = gen_udp:open(0, [binary]),
	ok = gen_udp:send(Socket, {127,0,0,1}, 15353, produce_dns_request(#{})),
	receive
		Good -> Good
	after 1000 -> ok
	end,
	ok = gen_udp:send(Socket, {127,0,0,1}, 15353, <<"wrong packet!">>),
	receive
		Bad -> Bad
	after 1000 -> ok
	end,
	ok = dnsplice:set_domain_route(<<"test-domain.com">>, #{ alerts => true }),
	ok = gen_udp:send(Socket, {127,0,0,1}, 15353, produce_dns_request(#{ domain => "test-domain.com" })),
	receive
		Routed -> Routed
	after 1000 -> ok
	end,
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
	application:set_env(dnsplice, backends, [
		{<<"A">>, "8.8.8.8"},
		{<<"B">>, "8.8.4.4"}
	]),
	application:set_env(dnsplice, default_backend, <<"A">>),
	application:set_env(dnsplice, default_alerts, true),
	application:set_env(dnsplice_web, port, 18080),
	ok.

produce_dns_request(Opts) when is_map(Opts)->
	Domain = maps:get(domain, Opts, "example.com"),
	Type   = maps:get(type, Opts, a),
	inet_dns:encode(#dns_rec{
		header = #dns_header{
			id = crypto:rand_uniform(1,16#FFFF),
			opcode = query,
			rd = true
		},
		qdlist = [#dns_query{
			domain = Domain,
			type = Type,
			class = in
		}]
	}).
