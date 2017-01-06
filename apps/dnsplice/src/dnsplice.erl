-module(dnsplice).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
	get_domain_route/1,
	set_domain_route/2
]).

%% ------------------------------------------------------------------
%% Defines and includes
%% ------------------------------------------------------------------
-include_lib("dnsplice/src/records.hrl").


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_domain_route(Domain) ->
	DomainNames = build_subdomains(Domain),
	mnesia:activity(async_dirty, fun
		FindRoute([]) ->
			{ok, DefaultBackend} = application:get_env(dnsplice, default_backend),
			{ok, DefaultReports} = application:get_env(dnsplice, default_reports),
			{DefaultBackend, DefaultReports};
		FindRoute([Next |Rest]) ->
			case mnesia:read(route, Next) of
				[] -> FindRoute(Rest);
				[#route{ backend = Backend, reported = Reports }] -> {Backend, Reports}
			end
	end, [DomainNames]).


set_domain_route(Domain, Fields) when is_map(Fields), is_binary(Domain) ->
	{ok, DefaultBackend} = application:get_env(dnsplice, default_backend),
	{ok, DefaultReports} = application:get_env(dnsplice, default_reports),
	NewRouteEntry = maps:with(record_info(fields, route), maps:merge(#{
		domain   => Domain,
		backend  => DefaultBackend,
		reported => DefaultReports
	}, maps:without([domain], Fields))),
	RouteRecord = #route{
		domain=maps:get(domain, NewRouteEntry),
		backend=maps:get(backend, NewRouteEntry),
		reported=maps:get(reported, NewRouteEntry)
	},
	mnesia:activity(transaction, fun()->
		mnesia:write(RouteRecord)
	end).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

build_subdomains(Domain) when is_list(Domain)-> build_subdomains(list_to_binary(Domain));
build_subdomains(Domain) when is_binary(Domain)->
	Labels = binary:split(Domain, <<".">>, [global]),
	do_subdomain_build(lists:reverse(Labels), []).


do_subdomain_build([], Acc) -> Acc;
do_subdomain_build([Chunk |Rest], []) ->
	do_subdomain_build(Rest, [Chunk]);
do_subdomain_build([Chunk |Rest], [Last |_] = Acc) ->
	do_subdomain_build(Rest, [<<Chunk/binary, $., Last/binary>> |Acc]).

