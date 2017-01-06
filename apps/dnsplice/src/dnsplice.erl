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
-define(record_to_list(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
-define(record_to_map(Rec, Ref), maps:from_list(?record_to_list(Rec, Ref))).


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
	{ok, DefaultAlerts} = application:get_env(dnsplice, default_alerts),
	mnesia:activity(transaction, fun()->
		Default = case mnesia:read(route, Domain) of
			[Route] -> ?record_to_map(route, Route);
			[] -> #{
				domain   => Domain,
				backend  => DefaultBackend,
				alerts   => DefaultAlerts
			}
		end,
		NewRouteEntry = maps:with(record_info(fields, route), maps:merge(Default, maps:without([domain], Fields))),
		RouteRecord = #route{
			domain=maps:get(domain, NewRouteEntry),
			backend=maps:get(backend, NewRouteEntry),
			alerts=maps:get(alerts, NewRouteEntry)
		},
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

