-module(dnsplice_web_route).

-export([
	init/2,
	content_types_provided/2,
	is_authorized/2,
	allowed_methods/2,
	content_types_accepted/2,
	resource_exists/2
]).

-export([
	list_route_json/2,
	create_route_json/2
]).


init(Req, Opts) -> {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
	case cowboy_req:parse_header(<<"authorization">>, Req) of
		{basic, User, Pass} ->
			ValidUsers = application:get_env(dnsplice_web, users, #{}),
			case maps:find(User, ValidUsers) of
				{ok, Pass} -> {true, Req, State#{ user => User }};
				_ -> {{false, <<"Basic realm=\"dnsplice\"">>}, Req, State}
			end;
		_ ->
			{{false, <<"Basic realm=\"dnsplice\"">>}, Req, State}
	end.

allowed_methods(Req, State) ->
	Methods = [<<"GET">>, <<"POST">>],
	{Methods, Req, State}.

content_types_provided(Req, State) ->
	Providers = [
		{<<"application/json">>, list_route_json}
	],
	{Providers, Req, State}.

content_types_accepted(Req, State) ->
	Types = [
		{{<<"application">>, <<"json">>, []}, create_route_json}
	],
	{Types,	Req, State}.

resource_exists(Req, _State) ->
	case cowboy_req:binding(route, Req) of
		undefined ->
			{true, Req, index};
		Route ->{true, Req, Route}
	end.

list_route_json(Req, index) ->
	Details = jsx:encode(#{
		explicit_routes => mnesia:table_info(route, size)
	}),
	{Details, Req, index};
list_route_json(Req, Domain) ->
	{Route, Alert} = dnsplice:get_domain_route(Domain),
	RouteIP = proplists:get_value(Route, application:get_env(dnsplice, backends, [])),
	Result = jsx:encode(#{
		domain => Domain,
		alerts => Alert,
		route  => Route,
		ip     => list_to_binary(RouteIP)
	}),
	{Result, Req, Domain}.

create_route_json(Req, State) -> {true, Req, State}.
