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
	create_route_json/2,
	list_route_html/2,
	create_route_form/2
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
		{<<"application/json">>, list_route_json},
		{<<"text/html">>, list_route_html}
	],
	{Providers, Req, State}.

content_types_accepted(Req, State) ->
	Types = [
		{{<<"application">>, <<"json">>, []}, create_route_json},
		{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_route_form}
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

list_route_html(Req, Domain) ->
	{Route, Alert} = dnsplice:get_domain_route(Domain),
	RouteIP = proplists:get_value(Route, application:get_env(dnsplice, backends, [])),
	TemplateArgs = #{
		domain => Domain,
		alerts => Alert,
		route  => Route,
		ip     => list_to_binary(RouteIP),
		backends => maps:to_list(dnsplice:get_backends())
	},
	{ok, Page} = dnsplice_route_dtl:render(TemplateArgs),
	{Page, Req, Domain}.

create_route_json(Req, State) ->
	HasBody = cowboy_req:has_body(Req),
	if
		not HasBody -> {false, Req, State};
		HasBody ->
			{ok, Input, Req2} = cowboy_req:read_body(Req),
			Args = maps:from_list([{binary_to_existing_atom(Field, utf8), Value} || {Field, Value} <- jsx:decode(Input)]),
			ok = dnsplice:set_domain_route(State, Args),
			{true, Req2, State}
	end.

create_route_form(Req, State) ->
	{ok, Input, Req2} = cowboy_req:read_urlencoded_body(Req),
	Args = maps:from_list([{binary_to_existing_atom(Field, utf8), Value} || {Field, Value} <- Input]),
	ok = dnsplice:set_domain_route(State, Args#{ alerts => maps:is_key(alerts, Args) }),
	{true, Req2, State}.
