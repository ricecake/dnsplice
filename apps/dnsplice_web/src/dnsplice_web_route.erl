-module(dnsplice_web_route).

-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([list_route_json/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, list_route_json}			
], Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, create_route}],
		Req, State}.

resource_exists(Req, _State) ->
	{true, Req, index}.

is_authorized(Req, State) ->
	case cowboy_req:parse_header(<<"authorization">>, Req) of
		{basic, User = <<"Alladin">>, <<"open sesame">>} ->
			{true, Req, User};
		_ ->
			{{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
	end.
