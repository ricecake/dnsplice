-module(dnsplice_web_route).

-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([list_route_json/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, list_route_json}			
], Req, State}.

is_authorized(Req, State) ->
	case cowboy_req:parse_header(<<"authorization">>, Req) of
		{basic, User = <<"Alladin">>, <<"open sesame">>} ->
			{true, Req, User};
		_ ->
			{{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
	end.
