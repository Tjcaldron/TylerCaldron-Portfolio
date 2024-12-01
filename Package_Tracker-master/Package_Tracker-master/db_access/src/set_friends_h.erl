%% @doc A handler to store the friends of some person in the database.
-module(set_friends_h).

-export([init/2]).

init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),
	[Name,Friends|_] = jsx:decode(Data),
	store_friends_server:set_friends_for(Name,Friends),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.
