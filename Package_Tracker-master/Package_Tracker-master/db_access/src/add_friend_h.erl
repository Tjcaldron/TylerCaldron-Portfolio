%% @doc A handler to deal with retrieval of the list of friends
%% for a specific individual.
-module(add_friend_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	[Name,Friend|_] = jsx:decode(Data),
	Friends = [Friend]
		++get_friends_server:get_friends_of(Name),
	store_friends_server:set_friends_for(Name,Friends),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	},"[\"done\"]", Req0),
	{ok, Req, Opts}.
