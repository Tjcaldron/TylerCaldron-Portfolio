%% @doc A handler to deal with retrieval of the list of friends
%% for a specific individual.
-module(get_friends_h).

-export([init/2]).

init(Req0, Opts) ->
	{ok,Data,_} = cowboy_req:read_body(Req0),
	%it is expected that the data consists of one quoted-string name
	%in an array.
	[Name|_] = jsx:decode(Data),
	Result=case get_friends_server:get_friends_of(Name) of
			{error,notfound} -> "no such person";
			Friends -> Friends
		end,
	        
	Encoded_message = jsx:encode(Result),
	%io:format("~p~n",[get_friends_server:get_friends_of(Name)]),
	Response = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, Encoded_message, Req0),
	{ok, Response, Opts}.
