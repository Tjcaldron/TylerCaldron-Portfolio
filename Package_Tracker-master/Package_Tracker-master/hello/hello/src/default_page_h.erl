%% @doc Hello world handler.
-module(default_page_h).

-export([init/2]).

init(Req0, Opts) ->
        % 200 is an html code saying it was success full
        Req = cowboy_req:reply(200, #{
                 <<"content-type">> => <<"text/plain">> % This line turns it into binary
        }, "Howdy y'all!", Req0), %Howdy Howdy Howdy 
        
        {ok, Req, Opts}.