%%%-------------------------------------------------------------------
%% @doc db_access public API
%% @end
%%%-------------------------------------------------------------------

-module(db_access_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
		%{"/", cowboy_static, {priv_file, db_access, "static/index.html"}},
	        {"/", toppage_h, []},
		{"/gfriends",get_friends_h,[]},
		{"/pfriends",set_friends_h,[]},
		{"/afriend",add_friend_h,[]}

	    ]}
	]),

	PrivDir = code:priv_dir(db_access),
	%tls stands for transport layer security
        {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),
	db_access_sup:start_link().
stop(_State) ->
    ok.

%% internal functions
