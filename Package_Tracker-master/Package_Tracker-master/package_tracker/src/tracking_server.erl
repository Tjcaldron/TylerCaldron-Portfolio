%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(tracking_server).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/0,start/3,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% transfer(Location_id, Package_id)
%% Updates the associated Location_id of the given Package_id.
%% 
%% 
%% delivered(Package_id)
%% Moves the given Package_id to delivered_b, recording there the package_id, 
%% last known Location_id, and geolocation of delivery, if possible.
%% 
%% 
%% request(Package_id)
%% Returns the Geoloctaton(aka {Lat, Long}) for a given Package_id
%% 
%% 
%% update(Location_id, Geolocation)
%% Updates the Geolocation(aka {Lat,Long}) for a  given Location_id
%%
%%
%% @end
%%--------------------------------------------------------------------
-type geolocation() :: {float(), float()}|nil.
-type uuid() :: string().

-spec transfer(uuid(), uuid()) -> ok|fail.
transfer(Location_id, Package_id) ->
    database_fc:store("package_b", Package_id, Location_id).

-spec delivered(uuid()) -> ok|fail.
delivered(Package_id) ->
    % Key 
    % Location_b -> location bucket
    % Database_fc -> Database Facade
    % Geoloaction -> lat and long of package location will return current time
    
    {ok, Location_id} = database_fc:get("package_b", Package_id),
    %store(package_b, package_id, location_id) -> result: geolocation
    {ok, Geolocation} = database_fc:get("location_b", Location_id),
    % localtime returns {{Year,Month,Day},{Hour,Min,Sec}}
    Current_time = erlang:localtime(),
    % store (delivered_b, package_b {location_id, geolocation, [timestamp]} 
    % delete (package_b, package_id)
    database_fc:store("delivered_b", Package_id, {Location_id, Geolocation, Current_time}),
    % case statement if result of the database facade says ok and fail term if it fails. 
    % thought bubble for error handling if returns fail at some point n near future
    database_fc:remove("package_b", Package_id).

-spec update(uuid(), geolocation()) -> ok.
update(Location_id, Geolocation) ->
    % store(location,location_id,{lat,long})
    database_fc:store("location_b", Location_id, Geolocation).


-spec request(uuid()) -> {ok, geolocation()}|{fail, nil}.

request(Package_id) ->
    % Get package location_id
    Package_b_result = database_fc:get("package_b", Package_id),
    % if it does not work try connecting it to the d
    % Case statement to check one location id is in the package_id: bucket 
    % and if its not in the package bucket we need to check the delivered_bucket
    case Package_b_result of   
    % Package_b_result - The result of looking up the package id in the package_b bucket
        not_found ->
            % Not found in package bucket, check delivered bucket
             %   Can be:
                %    not_found - Lookup failed
                %   {ok, Package_location_id} - Lookup succeeded, contains location id 

            Delivered_b_result = database_fc:get("delivered_b", Package_id),
            % Delivered_b_result - The result of looking up the package id in the delivered_b bucket 
            case Delivered_b_result of
                not_found ->  
                    %  not_found - Lookup failed
                    {fail, nil};
                {ok, Delivered_location_id} ->
                    %  {ok, Delivered_location_id} - Lookup succeeded, contains location id
                    Geolocation = database_fc:get("location_b", Delivered_location_id),
                    Geolocation
            end;
        {ok, Package_location_id} ->
            % Package_location_id - The location id from the package_b lookup
            Geolocation = database_fc:get("location_b", Package_location_id),
            % Delivered_location_id - The location id from the delivered_b lookup 
            % Geolocation - The latitude and longitude looked up 
            % from the location_b bucket using a location id
            Geolocation
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
        {ok,replace_up}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
%% Transfer
handle_call({transfer, Package_id, Location_id}, _From, State) ->
    {reply, transfer(Package_id, Location_id), State};
%% Delivered
handle_call({delivered, Package_id}, _From, State) ->
    {reply, delivered(Package_id), State};
%% Update
handle_call({update, Location_id, Geolocation}, _From, State) ->
    {noreply, update(Location_id, Geolocation), State};
%% Request
handle_call({request, Package_id}, _From, State) ->
    
    {reply, request(Package_id), State};

handle_call(stop, _From, _State) ->
    {stop, normal, replace_stopped, down}; %% setting the server's internal state to down

handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    % this should log the reason and the state at shutdown.
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%
% All nasty thoughts exist in fail tests

% handle_call test assuming database is working
% happy path
handle_call_test_() ->
    {setup,
    fun() -> 
        meck:new(database_fc, [non_strict]),
        meck:expect(database_fc, store, fun(Bucket, Key, Data) -> ok end),
        meck:expect(database_fc, get, fun(Bucket, Key) -> {ok, data} end),
        meck:expect(database_fc, remove, fun(Bucket, Key) -> ok end)
    end,
    fun(_) -> meck:unload(database_fc) end,
    [
        ?_assertEqual({reply, ok, state}, handle_call({transfer, package_id, location_id}, garbage, state)),
        ?_assertEqual({reply, ok, state}, handle_call({delivered, package_id}, garbage, state)),
        ?_assertEqual({noreply, ok, state}, handle_call({update, location_id, geolocation}, garbage, state)),
        ?_assertEqual({reply, {ok, data}, state}, handle_call({request, package_id}, garbage, state))
    ]}.

% handle_call test assuming database failure
% 'nasty thoughts' live here
handle_call_fail_test_() ->
    {setup,
    fun() ->
        meck:expect(database_fc, store, fun(Bucket, Key, Data) -> fail end),
        meck:expect(database_fc, get, fun(Bucket, Key) -> {fail, nil} end),
        meck:expect(database_fc, remove, fun(Bucket, Key) -> {fail, Key} end)
    end,
    fun(_) -> meck:unload(database_fc) end,
    [
        ?_assertEqual({reply, fail, state}, handle_call({transfer, package_id, location_id}, garbage, state)),
        ?_assertEqual({reply, fail, state}, handle_call({delivered, package_id}, garbage, state)),
        ?_assertEqual({noreply, fail, state}, handle_call({update, location_id, geolocation}, garbage, state)),
        ?_assertEqual({reply, {fail, nil}, state}, handle_call({request, package_id}, garbage, state))
    ]}.

terminate_test_() ->
    [
        % Happy Path
        ?_assertEqual(ok, terminate(test, state)),
        ?_assertEqual({stop, normal, replace_stopped, down}, handle_call(stop, from, state))
    ].

bad_request_test_() ->
    [
        % Nasty thoughts
        ?_assertEqual({reply, bad_request, state}, handle_call({gobbledygook, nonsense}, from, state))
        % No request was sent
        % request not recieved
    ].
-endif.

