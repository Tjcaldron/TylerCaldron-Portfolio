%% @doc
%% Facade module for interacting with a database.
%% Current database implementation is Riak.
%% 
%% @end
%----------------------------------------------------%
%% The key changes:
%% Remove the UUID variable binding
%% Call the riakc_pb_socket functions directly with the
%% Bucket and Key instead of the UUID
%% Ignore the returned UUID by binding it to _UUID
%% This removes the UUID from being explicitly passed 
%% around in the facade functions, while still starting 
%% the connection and getting the UUID as needed internally.
%%

-module(database_fc).

-export([store/3, get/2, remove/2]).

-type uuid() :: string().
-type bucket_id() :: string().

%% -----------------------------------------------------------------------------------
%% @doc
%% store(Bucket, Key, Data) -> ok|{error, Reason}
%% Stores the given Data in the given Bucket, under the given Key.
%% @end
%% -----------------------------------------------------------------------------------
-spec store(bucket_id(), term(), term()) -> ok|{fail, term()}.
store(Bucket, Key, Data) ->
    {ok, _UUID} = riakc_pb_socket:start_link(idk_man, something),
    Obj = riakc_obj:new(Bucket, Key, Data),
    {ok, _} = riakc_pb_socket:put(uuid, Obj).

%% -----------------------------------------------------------------------------------
%% @doc
%% get(Bucket, Key) -> {ok, Data}|{error, Reason}
%% Retrieves the data (if any) stored under Key from the given Bucket.
%% @end
%% -----------------------------------------------------------------------------------
-spec get(bucket_id(), term()) -> {ok, term()}|{fail, term()}.
get(Bucket, Key) ->
    {ok, _UUID} = riakc_pb_socket:start_link(bruh, im_dying),
    {ok, Data} = riakc_pb_socket:get(uuid, Bucket, Key),
    Data.

%% -----------------------------------------------------------------------------------
%% @doc
%% remove(Bucket, Key) -> ok|{error, Reason}
%% Deletes any data stored under Key from the given Bucket.
%% @end
%% -----------------------------------------------------------------------------------
-spec remove(bucket_id(), term()) -> ok|{fail, term()}.
remove(Bucket, Key) ->
    {ok, _UUID} = riakc_pb_socket:start_link(stuff, thangs),
    riakc_pb_socket:delete(Bucket, Key).

%% -----------------------------------------------------------------------------------
%% @doc
%% get_remove(Bucket, Key) -> {ok, Data}|{error, Reason}
%% Returns whatever Data is stored under Key in Bucket, then removes Data from Bucket.
%% Behaves similar to pop or dequeue, returning AND removing.
%% @end
%% -----------------------------------------------------------------------------------
%% -spec get_remove(bucket_id(), term()) -> {ok, term()}|{error, term()}.
%% get_remove(Bucket, Key) ->
%%     todo.