-module(logger).
-behaviour(gen_event).




%% Supervisor Callbacks
-export([init/1,terminate/3,code_change/2]).
%% event Callbacks
-export([handle_event/2,handle_info/2,handle_call/2]).

%%%===================================================================
%%% Mandatory callback functions
%%%===================================================================

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State) ->
    ok.


init(Start_info) ->
    %% This function has a value that is a tuple
    %% consisting of ok and the initial state data.
    {ok,[]}.


%%%===================================================================
%%% Callback functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Used when a non-OTP-standard message is sent to a manager.
%%
%%
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {ok,stateData}. % was {ok,StateData}.

%%--------------------------------------------------------------------
%% @doc
%%
%% Used when a manager is sent a request using gen_event:call/3/4.
%%
%%
%% @end
%%--------------------------------------------------------------------
handle_call(Request,State)->
    Response = [],
    {ok,Response,newState}. % was {ok,Response,NewState}.

%%--------------------------------------------------------------------
%% @doc
%%
%% Used when a manager is sent an event using gen_event:notify/2 or 
%% gen_event:sync_notify/2.
%%
%%
%% @end
%%--------------------------------------------------------------------
handle_event(Message,State) ->
    %Modify the state as appropriate.
    {ok,State}.


%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
%%
%% Unit tests go here. 
%%

handle_info_test_() ->
    [
        ?_assertEqual(handle_info(info, state), {ok,stateData})  % fake test
        
    ].

handle_call_test_() ->
    [
        
    ].

handle_event_test_() ->
    [

    ].
    
-endif.
