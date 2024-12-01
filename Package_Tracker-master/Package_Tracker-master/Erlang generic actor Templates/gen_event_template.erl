%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a template for gen_event handlers. Usually, event handlers
%%% and their event manager are initiated by one or more supervisors, 
%%% though event handlers can be initialized directly by their manager.
%%%
%%% Event handlers' methods are intended to be used by their manager.
%%% Therefore, event handlers do not have an API that is used
%%% in other parts of your code.
%%%
%%%
%%% @end

%%% Created : 24 October 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_event_template).
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
    {ok,StateData}.

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
    {ok,Response,NewState}.

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
-endif.
