%% @doc
%% Facade module for interacting with the logger.
%% Current logger implementation is RabbitMQ.
%% 
%% @end

-module(logger_fc).

-export([log_message/2, log_error/2]).

%% -----------------------------------------------------------------------------------
%% @doc
%% log_message(Transaction_UUID, Message) -> ok
%% Logs the given Message to RabbitMQ
%% @end
%% -----------------------------------------------------------------------------------
log_message(Transaction_UUID, Message) ->
    ok.

%% -----------------------------------------------------------------------------------
%% @doc
%% log_error(Transaction_UUID, Message) -> ok
%% Logs the given Error to RabbitMQ
%% @end
%% -----------------------------------------------------------------------------------
log_error(Transaction_UUID, Error) ->
    ok.