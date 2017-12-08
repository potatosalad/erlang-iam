%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  22 Apr 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(IAM_ERROR_HRL).

-define(iam_raise(Class, Error),
	erlang:raise(Class, Error, [hd(element(2, erlang:process_info(erlang:self(), current_stacktrace))) | erlang:get_stacktrace()])).
-define(iam_raise(Class, App, Key),
	?iam_raise(Class, iam_error:new(App, Key))).
-define(iam_raise(Class, App, Key, Data),
	?iam_raise(Class, iam_error:new(App, Key, Data))).
-define(iam_raise(Class, App, Key, Data, Message),
	?iam_raise(Class, iam_error:new(App, Key, Data, Message))).

-define(iam_throw(App, Key),
	erlang:throw(iam_error:new(App, Key))).
-define(iam_throw(App, Key, Data),
	erlang:throw(iam_error:new(App, Key, Data))).
-define(iam_throw(App, Key, Data, Message),
	erlang:throw(iam_error:new(App, Key, Data, Message))).

-define(IAM_ERROR_HRL, 1).

-endif.
