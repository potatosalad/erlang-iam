%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  09 May 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_app).
-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).
% -export([config_change/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_Type, _Args) ->
	iam_sup:start_link().

stop(_State) ->
	ok.

% config_change(_Changed, _New, _Removed) ->
% 	iam_server:config_change().
