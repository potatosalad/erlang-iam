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
-module(iam).

%% API
%% Private API
-export([start/0]).

-define(MAYBE_START_IAM(F), try
	F
catch
	_:_ ->
		_ = iam:start(),
		F
end).

%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% Private API functions
%%====================================================================

%% @private
start() ->
	case application:ensure_all_started(?MODULE) of
		{ok, _} ->
			ok;
		StartError ->
			StartError
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
