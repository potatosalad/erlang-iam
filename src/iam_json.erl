%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  21 Apr 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_json).

%% API
-export([decode/1]).
-export([decode/2]).
-export([encode/1]).
-export([encode/2]).
-export([try_decode/1]).
-export([try_decode/2]).
-export([try_encode/1]).
-export([try_encode/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

decode(Input) ->
	decode(Input, #{}).

decode(Input, Options) ->
	ojson:'decode!'(Input, Options).

encode(Input) ->
	encode(Input, #{}).

encode(Input, Options) ->
	ojson:'encode!'(Input, Options).

try_decode(Input) ->
	try_decode(Input, #{}).

try_decode(Input, Options) ->
	ojson:decode(Input, Options).

try_encode(Input) ->
	try_encode(Input, #{}).

try_encode(Input, Options) ->
	ojson:encode(Input, Options).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
