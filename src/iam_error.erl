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
-module(iam_error).

%% Types
-type t(DataType) :: #{
	'__struct__' := ?MODULE,
	'__exception__' := true,
	app := atom(),
	key := atom(),
	data := DataType,
	message := binary()
}.

-export_type([t/1]).

-type t() :: t(term()).

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
-export([exception/1]).
-export([message/1]).
%% API
-export([new/2]).
-export([new/3]).
-export([new/4]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		'__exception__' => true,
		app => nil,
		key => nil,
		data => nil,
		message => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:merge('__struct__'(), Map).

exception(Attributes) ->
	'__struct__'(Attributes).

message(#{ '__struct__' := ?MODULE, message := Message }) ->
	Message.

%%%===================================================================
%%% API functions
%%%===================================================================

new(App, Key) when is_atom(App) andalso is_atom(Key) ->
	new(App, Key, nil, erlang:atom_to_binary(Key, unicode)).

new(App, Key, Data) when is_atom(App) andalso is_atom(Key) ->
	new(App, Key, Data, erlang:atom_to_binary(Key, unicode)).

new(App, Key, Data, Message) when is_atom(App) andalso is_atom(Key) andalso is_binary(Message) ->
	#{
		'__struct__' => ?MODULE,
		'__exception__' => true,
		app => App,
		key => Key,
		data => Data,
		message => Message
	}.
