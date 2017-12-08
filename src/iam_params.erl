%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  14 Jun 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_params).
-behaviour(iam_engine).

-include("iam_error.hrl").

%% Types
-type json() :: #{ binary() => json() }
	| [json()] | []
	| true | false | nil
	| integer() | float()
	| binary().

-export_type([json/0]).

-type params() :: #{ binary() => json() }.

-export_type([params/0]).

-type transform() ::
	fun((t(), atom(), read | write) -> t() | {ok, any(), t()}).

-export_type([transform/0]).

-type type() :: any | array | boolean | integer | list | object | set | string | url.

-export_type([type/0]).

-type schema() :: #{
	atom() => type() | transform()
}.

-export_type([schema/0]).

-type t() :: #{
	'__engine__' := iam_engine:t(),
	'__params__' := params(),
	'__schema__' := schema(),
	'__struct__' := module()
}.

-export_type([t/0]).

%% Callbacks
-callback '__schema__'(Key) -> Type when Key :: atom(), Type :: type() | transform().
-callback new() -> Env when Env :: t().
-callback new(Enumerable) -> Env when Enumerable :: list() | map(), Env :: t().

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([fetch/2]).
-export([type/2]).
%% iam_params callbacks
-export(['__schema__'/1]).
-export([new/0]).
-export([new/1]).
%% iam_engine callbacks
-export([init/1]).
-export([handle_event/4]).
%% Import/Export API
-export([encode_form_data/1]).
-export([encode_form_data/2]).
-export([encode_query/1]).
-export([encode_query/2]).
-export([from_json/1]).
-export([from_json/2]).
-export([from_query/1]).
-export([from_query/2]).
-export([to_form_data/1]).
-export([to_form_data/2]).
-export([to_json/1]).
-export([to_json/2]).
-export([to_query/1]).
-export([to_query/2]).
%% Read API
-export([read/1]).
-export([read/2]).
-export([try_read/1]).
-export([try_read/2]).
-export([read_any/2]).
-export([read_array/2]).
-export([read_boolean/2]).
-export([read_integer/2]).
-export([read_list/2]).
-export([read_object/2]).
-export([read_set/2]).
-export([read_string/2]).
-export([read_url/2]).
%% Write API
-export([write/1]).
-export([write/2]).
-export([write_any/2]).
-export([write_array/2]).
-export([write_boolean/2]).
-export([write_integer/2]).
-export([write_list/2]).
-export([write_object/2]).
-export([write_set/2]).
-export([write_string/2]).
-export([write_url/2]).

%% Macros
-define(iam_params,
	#{ '__engine__' := _, '__params__' := _, '__schema__' := _, '__struct__' := _ }).
-define(is_badkey(K),
	K == '__engine__' orelse K == '__params__' orelse K == '__schema__' orelse K == '__struct__').

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	new(#{}).

'__struct__'(Enumerable) ->
	new(Enumerable).

%%%===================================================================
%%% API functions
%%%===================================================================

fetch(Env0=?iam_params, Key) when is_atom(Key) ->
	Env1 = try_read(Env0, Key),
	case maps:find(Key, Env1) of
		{ok, nil} ->
			error;
		{ok, Value} ->
			{ok, Value, Env1};
		error ->
			error
	end.

type(Env=#{ '__schema__' := Schema }, Key) ->
	case maps:find(Key, Schema) of
		{ok, Type} ->
			Type;
		error ->
			iam_engine:apply(Env, '__schema__', [Key], fun type_predicate/1, '__schema__'(Key))
	end.

%% @private
type_predicate(any) -> false;
type_predicate(_) -> true.

%%%===================================================================
%%% iam_params callbacks
%%%===================================================================

'__schema__'(_) ->
	any.

new() ->
	new(#{}).

new(BaseMap) when is_map(BaseMap) ->
	iam_engine:new(?MODULE, BaseMap);
new(List) when is_list(List) ->
	new(maps:from_list(List)).

%%%===================================================================
%%% iam_engine callbacks
%%%===================================================================

init(BaseMap) ->
	Data = maps:merge(#{
		'__struct__' => ?MODULE,
		'__engine__' => nil,
		'__params__' => #{},
		'__schema__' => #{}
	}, BaseMap),
	{ok, ?MODULE, Data}.

handle_event(cast, {read, Key}, _State, Env) ->
	{keep_state, do_read(Env, Key)};
handle_event(cast, {write, Key}, _State, Env) ->
	{keep_state, do_write(Env, Key)};
handle_event(_Type, _Content, _State, _Env) ->
	keep_state_and_data.

%%%===================================================================
%%% Import/Export API functions
%%%===================================================================

encode_form_data(Env=?iam_params) ->
	Keys = maps:keys(Env),
	encode_form_data(Env, Keys).

encode_form_data(Env0=?iam_params, Keys) when is_list(Keys) ->
	Env = write(Env0, Keys),
	{_, FormData} = lists:foldl(fun encode_form_data_folder/2, {Env, #{}}, Keys),
	{ok, FormData, Env}.

%% @private
encode_form_data_folder(K, Acc) when ?is_badkey(K) ->
	Acc;
encode_form_data_folder(Key, {Env=#{ '__params__' := Params }, Acc}) ->
	Type =
		case type(Env, Key) of
			Atom when is_atom(Atom) ->
				Atom;
			_ ->
				any
		end,
	Param = erlang:atom_to_binary(Key, unicode),
	case maps:find(Param, Params) of
		{ok, nil} ->
			{Env, Acc};
		{ok, V} when (Type == any orelse Type == boolean) andalso is_boolean(V) ->
			Value = erlang:atom_to_binary(V, unicode),
			{Env, maps:put(Param, Value, Acc)};
		{ok, V} when (Type == array) andalso is_list(V) ->
			Value = iam_json:encode(V),
			{Env, maps:put(Param, Value, Acc)};
		{ok, V} when (Type == list orelse Type == set) andalso is_list(V) ->
			Value = iam_util:list_join(V, $\s),
			{Env, maps:put(Param, Value, Acc)};
		{ok, V} when (Type == any) andalso is_list(V) ->
			Value =
				case lists:all(fun erlang:is_binary/1, V) of
					true ->
						iam_util:list_join(V, $\s);
					false ->
						iam_json:encode(V)
				end,
			{Env, maps:put(Param, Value, Acc)};
		{ok, V} when (Type == any orelse Type == integer) andalso is_integer(V) ->
			Value = erlang:integer_to_binary(V),
			{Env, maps:put(Param, Value, Acc)};
		{ok, V} when (Type == any orelse Type == object) andalso is_map(V) ->
			Value = iam_json:encode(V),
			{Env, maps:put(Param, Value, Acc)};
		{ok, V} when (Type == any orelse Type == string orelse Type == url) andalso is_binary(V) ->
			Value = V,
			{Env, maps:put(Param, Value, Acc)};
		error ->
			{Env, Acc}
	end.

encode_query(Env=?iam_params) ->
	Keys = maps:keys(Env),
	encode_query(Env, Keys).

encode_query(Env0=?iam_params, Keys) when is_list(Keys) ->
	{ok, FormData, Env} = encode_form_data(Env0, Keys),
	Query = iam_uri:encode_query(FormData),
	{ok, Query, Env}.

from_json(Params) ->
	from_json(?MODULE, Params).

from_json(Struct, Params) when is_map(Params) ->
	try_read(Struct:new(#{ '__params__' => Params }));
from_json(Struct, JSON) when is_binary(JSON) ->
	Params = iam_json:decode(JSON),
	from_json(Struct, Params).

from_query(Params) ->
	from_query(?MODULE, Params).

from_query(Struct, Params) when is_map(Params) ->
	try_read(Struct:new(#{ '__params__' => Params }));
from_query(Struct, Query) when is_binary(Query) ->
	Params = iam_uri:decode_query(Query),
	from_query(Struct, Params).

to_form_data(Env=?iam_params) ->
	{ok, FormData, _} = encode_form_data(Env),
	FormData.

to_form_data(Env=?iam_params, Keys) when is_list(Keys) ->
	{ok, FormData, _} = encode_form_data(Env, Keys),
	FormData.

to_json(Env=?iam_params) ->
	#{ '__params__' := Params } = write(Env),
	Params.

to_json(Env=?iam_params, Keys) when is_list(Keys) ->
	#{ '__params__' := Params } = write(Env, Keys),
	Params.

to_query(Env=?iam_params) ->
	{ok, Query, _} = encode_query(Env),
	Query.

to_query(Env=?iam_params, Keys) when is_list(Keys) ->
	{ok, Query, _} = encode_query(Env, Keys),
	Query.

%%%===================================================================
%%% Read API functions
%%%===================================================================

read(Env=?iam_params) ->
	Keys = maps:keys(Env),
	read(Env, Keys).

read(Env=?iam_params, Key) when is_atom(Key) ->
	iam_engine:cast(Env, {read, Key});
read(Env=?iam_params, Keys) when is_list(Keys) ->
	lists:foldl(fun read_folder/2, Env, Keys).

%% @private
read_folder(Key, Env) ->
	read(Env, Key).

try_read(Env=?iam_params) ->
	Keys = maps:keys(Env),
	try_read(Env, Keys).

try_read(Env=?iam_params, Key) when is_atom(Key) ->
	try
		read(Env, Key)
	catch
		throw:Exception=#{ '__struct__' := iam_error } ->
			_ = Exception,
			Env
	end;
try_read(Env=?iam_params, Keys) when is_list(Keys) ->
	lists:foldl(fun try_read_folder/2, Env, Keys).

%% @private
try_read_folder(Key, Env) ->
	try_read(Env, Key).

read_any(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_boolean(V) orelse is_integer(V) orelse is_list(V) orelse is_map(V) ->
			Value = V,
			maps:update(Key, Value, Env);
		{ok, V = << C, _/binary >>} when (byte_size(V) == 4 andalso (C == $t orelse C == $T)) orelse (byte_size(V) == 5 andalso (C == $f orelse C == $F)) ->
			Value =
				case cast_boolean(V) of
					nil ->
						V;
					Boolean ->
						Boolean
				end,
			maps:update(Key, Value, Env);
		{ok, V = << C, _/binary >>} when C >= $0 andalso C =< $9 ->
			Value =
				try erlang:binary_to_integer(V) of
					Integer when is_integer(Integer) andalso Integer >= 0 ->
						Integer;
					_ ->
						V
				catch
					_:_ ->
						V
				end,
			maps:update(Key, Value, Env);
		{ok, V} when is_binary(V) ->
			Value = V,
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_array(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_binary(V) ->
			try iam_json:decode(V) of
				Value when is_list(Value) ->
					maps:update(Key, Value, Env);
				_ ->
					?iam_throw(iam, invalid_params, Env)
			catch
				_:_ ->
					?iam_raise(throw, iam, invalid_params, Env)
			end;
		{ok, V} when is_list(V) ->
			Value = V,
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_boolean(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_binary(V) orelse is_boolean(V) ->
			Value =
				case cast_boolean(V) of
					nil ->
						?iam_throw(iam, invalid_params, Env);
					Boolean ->
						Boolean
				end,
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_integer(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_integer(V) andalso V >= 0 ->
			Value = V,
			maps:update(Key, Value, Env);
		{ok, V} when is_binary(V) ->
			Value =
				try erlang:binary_to_integer(V) of
					Integer when is_integer(Integer) andalso Integer >= 0 ->
						Integer;
					_ ->
						?iam_throw(iam, invalid_params, Env)
				catch
					_:_ ->
						?iam_raise(throw, iam, invalid_params, Env)
				end,
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_list(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_binary(V) ->
			case iam_json:try_decode(V) of
				{ok, Value} when is_list(Value) ->
					maps:update(Key, Value, Env);
				error ->
					Value = binary:split(V, << $\s >>, [global, trim_all]),
					maps:update(Key, Value, Env);
				_ ->
					?iam_throw(iam, invalid_params, Env)
			end;
		{ok, V} when is_list(V) ->
			Value = V,
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_object(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_binary(V) ->
			try iam_json:decode(V) of
				Value when is_map(Value) ->
					maps:update(Key, Value, Env);
				_ ->
					?iam_throw(iam, invalid_params, Env)
			catch
				_:_ ->
					?iam_raise(throw, iam, invalid_params, Env)
			end;
		{ok, V} when is_map(V) ->
			Value = V,
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_set(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_binary(V) ->
			case iam_json:try_decode(V) of
				{ok, L} when is_list(L) ->
					Value = iam_set:new(L),
					maps:update(Key, Value, Env);
				_ ->
					Value = iam_set:from_string(V, $\s),
					maps:update(Key, Value, Env)
			end;
		{ok, V} when is_list(V) ->
			Value = iam_set:new(V),
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_string(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, Value} when is_binary(Value) ->
			maps:update(Key, Value, Env);
		_ ->
			?iam_throw(iam, invalid_params, Env)
	end.

read_url(Env=?iam_params, Key) when is_atom(Key) ->
	case pull(Env, Key) of
		{true, _} ->
			Env;
		{ok, V} when is_binary(V) ->
			Value = V,
			maps:update(Key, Value, Env);
		error ->
			?iam_throw(iam, invalid_params, Env)
	end.

%%%===================================================================
%%% Write API functions
%%%===================================================================

write(Env=?iam_params) ->
	Keys = maps:keys(Env),
	write(Env, Keys).

write(Env=?iam_params, Key) when is_atom(Key) ->
	iam_engine:cast(Env, {write, Key});
write(Env=?iam_params, Keys) when is_list(Keys) ->
	lists:foldl(fun write_folder/2, Env, Keys).

%% @private
write_folder(Key, Env) ->
	write(Env, Key).

write_any(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, Boolean} when is_boolean(Boolean) ->
			push(Env, Key, Boolean);
		{ok, Integer} when is_integer(Integer) ->
			push(Env, Key, Integer);
		{ok, List} when is_list(List) ->
			push(Env, Key, List);
		{ok, Set=#{ '__struct__' := iam_set }} ->
			push(Env, Key, lists:sort(iam_set:to_list(Set)));
		{ok, URI=#{ '__struct__' := 'Elixir.URI' }} ->
			push(Env, Key, iam_uri:to_string(URI));
		{ok, Map} when is_map(Map) ->
			push(Env, Key, Map);
		{ok, Binary} when is_binary(Binary) ->
			push(Env, Key, Binary);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_array(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, List} when is_list(List) ->
			push(Env, Key, List);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_boolean(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, Boolean} when is_boolean(Boolean) ->
			push(Env, Key, Boolean);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_integer(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, Integer} when is_integer(Integer) ->
			push(Env, Key, Integer);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_list(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, List} when is_list(List) ->
			push(Env, Key, List);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_object(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, Map} when is_map(Map) ->
			push(Env, Key, Map);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_set(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, Set=#{ '__struct__' := iam_set }} ->
			push(Env, Key, lists:sort(iam_set:to_list(Set)));
		{ok, List} when is_list(List) ->
			push(Env, Key, List);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_string(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, Value} when is_binary(Value) ->
			push(Env, Key, Value);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

write_url(Env=?iam_params, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, URI=#{ '__struct__' := 'Elixir.URI' }} ->
			push(Env, Key, iam_uri:to_string(URI));
		{ok, String} when is_binary(String) ->
			push(Env, Key, String);
		{ok, nil} ->
			Env;
		error ->
			erlang:error({badkey, Key})
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
cast_boolean(false) ->
	false;
cast_boolean(<<"false">>) ->
	false;
cast_boolean(<< Binary:5/binary >>) ->
	case iam_util:lowercase(Binary) of
		<<"false">> ->
			false;
		_ ->
			nil
	end;
cast_boolean(true) ->
	true;
cast_boolean(<<"true">>) ->
	true;
cast_boolean(<< Binary:4/binary >>) ->
	case iam_util:lowercase(Binary) of
		<<"true">> ->
			true;
		_ ->
			nil
	end;
cast_boolean(_) ->
	nil.

%% @private
do_read(Env, K) when ?is_badkey(K) ->
	Env;
do_read(Env0, Key) ->
	Type = type(Env0, Key),
	Read =
		case Type of
			any -> fun ?MODULE:read_any/2;
			array -> fun ?MODULE:read_array/2;
			boolean -> fun ?MODULE:read_boolean/2;
			integer -> fun ?MODULE:read_integer/2;
			list -> fun ?MODULE:read_list/2;
			object -> fun ?MODULE:read_object/2;
			set -> fun ?MODULE:read_set/2;
			string -> fun ?MODULE:read_string/2;
			url -> fun ?MODULE:read_url/2;
			Fun when is_function(Fun, 3) -> fun(E, K) -> Fun(E, K, read) end
		end,
	Env1 = Read(Env0, Key),
	Env1.

%% @private
do_write(Env, K) when ?is_badkey(K) ->
	Env;
do_write(Env0, Key) ->
	Type = type(Env0, Key),
	Write =
		case Type of
			any -> fun ?MODULE:write_any/2;
			array -> fun ?MODULE:write_array/2;
			boolean -> fun ?MODULE:write_boolean/2;
			integer -> fun ?MODULE:write_integer/2;
			list -> fun ?MODULE:write_list/2;
			object -> fun ?MODULE:write_object/2;
			set -> fun ?MODULE:write_set/2;
			string -> fun ?MODULE:write_string/2;
			url -> fun ?MODULE:write_url/2;
			Fun when is_function(Fun, 3) -> fun(E, K) -> Fun(E, K, write) end
		end,
	Env1 = Write(Env0, Key),
	Env1.

%% @private
pull(Env=#{ '__params__' := Params }, Key) when is_atom(Key) ->
	case maps:find(Key, Env) of
		{ok, nil} ->
			Param = erlang:atom_to_binary(Key, unicode),
			maps:find(Param, Params);
		{ok, Value} ->
			{true, Value};
		error ->
			erlang:error({badkey, Key})
	end.

%% @private
push(Env=#{ '__params__' := Params }, Key, Value) when is_atom(Key) ->
	Param = erlang:atom_to_binary(Key, unicode),
	NewParams =
		case Params of
			nil ->
				maps:put(Param, Value, maps:new());
			_ when is_map(Params) ->
				maps:put(Param, Value, Params)
		end,
	Env#{ '__params__' := NewParams }.
