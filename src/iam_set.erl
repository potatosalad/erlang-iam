%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  28 Apr 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_set).

%% Types
-type t(Type) :: #{
	'__struct__' := ?MODULE,
	Type => nil()
}.

-export_type([t/1]).

-type t() :: t(term()).

-export_type([t/0]).

%% API
-export([from_string/2]).
-export([to_string/2]).
%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
-export([new/0]).
-export([new/1]).
-export([new/2]).
-export([delete/2]).
-export([difference/2]).
-export(['disjoint?'/2]).
-export(['equal?'/2]).
-export([intersection/2]).
-export(['member?'/2]).
-export([put/2]).
-export([size/1]).
-export(['subset?'/2]).
-export([to_list/1]).
-export([union/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec from_string(binary(), char()) -> t(binary()).
from_string(String, Separator) ->
	new(binary:split(String, << Separator >>, [global, trim_all])).

-spec to_string(t(binary()), char()) -> binary().
to_string(Set=#{ '__struct__' := ?MODULE }, Separator) ->
	iam_util:list_join(to_list(Set), Separator).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:merge('__struct__'(), Map).

-spec new() -> t().
new() ->
	'__struct__'().

-spec new(term()) -> t().
new(Set=#{ '__struct__' := ?MODULE }) ->
	Set;
new(Enumerable) ->
	_ = code:ensure_loaded('Elixir.Enum'),
	List =
		case erlang:function_exported('Elixir.Enum', to_list, 1) of
			true ->
				'Elixir.Enum':to_list(Enumerable);
			false when is_list(Enumerable) ->
				Enumerable;
			false when is_map(Enumerable) ->
				case Enumerable of
					#{ '__struct__' := 'Elixir.MapSet', map := MapSet } ->
						maps:keys(MapSet);
					#{ '__struct__' := 'Elixir.MapSet', data := MapSet } ->
						maps:keys(MapSet);
					_ ->
						maps:keys(Enumerable)
				end;
			false ->
				erlang:error({badarg, [Enumerable]})
		end,
	Map = maps:from_list([{Item, []} || Item <- List]),
	'__struct__'(Map).

-spec new(term(), function()) -> t().
new(Enumerable, Transform) when is_function(Transform, 1) ->
	List =
		case erlang:function_exported('Elixir.Enum', to_list, 1) of
			true ->
				'Elixir.Enum':to_list(Enumerable);
			false when is_list(Enumerable) ->
				Enumerable;
			false when is_map(Enumerable) ->
				case Enumerable of
					#{ '__struct__' := 'Elixir.MapSet', map := MapSet } ->
						maps:keys(MapSet);
					#{ '__struct__' := 'Elixir.MapSet', data := MapSet } ->
						maps:keys(MapSet);
					_ ->
						maps:keys(Enumerable)
				end;
			false ->
				erlang:error({badarg, [Enumerable, Transform]})
		end,
	Map = maps:from_list([{Transform(Item), []} || Item <- List]),
	'__struct__'(Map).

-spec delete(t(T), T) -> t(T) when T :: any().
delete(Set0=#{ '__struct__' := ?MODULE }, Value) ->
	Set = maps:remove(Value, Set0),
	Set.

-spec difference(t(T), t(T)) -> t(T) when T :: any().
difference(SetA=#{ '__struct__' := ?MODULE }, SetB=#{ '__struct__' := ?MODULE }) ->
	Set =
		case (map_size(SetA) - 1) < ((map_size(SetB) - 1) * 2) of
			true ->
				filter_not_in(maps:keys(maps:remove('__struct__', SetA)), maps:remove('__struct__', SetB), []);
			false ->
				maps:without(maps:keys(maps:remove('__struct__', SetB)), maps:remove('__struct__', SetA))
		end,
	'__struct__'(Set).

%% @private
filter_not_in([], _Map, Acc) ->
	maps:from_list(Acc);
filter_not_in([Key | Rest], Map, Acc0) ->
	Acc =
		case maps:is_key(Key, Map) of
			true ->
				Acc0;
			false ->
				[{Key, []} | Acc0]
		end,
	filter_not_in(Rest, Map, Acc).

-spec 'disjoint?'(t(), t()) -> boolean().
'disjoint?'(SetA=#{ '__struct__' := ?MODULE }, SetB=#{ '__struct__' := ?MODULE }) ->
	{Set1, Set2} = order_by_size(maps:remove('__struct__', SetA), maps:remove('__struct__', SetB)),
	'none_in?'(maps:keys(Set1), Set2).

%% @private
'none_in?'([], _) ->
	true;
'none_in?'([Key | Rest], Map2) ->
	case maps:is_key(Key, Map2) of
		true ->
			false;
		false ->
			'none_in?'(Rest, Map2)
	end.

-spec 'equal?'(t(), t()) -> boolean().
'equal?'(SetA=#{ '__struct__' := ?MODULE }, SetB=#{ '__struct__' := ?MODULE }) ->
	SetA == SetB.

-spec intersection(t(T), t(T)) -> t(T) when T :: any().
intersection(SetA=#{ '__struct__' := ?MODULE }, SetB=#{ '__struct__' := ?MODULE }) ->
	{Set1, Set2} = order_by_size(maps:remove('__struct__', SetA), maps:remove('__struct__', SetB)),
	Set = maps:with(maps:keys(Set1), Set2),
	'__struct__'(Set).

-spec 'member?'(t(T), T) -> boolean() when T :: any().
'member?'(Set=#{ '__struct__' := ?MODULE }, Value) ->
	maps:is_key(Value, Set).

-spec put(t(T), T) -> t(T) when T :: any().
put(Set0=#{ '__struct__' := ?MODULE }, Value) ->
	maps:put(Value, [], Set0).

-spec size(t()) -> non_neg_integer().
size(Set=#{ '__struct__' := ?MODULE }) ->
	map_size(Set) - 1.

-spec 'subset?'(t(), t()) -> boolean().
'subset?'(SetA=#{ '__struct__' := ?MODULE }, SetB=#{ '__struct__' := ?MODULE }) ->
	case ?MODULE:size(SetA) =< ?MODULE:size(SetB) of
		true ->
			'do_subset?'(maps:keys(maps:remove('__struct__', SetA)), SetB);
		false ->
			false
	end.

%% @private
'do_subset?'([], _) ->
	true;
'do_subset?'([Key | Rest], Set2) ->
	case maps:is_key(Key, Set2) of
		true ->
			'do_subset?'(Rest, Set2);
		false ->
			false
	end.

-spec to_list(t(T)) -> [T] when T :: any().
to_list(Set=#{ '__struct__' := ?MODULE }) ->
	lists:sort(maps:keys(maps:remove('__struct__', Set))).

-spec union(t(Val1), t(Val2)) -> t(Val1 | Val2) when Val1 :: term(), Val2 :: term().
union(SetA=#{ '__struct__' := ?MODULE }, SetB=#{ '__struct__' := ?MODULE }) ->
	maps:merge(SetA, SetB).

%% @private
order_by_size(Set1, Set2) when map_size(Set1) > map_size(Set2) ->
	{Set2, Set1};
order_by_size(Set1, Set2) ->
	{Set1, Set2}.
