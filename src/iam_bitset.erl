%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  21 Jul 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_bitset).

%% Types
-type t(Type) :: #{
	'__struct__' := ?MODULE,
	'__legend__' := [Type],
	Type => nil()
}.

-export_type([t/1]).

-type t() :: t(term()).

-export_type([t/0]).

% %% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
-export([new/0]).
-export([new/1]).
-export([new/2]).
-export([decode/2]).
-export([encode/1]).
-export([delete/2]).
-export([difference/2]).
-export(['disjoint?'/2]).
-export(['equal?'/2]).
-export([intersection/2]).
-export(['member?'/2]).
-export([put/2]).
-export([put_enum/2]).
-export([size/1]).
-export(['subset?'/2]).
-export([to_list/1]).
-export([union/2]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		'__legend__' => []
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:merge('__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec new() -> t().
new() ->
	'__struct__'().

-spec new(term()) -> t().
new(BitSet=#{ '__struct__' := ?MODULE }) ->
	BitSet;
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
	ok = check_for_duplicates(List, #{}),
	'__struct__'(#{ '__legend__' => List }).

-spec new(term(), term()) -> t().
new(Legend, Enumerable) ->
	BitSet = new(Legend),
	put_enum(BitSet, Enumerable).

encode(BitSet=#{ '__struct__' := ?MODULE, '__legend__' := Legend }) ->
	Integer = encode(Legend, BitSet, 0, 0),
	binary:encode_unsigned(Integer, big).

%% @private
encode([Key | Keys], BitSet, Index, Acc) ->
	case BitSet of
		#{ Key := [] } ->
			case Keys of
				[] ->
					Acc bor (1 bsl Index);
				_ ->
					encode(Keys, BitSet, Index + 1, Acc bor (1 bsl Index))
			end;
		_ ->
			case Keys of
				[] ->
					Acc;
				_ ->
					encode(Keys, BitSet, Index + 1, Acc)
			end
	end;
encode([], _, _, Acc) ->
	Acc.

decode(BitSet=#{ '__struct__' := ?MODULE, '__legend__' := Legend }, Encoded) ->
	Integer =
		case Encoded of
			_ when is_integer(Encoded) ->
				Encoded;
			_ when is_binary(Encoded) ->
				binary:decode_unsigned(Encoded, big)
		end,
	decode(Legend, BitSet, 0, Integer).

decode([Key | Keys], BitSet, Index, Acc) ->
	case Acc band (1 bsl Index) of
		0 ->
			decode(Keys, BitSet, Index + 1, Acc);
		_ ->
			decode(Keys, ?MODULE:put(BitSet, Key), Index + 1, Acc)
	end;
decode([], BitSet, _, _) ->
	BitSet.

-spec delete(t(T), T) -> t(T) when T :: any().
delete(BitSet0=#{ '__struct__' := ?MODULE }, Value) ->
	BitSet = maps:remove(Value, BitSet0),
	BitSet.

-spec difference(t(T), t(T)) -> t(T) when T :: any().
difference(BitSetA=#{ '__struct__' := ?MODULE, '__legend__' := Legend }, BitSetB=#{ '__struct__' := ?MODULE, '__legend__' := Legend }) ->
	Set =
		case (map_size(BitSetA) - 1) < ((map_size(BitSetB) - 1) * 2) of
			true ->
				filter_not_in(to_list(BitSetA), maps:remove('__legend__', maps:remove('__struct__', BitSetB)), []);
			false ->
				maps:without(to_list(BitSetB), maps:remove('__legend__', maps:remove('__struct__', BitSetA)))
		end,
	new(Legend, maps:keys(Set)).

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
'disjoint?'(BitSetA=#{ '__struct__' := ?MODULE }, BitSetB=#{ '__struct__' := ?MODULE }) ->
	{BitSet1, BitSet2} = order_by_size(maps:remove('__legend__', maps:remove('__struct__', BitSetA)), maps:remove('__legend__', maps:remove('__struct__', BitSetB))),
	'none_in?'(maps:keys(BitSet1), BitSet2).

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
'equal?'(BitSetA=#{ '__struct__' := ?MODULE }, BitSetB=#{ '__struct__' := ?MODULE }) ->
	BitSetA == BitSetB.

-spec intersection(t(T), t(T)) -> t(T) when T :: any().
intersection(BitSetA=#{ '__struct__' := ?MODULE, '__legend__' := Legend }, BitSetB=#{ '__struct__' := ?MODULE, '__legend__' := Legend }) ->
	{BitSet1, BitSet2} = order_by_size(maps:remove('__legend__', maps:remove('__struct__', BitSetA)), maps:remove('__legend__', maps:remove('__struct__', BitSetB))),
	Set = maps:with(maps:keys(BitSet1), BitSet2),
	new(Legend, maps:keys(Set)).

-spec 'member?'(t(T), T) -> boolean() when T :: any().
'member?'(BitSet=#{ '__struct__' := ?MODULE }, Value) ->
	maps:is_key(Value, BitSet).

-spec put(t(T), T) -> t(T) when T :: any().
put(BitSet=#{ '__struct__' := ?MODULE, '__legend__' := Legend }, Value) ->
	case lists:member(Value, Legend) of
		true ->
			maps:put(Value, [], BitSet);
		false ->
			erlang:error({badkey, Value})
	end.

put_enum(BitSet=#{ '__struct__' := ?MODULE }, Enumerable) ->
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
					#{ '__struct__' := ?MODULE } ->
						to_list(Enumerable);
					_ ->
						maps:keys(Enumerable)
				end;
			false ->
				erlang:error({badarg, [Enumerable]})
		end,
	do_put_enum(List, BitSet).

%% @private
do_put_enum([Key | Keys], BitSet) ->
	do_put_enum(Keys, ?MODULE:put(BitSet, Key));
do_put_enum([], BitSet) ->
	BitSet.

-spec size(t()) -> non_neg_integer().
size(Set=#{ '__struct__' := ?MODULE }) ->
	map_size(Set) - 2.

-spec 'subset?'(t(), t()) -> boolean().
'subset?'(BitSetA=#{ '__struct__' := ?MODULE }, BitSetB=#{ '__struct__' := ?MODULE }) ->
	case ?MODULE:size(BitSetA) =< ?MODULE:size(BitSetB) of
		true ->
			'do_subset?'(to_list(BitSetA), BitSetB);
		false ->
			false
	end.

%% @private
'do_subset?'([], _) ->
	true;
'do_subset?'([Key | Rest], BitSet2) ->
	case maps:is_key(Key, BitSet2) of
		true ->
			'do_subset?'(Rest, BitSet2);
		false ->
			false
	end.

-spec to_list(t(T)) -> [T] when T :: any().
to_list(BitSet=#{ '__struct__' := ?MODULE }) ->
	lists:sort(maps:keys(maps:remove('__legend__', maps:remove('__struct__', BitSet)))).

-spec union(t(Val1), t(Val2)) -> t(Val1 | Val2) when Val1 :: term(), Val2 :: term().
union(BitSetA=#{ '__struct__' := ?MODULE }, BitSetB=#{ '__struct__' := ?MODULE }) ->
	maps:merge(BitSetA, BitSetB).

%% @private
order_by_size(BitSet1, BitSet2) when map_size(BitSet1) > map_size(BitSet2) ->
	{BitSet2, BitSet1};
order_by_size(BitSet1, BitSet2) ->
	{BitSet1, BitSet2}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
check_for_duplicates([Key | Keys], Acc) ->
	case Acc of
		#{ Key := [] } ->
			erlang:error({duplicate_key, Key});
		_ ->
			check_for_duplicates(Keys, maps:put(Key, [], Acc))
	end;
check_for_duplicates([], _) ->
	ok.
