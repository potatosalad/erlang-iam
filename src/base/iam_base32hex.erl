%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc RFC 4648, Section 7: https://tools.ietf.org/html/rfc4648#section-7
%%%
%%% @end
%%% Created :  11 May 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_base32hex).

-include("iam_base.hrl").

%% API
-export([decode/1]).
-export([decode/2]).
-export(['decode!'/1]).
-export(['decode!'/2]).
-export([encode/1]).
-export([encode/2]).
-export([random/1]).
-export([random/2]).

% Macros
-define(LC_B32HEX_TO_INT(C),
	case C of
		$0 -> 16#00;
		$1 -> 16#01;
		$2 -> 16#02;
		$3 -> 16#03;
		$4 -> 16#04;
		$5 -> 16#05;
		$6 -> 16#06;
		$7 -> 16#07;
		$8 -> 16#08;
		$9 -> 16#09;
		$a -> 16#0A;
		$b -> 16#0B;
		$c -> 16#0C;
		$d -> 16#0D;
		$e -> 16#0E;
		$f -> 16#0F;
		$g -> 16#10;
		$h -> 16#11;
		$i -> 16#12;
		$j -> 16#13;
		$k -> 16#14;
		$l -> 16#15;
		$m -> 16#16;
		$n -> 16#17;
		$o -> 16#18;
		$p -> 16#19;
		$q -> 16#1A;
		$r -> 16#1B;
		$s -> 16#1C;
		$t -> 16#1D;
		$u -> 16#1E;
		$v -> 16#1F
	end).

-define(MC_B32HEX_TO_INT(C),
	case C of
		$0 -> 16#00;
		$1 -> 16#01;
		$2 -> 16#02;
		$3 -> 16#03;
		$4 -> 16#04;
		$5 -> 16#05;
		$6 -> 16#06;
		$7 -> 16#07;
		$8 -> 16#08;
		$9 -> 16#09;
		$a -> 16#0A;
		$b -> 16#0B;
		$c -> 16#0C;
		$d -> 16#0D;
		$e -> 16#0E;
		$f -> 16#0F;
		$g -> 16#10;
		$h -> 16#11;
		$i -> 16#12;
		$j -> 16#13;
		$k -> 16#14;
		$l -> 16#15;
		$m -> 16#16;
		$n -> 16#17;
		$o -> 16#18;
		$p -> 16#19;
		$q -> 16#1A;
		$r -> 16#1B;
		$s -> 16#1C;
		$t -> 16#1D;
		$u -> 16#1E;
		$v -> 16#1F;
		$A -> 16#0A;
		$B -> 16#0B;
		$C -> 16#0C;
		$D -> 16#0D;
		$E -> 16#0E;
		$F -> 16#0F;
		$G -> 16#10;
		$H -> 16#11;
		$I -> 16#12;
		$J -> 16#13;
		$K -> 16#14;
		$L -> 16#15;
		$M -> 16#16;
		$N -> 16#17;
		$O -> 16#18;
		$P -> 16#19;
		$Q -> 16#1A;
		$R -> 16#1B;
		$S -> 16#1C;
		$T -> 16#1D;
		$U -> 16#1E;
		$V -> 16#1F
	end).

-define(UC_B32HEX_TO_INT(C),
	case C of
		$0 -> 16#00;
		$1 -> 16#01;
		$2 -> 16#02;
		$3 -> 16#03;
		$4 -> 16#04;
		$5 -> 16#05;
		$6 -> 16#06;
		$7 -> 16#07;
		$8 -> 16#08;
		$9 -> 16#09;
		$A -> 16#0A;
		$B -> 16#0B;
		$C -> 16#0C;
		$D -> 16#0D;
		$E -> 16#0E;
		$F -> 16#0F;
		$G -> 16#10;
		$H -> 16#11;
		$I -> 16#12;
		$J -> 16#13;
		$K -> 16#14;
		$L -> 16#15;
		$M -> 16#16;
		$N -> 16#17;
		$O -> 16#18;
		$P -> 16#19;
		$Q -> 16#1A;
		$R -> 16#1B;
		$S -> 16#1C;
		$T -> 16#1D;
		$U -> 16#1E;
		$V -> 16#1F
	end).

-define(LC_INT_TO_B32HEX(C),
	case C of
		16#00 -> $0;
		16#01 -> $1;
		16#02 -> $2;
		16#03 -> $3;
		16#04 -> $4;
		16#05 -> $5;
		16#06 -> $6;
		16#07 -> $7;
		16#08 -> $8;
		16#09 -> $9;
		16#0A -> $a;
		16#0B -> $b;
		16#0C -> $c;
		16#0D -> $d;
		16#0E -> $e;
		16#0F -> $f;
		16#10 -> $g;
		16#11 -> $h;
		16#12 -> $i;
		16#13 -> $j;
		16#14 -> $k;
		16#15 -> $l;
		16#16 -> $m;
		16#17 -> $n;
		16#18 -> $o;
		16#19 -> $p;
		16#1A -> $q;
		16#1B -> $r;
		16#1C -> $s;
		16#1D -> $t;
		16#1E -> $u;
		16#1F -> $v
	end).

-define(UC_INT_TO_B32HEX(C),
	case C of
		16#00 -> $0;
		16#01 -> $1;
		16#02 -> $2;
		16#03 -> $3;
		16#04 -> $4;
		16#05 -> $5;
		16#06 -> $6;
		16#07 -> $7;
		16#08 -> $8;
		16#09 -> $9;
		16#0A -> $A;
		16#0B -> $B;
		16#0C -> $C;
		16#0D -> $D;
		16#0E -> $E;
		16#0F -> $F;
		16#10 -> $G;
		16#11 -> $H;
		16#12 -> $I;
		16#13 -> $J;
		16#14 -> $K;
		16#15 -> $L;
		16#16 -> $M;
		16#17 -> $N;
		16#18 -> $O;
		16#19 -> $P;
		16#1A -> $Q;
		16#1B -> $R;
		16#1C -> $S;
		16#1D -> $T;
		16#1E -> $U;
		16#1F -> $V
	end).

%%%===================================================================
%%% API functions
%%%===================================================================

decode(Input) when ?is_iodata(Input) ->
	decode(Input, #{}).

decode(Input, Opts) when ?is_iodata(Input) andalso is_map(Opts) ->
	try 'decode!'(Input, Opts) of
		Output when is_binary(Output) ->
			{ok, Output}
	catch
		_:_ ->
			error
	end;
decode(Input, Opts) when ?is_iodata(Input) andalso is_list(Opts) ->
	decode(Input, maps:from_list(Opts)).

'decode!'(Input) when ?is_iodata(Input) ->
	'decode!'(Input, #{}).

'decode!'([], #{}) ->
	<<>>;
'decode!'(<<>>, #{}) ->
	<<>>;
'decode!'(Input, Opts) when ?is_iodata(Input) andalso is_map(Opts) ->
	Case = maps:get('case', Opts, 'mixed'),
	Padding = maps:get('padding', Opts, nil),
	Size = erlang:iolist_size(Input),
	Offset =
		case Padding of
			_ when (Padding == false orelse Padding == nil) andalso Size =< 8 ->
				0;
			_ when (Padding == false orelse Padding == nil) andalso (Size rem 8) =:= 0 ->
				Size - 8;
			_ when (Padding == false orelse Padding == nil) andalso (Size rem 8) =/= 0 ->
				Size - (Size rem 8);
			_ when (Padding == true orelse Padding == nil) andalso Size >= 8 ->
				Size - 8;
			_ ->
				erlang:error({badarg, [Input, Opts]})
		end,
	<< Head0:Offset/binary, Tail0/binary >> = ?to_binary(Input),
	Tail1 =
		case Padding of
			false ->
				case Tail0 of
					<< T0:1/binary, T1:8 >> ->
						{T0, T1, 2, 3};
					<< T0:3/binary, T1:8 >> ->
						{T0, T1, 4, 1};
					<< T0:4/binary, T1:8 >> ->
						{T0, T1, 1, 4};
					<< T0:6/binary, T1:8 >> ->
						{T0, T1, 3, 2};
					<<>> ->
						<<>>;
					_ ->
						erlang:error({badarg, [Input, Opts]})
				end;
			nil ->
				case Tail0 of
					<< T0:1/binary, T1:8, $=, $=, $=, $=, $=, $= >> ->
						{T0, T1, 2, 3};
					<< T0:3/binary, T1:8, $=, $=, $=, $= >> ->
						{T0, T1, 4, 1};
					<< T0:4/binary, T1:8, $=, $=, $= >> ->
						{T0, T1, 1, 4};
					<< T0:6/binary, T1:8, $= >> ->
						{T0, T1, 3, 2};
					<< T0:1/binary, T1:8 >> ->
						{T0, T1, 2, 3};
					<< T0:3/binary, T1:8 >> ->
						{T0, T1, 4, 1};
					<< T0:4/binary, T1:8 >> ->
						{T0, T1, 1, 4};
					<< T0:6/binary, T1:8 >> ->
						{T0, T1, 3, 2};
					<< T0:8/binary >> ->
						T0;
					<<>> ->
						<<>>
				end;
			true ->
				case Tail0 of
					<< T0:1/binary, T1:8, $=, $=, $=, $=, $=, $= >> ->
						{T0, T1, 2, 3};
					<< T0:3/binary, T1:8, $=, $=, $=, $= >> ->
						{T0, T1, 4, 1};
					<< T0:4/binary, T1:8, $=, $=, $= >> ->
						{T0, T1, 1, 4};
					<< T0:6/binary, T1:8, $= >> ->
						{T0, T1, 3, 2};
					_ ->
						erlang:error({badarg, [Input, Opts]})
				end
		end,
	{Head, Tail} =
		case Case of
			'lower' ->
				H = << << (?LC_B32HEX_TO_INT(V)):5 >> || << V >> <= Head0 >>,
				T =
					case Tail1 of
						<<>> ->
							<<>>;
						_ when is_binary(Tail1) ->
							<< << (?LC_B32HEX_TO_INT(V)):5 >> || << V >> <= Tail1 >>;
						{Tail2, Last0, BitShift, Bits} ->
							Tail3 = << << (?LC_B32HEX_TO_INT(V)):5 >> || << V >> <= Tail2 >>,
							Last = (?LC_B32HEX_TO_INT(Last0)) bsr BitShift,
							<< Tail3/bitstring, Last:Bits >>
					end,
				{H, T};
			'mixed' ->
				H = << << (?MC_B32HEX_TO_INT(V)):5 >> || << V >> <= Head0 >>,
				T =
					case Tail1 of
						<<>> ->
							<<>>;
						_ when is_binary(Tail1) ->
							<< << (?MC_B32HEX_TO_INT(V)):5 >> || << V >> <= Tail1 >>;
						{Tail2, Last0, BitShift, Bits} ->
							Tail3 = << << (?MC_B32HEX_TO_INT(V)):5 >> || << V >> <= Tail2 >>,
							Last = (?MC_B32HEX_TO_INT(Last0)) bsr BitShift,
							<< Tail3/bitstring, Last:Bits >>
					end,
				{H, T};
			'upper' ->
				H = << << (?UC_B32HEX_TO_INT(V)):5 >> || << V >> <= Head0 >>,
				T =
					case Tail1 of
						<<>> ->
							<<>>;
						_ when is_binary(Tail1) ->
							<< << (?UC_B32HEX_TO_INT(V)):5 >> || << V >> <= Tail1 >>;
						{Tail2, Last0, BitShift, Bits} ->
							Tail3 = << << (?UC_B32HEX_TO_INT(V)):5 >> || << V >> <= Tail2 >>,
							Last = (?UC_B32HEX_TO_INT(Last0)) bsr BitShift,
							<< Tail3/bitstring, Last:Bits >>
					end,
				{H, T};
			_ ->
				erlang:error({badarg, [Input, Opts]})
		end,
	<< Head/bitstring, Tail/bitstring >>;
'decode!'(Input, Opts) when ?is_iodata(Input) andalso is_list(Opts) ->
	'decode!'(Input, maps:from_list(Opts)).

encode(Input) when ?is_iodata(Input) ->
	encode(Input, #{}).

encode(Input, Opts) when ?is_iodata(Input) andalso is_map(Opts) ->
	Case = maps:get('case', Opts, 'upper'),
	Padding = maps:get('padding', Opts, true),
	Offset = 5 * ((erlang:iolist_size(Input) * 8) div 5),
	<< Head:Offset/bitstring, Tail/bitstring >> = ?to_binary(Input),
	case Case of
		'lower' when is_boolean(Padding) ->
			H = << << (?LC_INT_TO_B32HEX(V)) >> || << V:5 >> <= Head >>,
			{T, Pad} =
				case Tail of
					<< V:1 >> -> {<< (?LC_INT_TO_B32HEX(V bsl 4)) >>, 4};
					<< V:2 >> -> {<< (?LC_INT_TO_B32HEX(V bsl 3)) >>, 1};
					<< V:3 >> -> {<< (?LC_INT_TO_B32HEX(V bsl 2)) >>, 6};
					<< V:4 >> -> {<< (?LC_INT_TO_B32HEX(V bsl 1)) >>, 3};
					<<>> -> {<<>>, 0}
				end,
			case Padding of
				true ->
					<< H/binary, T/binary, (binary:copy(<< $= >>, Pad))/binary >>;
				false ->
					<< H/binary, T/binary >>
			end;
		'upper' when is_boolean(Padding) ->
			H = << << (?UC_INT_TO_B32HEX(V)) >> || << V:5 >> <= Head >>,
			{T, Pad} =
				case Tail of
					<< V:1 >> -> {<< (?UC_INT_TO_B32HEX(V bsl 4)) >>, 4};
					<< V:2 >> -> {<< (?UC_INT_TO_B32HEX(V bsl 3)) >>, 1};
					<< V:3 >> -> {<< (?UC_INT_TO_B32HEX(V bsl 2)) >>, 6};
					<< V:4 >> -> {<< (?UC_INT_TO_B32HEX(V bsl 1)) >>, 3};
					<<>> -> {<<>>, 0}
				end,
			case Padding of
				true ->
					<< H/binary, T/binary, (binary:copy(<< $= >>, Pad))/binary >>;
				false ->
					<< H/binary, T/binary >>
			end;
		_ ->
			erlang:error({badarg, [Input, Opts]})
	end;
encode(Input, Opts) when ?is_iodata(Input) andalso is_list(Opts) ->
	encode(Input, maps:from_list(Opts)).

random(Bytes) when is_integer(Bytes) andalso Bytes >= 0 ->
	random(Bytes, #{}).

random(0, Opts) when is_map(Opts) ->
	<<>>;
random(Bytes, Opts) when (Bytes =:= 1) andalso is_map(Opts) ->
	erlang:error({badarg, [Bytes, Opts]});
random(Bytes, Opts) when is_integer(Bytes) andalso Bytes > 0 andalso is_map(Opts) ->
	Padding = maps:get('padding', Opts, true),
	R = (Bytes rem 8),
	Size =
		case Padding of
			true when R =:= 0 ->
				(Bytes * 5) div 8;
			false when R =:= 0 orelse R =:= 2 orelse R =:= 4 orelse R =:= 5 orelse R =:= 7 ->
				(Bytes * 5) div 8;
			_ ->
				erlang:error({badarg, [Bytes, Opts]})
		end,
	Binary = crypto:strong_rand_bytes(Size),
	encode(Binary, Opts);
random(Bytes, Opts) when is_integer(Bytes) andalso Bytes >= 0 andalso is_list(Opts) ->
	random(Bytes, maps:from_list(Opts)).
