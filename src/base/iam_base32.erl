%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc RFC 4648, Section 6: https://tools.ietf.org/html/rfc4648#section-6
%%%
%%% @end
%%% Created :  11 May 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_base32).

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
-define(LC_B32_TO_INT(C),
	case C of
		$a -> 16#00;
		$b -> 16#01;
		$c -> 16#02;
		$d -> 16#03;
		$e -> 16#04;
		$f -> 16#05;
		$g -> 16#06;
		$h -> 16#07;
		$i -> 16#08;
		$j -> 16#09;
		$k -> 16#0A;
		$l -> 16#0B;
		$m -> 16#0C;
		$n -> 16#0D;
		$o -> 16#0E;
		$p -> 16#0F;
		$q -> 16#10;
		$r -> 16#11;
		$s -> 16#12;
		$t -> 16#13;
		$u -> 16#14;
		$v -> 16#15;
		$w -> 16#16;
		$x -> 16#17;
		$y -> 16#18;
		$z -> 16#19;
		$2 -> 16#1A;
		$3 -> 16#1B;
		$4 -> 16#1C;
		$5 -> 16#1D;
		$6 -> 16#1E;
		$7 -> 16#1F
	end).

-define(MC_B32_TO_INT(C),
	case C of
		$a -> 16#00;
		$b -> 16#01;
		$c -> 16#02;
		$d -> 16#03;
		$e -> 16#04;
		$f -> 16#05;
		$g -> 16#06;
		$h -> 16#07;
		$i -> 16#08;
		$j -> 16#09;
		$k -> 16#0A;
		$l -> 16#0B;
		$m -> 16#0C;
		$n -> 16#0D;
		$o -> 16#0E;
		$p -> 16#0F;
		$q -> 16#10;
		$r -> 16#11;
		$s -> 16#12;
		$t -> 16#13;
		$u -> 16#14;
		$v -> 16#15;
		$w -> 16#16;
		$x -> 16#17;
		$y -> 16#18;
		$z -> 16#19;
		$A -> 16#00;
		$B -> 16#01;
		$C -> 16#02;
		$D -> 16#03;
		$E -> 16#04;
		$F -> 16#05;
		$G -> 16#06;
		$H -> 16#07;
		$I -> 16#08;
		$J -> 16#09;
		$K -> 16#0A;
		$L -> 16#0B;
		$M -> 16#0C;
		$N -> 16#0D;
		$O -> 16#0E;
		$P -> 16#0F;
		$Q -> 16#10;
		$R -> 16#11;
		$S -> 16#12;
		$T -> 16#13;
		$U -> 16#14;
		$V -> 16#15;
		$W -> 16#16;
		$X -> 16#17;
		$Y -> 16#18;
		$Z -> 16#19;
		$2 -> 16#1A;
		$3 -> 16#1B;
		$4 -> 16#1C;
		$5 -> 16#1D;
		$6 -> 16#1E;
		$7 -> 16#1F
	end).

-define(UC_B32_TO_INT(C),
	case C of
		$A -> 16#00;
		$B -> 16#01;
		$C -> 16#02;
		$D -> 16#03;
		$E -> 16#04;
		$F -> 16#05;
		$G -> 16#06;
		$H -> 16#07;
		$I -> 16#08;
		$J -> 16#09;
		$K -> 16#0A;
		$L -> 16#0B;
		$M -> 16#0C;
		$N -> 16#0D;
		$O -> 16#0E;
		$P -> 16#0F;
		$Q -> 16#10;
		$R -> 16#11;
		$S -> 16#12;
		$T -> 16#13;
		$U -> 16#14;
		$V -> 16#15;
		$W -> 16#16;
		$X -> 16#17;
		$Y -> 16#18;
		$Z -> 16#19;
		$2 -> 16#1A;
		$3 -> 16#1B;
		$4 -> 16#1C;
		$5 -> 16#1D;
		$6 -> 16#1E;
		$7 -> 16#1F
	end).

-define(LC_INT_TO_B32(C),
	case C of
		16#00 -> $a;
		16#01 -> $b;
		16#02 -> $c;
		16#03 -> $d;
		16#04 -> $e;
		16#05 -> $f;
		16#06 -> $g;
		16#07 -> $h;
		16#08 -> $i;
		16#09 -> $j;
		16#0A -> $k;
		16#0B -> $l;
		16#0C -> $m;
		16#0D -> $n;
		16#0E -> $o;
		16#0F -> $p;
		16#10 -> $q;
		16#11 -> $r;
		16#12 -> $s;
		16#13 -> $t;
		16#14 -> $u;
		16#15 -> $v;
		16#16 -> $w;
		16#17 -> $x;
		16#18 -> $y;
		16#19 -> $z;
		16#1A -> $2;
		16#1B -> $3;
		16#1C -> $4;
		16#1D -> $5;
		16#1E -> $6;
		16#1F -> $7
	end).

-define(UC_INT_TO_B32(C),
	case C of
		16#00 -> $A;
		16#01 -> $B;
		16#02 -> $C;
		16#03 -> $D;
		16#04 -> $E;
		16#05 -> $F;
		16#06 -> $G;
		16#07 -> $H;
		16#08 -> $I;
		16#09 -> $J;
		16#0A -> $K;
		16#0B -> $L;
		16#0C -> $M;
		16#0D -> $N;
		16#0E -> $O;
		16#0F -> $P;
		16#10 -> $Q;
		16#11 -> $R;
		16#12 -> $S;
		16#13 -> $T;
		16#14 -> $U;
		16#15 -> $V;
		16#16 -> $W;
		16#17 -> $X;
		16#18 -> $Y;
		16#19 -> $Z;
		16#1A -> $2;
		16#1B -> $3;
		16#1C -> $4;
		16#1D -> $5;
		16#1E -> $6;
		16#1F -> $7
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
			_ when (Padding == false orelse Padding == nil) andalso Size < 8 ->
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
				H = << << (?LC_B32_TO_INT(V)):5 >> || << V >> <= Head0 >>,
				T =
					case Tail1 of
						<<>> ->
							<<>>;
						_ when is_binary(Tail1) ->
							<< << (?LC_B32_TO_INT(V)):5 >> || << V >> <= Tail1 >>;
						{Tail2, Last0, BitShift, Bits} ->
							Tail3 = << << (?LC_B32_TO_INT(V)):5 >> || << V >> <= Tail2 >>,
							Last = (?LC_B32_TO_INT(Last0)) bsr BitShift,
							<< Tail3/bitstring, Last:Bits >>
					end,
				{H, T};
			'mixed' ->
				H = << << (?MC_B32_TO_INT(V)):5 >> || << V >> <= Head0 >>,
				T =
					case Tail1 of
						<<>> ->
							<<>>;
						_ when is_binary(Tail1) ->
							<< << (?MC_B32_TO_INT(V)):5 >> || << V >> <= Tail1 >>;
						{Tail2, Last0, BitShift, Bits} ->
							Tail3 = << << (?MC_B32_TO_INT(V)):5 >> || << V >> <= Tail2 >>,
							Last = (?MC_B32_TO_INT(Last0)) bsr BitShift,
							<< Tail3/bitstring, Last:Bits >>
					end,
				{H, T};
			'upper' ->
				H = << << (?UC_B32_TO_INT(V)):5 >> || << V >> <= Head0 >>,
				T =
					case Tail1 of
						<<>> ->
							<<>>;
						_ when is_binary(Tail1) ->
							<< << (?UC_B32_TO_INT(V)):5 >> || << V >> <= Tail1 >>;
						{Tail2, Last0, BitShift, Bits} ->
							Tail3 = << << (?UC_B32_TO_INT(V)):5 >> || << V >> <= Tail2 >>,
							Last = (?UC_B32_TO_INT(Last0)) bsr BitShift,
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
			H = << << (?LC_INT_TO_B32(V)) >> || << V:5 >> <= Head >>,
			{T, Pad} =
				case Tail of
					<< V:1 >> -> {<< (?LC_INT_TO_B32(V bsl 4)) >>, 4};
					<< V:2 >> -> {<< (?LC_INT_TO_B32(V bsl 3)) >>, 1};
					<< V:3 >> -> {<< (?LC_INT_TO_B32(V bsl 2)) >>, 6};
					<< V:4 >> -> {<< (?LC_INT_TO_B32(V bsl 1)) >>, 3};
					<<>> -> {<<>>, 0}
				end,
			case Padding of
				true ->
					<< H/binary, T/binary, (binary:copy(<< $= >>, Pad))/binary >>;
				false ->
					<< H/binary, T/binary >>
			end;
		'upper' when is_boolean(Padding) ->
			H = << << (?UC_INT_TO_B32(V)) >> || << V:5 >> <= Head >>,
			{T, Pad} =
				case Tail of
					<< V:1 >> -> {<< (?UC_INT_TO_B32(V bsl 4)) >>, 4};
					<< V:2 >> -> {<< (?UC_INT_TO_B32(V bsl 3)) >>, 1};
					<< V:3 >> -> {<< (?UC_INT_TO_B32(V bsl 2)) >>, 6};
					<< V:4 >> -> {<< (?UC_INT_TO_B32(V bsl 1)) >>, 3};
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
