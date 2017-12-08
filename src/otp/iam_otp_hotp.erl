%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc RFC 4226: https://tools.ietf.org/html/rfc4226
%%%
%%% @end
%%% Created :  09 May 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_otp_hotp).

%% Types
-type counter() :: binary() | non_neg_integer().
-type options() :: #{
	digits => non_neg_integer(),
	hash => atom()
}.

%% API
-export([authenticate/2]).
-export([authenticate/3]).
-export([validate/3]).
-export([validate/4]).

%% Macros
-define(is_counter(C),
	((is_integer(C) andalso C >= 0 andalso C =< 16#ffffffffffffffff)
		orelse (is_binary(C) andalso byte_size(C) == 8))).
-define(mod(B, M),
	((B rem M + M) rem M)).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec authenticate(K :: binary(), C :: counter()) -> binary().
authenticate(K, C) when is_binary(K) andalso ?is_counter(C) ->
	authenticate(K, C, #{}).

-spec authenticate(K :: binary(), C :: counter(), Opts :: options()) -> binary().
authenticate(K, C, Opts) when is_binary(K) andalso ?is_counter(C) andalso is_map(Opts) ->
	Digits = maps:get(digits, Opts, 6),
	Hash = maps:get(hash, Opts, sha),
	Counter = cast_counter(C),
	Digest = crypto:hmac(Hash, K, Counter),
	case Digits of
		0 ->
			binary_to_decimal(Digest);
		_ when Digits >= 4 andalso Digits =< 10 ->
			H = dynamic_truncation(Digest),
			HOTP = ?mod(H, intpow(10, Digits)),
			prepend0(erlang:integer_to_binary(HOTP), Digits);
		_ ->
			erlang:error({badarg, [K, C, Opts]})
	end.

-spec validate(K :: binary(), C :: counter(), HOTP :: binary()) -> boolean().
validate(K, C, HOTP) when is_binary(K) andalso ?is_counter(C) andalso is_binary(HOTP) ->
	validate(K, C, HOTP, #{}).

-spec validate(K :: binary(), C :: counter(), HOTP :: binary(), Opts :: options()) -> boolean().
validate(K, C, HOTP, Opts) when is_binary(K) andalso ?is_counter(C) andalso is_binary(HOTP) andalso is_map(Opts) ->
	Challenge = authenticate(K, C, Opts),
	OTP = cast_otp(HOTP),
	iam_crypto:constant_time_compare(OTP, Challenge).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
binary_to_decimal(B) ->
	Size = decimal_size(B),
	Integer = binary:decode_unsigned(B, big),
	Decimal = erlang:integer_to_binary(Integer),
	prepend0(Decimal, Size).

%% @private
cast_counter(C) when is_integer(C) andalso C >= 0 andalso C =< 16#ffffffffffffffff ->
	<< C:64/unsigned-big-integer-unit:1 >>;
cast_counter(C) when is_binary(C) andalso byte_size(C) == 8 ->
	C.

%% @private
cast_otp(OTP) ->
	<< << C >> || << C >> <= OTP, C =/= $\s andalso C =/= $- >>.

%% @private
decimal_size(B) when bit_size(B) == 160 ->
	49;
decimal_size(B) when bit_size(B) == 224 ->
	68;
decimal_size(B) when bit_size(B) == 256 ->
	78;
decimal_size(B) when bit_size(B) == 384 ->
	116;
decimal_size(B) when bit_size(B) == 512 ->
	155;
decimal_size(B) when is_binary(B) ->
	intlog10(intpow(2, bit_size(B))).

%% @private
dynamic_truncation(Digest) when is_binary(Digest) andalso byte_size(Digest) >= 20 ->
	SkipBits = bit_size(Digest) - 4,
	<< _:SkipBits/bitstring, Offset:4/unsigned-big-integer-unit:1 >> = Digest,
	<< _:Offset/binary, _:1, H:31/unsigned-big-integer-unit:1, _/bitstring >> = Digest,
	H.

%% @private
intlog10(V) when V > 0 ->
	intlog10(V, 0).

%% @private
intlog10(V, R) when V > 0 ->
	intlog10(V div 10, R + 1);
intlog10(0, R) ->
	R.

%% @private
intpow(B, E) when is_integer(B) andalso is_integer(E) andalso E >= 0 ->
	case B of
		0 ->
			0;
		1 ->
			1;
		2 ->
			1 bsl E;
		_ ->
			intpow(B, E, 1)
	end.

%% @private
intpow(B, E, R) when (E rem 2) =:= 0 ->
	intpow(B * B, E div 2, R);
intpow(B, E, R) when (E div 2) =:= 0 ->
	B * R;
intpow(B, E, R) ->
	intpow(B * B, E div 2, B * R).

%% @private
prepend0(B, Size) when byte_size(B) == Size ->
	B;
prepend0(B, Size) when byte_size(B) < Size ->
	<< (binary:copy(<< $0 >>, Size - byte_size(B)))/binary, B/binary >>.
