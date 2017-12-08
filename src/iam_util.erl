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
-module(iam_util).

-include_lib("jose/include/jose.hrl").

%% API
-export([cast_jwe/1]).
-export([cast_jwk/1]).
-export([cast_jws/1]).
-export([cast_jwt/1]).
-export([list_join/2]).
-export([lowercase/1]).

%% Macros
-define(LC(C),
	case C of
		$A -> $a;
		$B -> $b;
		$C -> $c;
		$D -> $d;
		$E -> $e;
		$F -> $f;
		$G -> $g;
		$H -> $h;
		$I -> $i;
		$J -> $j;
		$K -> $k;
		$L -> $l;
		$M -> $m;
		$N -> $n;
		$O -> $o;
		$P -> $p;
		$Q -> $q;
		$R -> $r;
		$S -> $s;
		$T -> $t;
		$U -> $u;
		$V -> $v;
		$W -> $w;
		$X -> $x;
		$Y -> $y;
		$Z -> $z;
		_ -> C
	end).

%%%===================================================================
%%% API functions
%%%===================================================================

cast_jwe(JWE=#jose_jwe{}) ->
	JWE;
cast_jwe(JWE=#{ '__struct__' := 'Elixir.JOSE.JWE' }) ->
	'Elixir.JOSE.JWE':to_record(JWE);
cast_jwe({EncryptionAlg, EncryptionEnc}) when is_binary(EncryptionAlg) andalso is_binary(EncryptionEnc) ->
	cast_jwe(#{ <<"alg">> => EncryptionAlg, <<"enc">> => EncryptionEnc });
cast_jwe(Map) when is_map(Map) ->
	jose_jwe:from(Map).

cast_jwk(JWK=#jose_jwk{}) ->
	JWK;
cast_jwk(JWK=#{ '__struct__' := 'Elixir.JOSE.JWK' }) ->
	'Elixir.JOSE.JWK':to_record(JWK);
cast_jwk(Map) when is_map(Map) ->
	jose_jwk:from(Map);
cast_jwk(Binary) when is_binary(Binary) ->
	jose_jwk:from_oct(Binary).

cast_jws(JWS=#jose_jws{}) ->
	JWS;
cast_jws(JWS=#{ '__struct__' := 'Elixir.JOSE.JWS' }) ->
	'Elixir.JOSE.JWS':to_record(JWS);
cast_jws(Map) when is_map(Map) ->
	jose_jws:from(Map);
cast_jws(Binary) when is_binary(Binary) ->
	cast_jws(#{ <<"alg">> => Binary, <<"typ">> => <<"JWT">> }).

cast_jwt(JWT=#jose_jwt{}) ->
	JWT;
cast_jwt(JWT=#{ '__struct__' := 'Elixir.JOSE.JWT' }) ->
	'Elixir.JOSE.JWT':to_record(JWT);
cast_jwt(Map) when is_map(Map) ->
	jose_jwt:from(Map);
cast_jwt(Binary) when is_binary(Binary) ->
	jose_jwt:from_binary(Binary).

list_join(List, Sep) when is_list(List) ->
	list_join(List, Sep, <<>>).

lowercase(List) when is_list(List) ->
	lowercase(erlang:iolist_to_binary(List));
lowercase(Binary) when is_binary(Binary) ->
	<< << ?LC(C) >> || << C >> <= Binary >>.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
list_join([H], _Sep, Acc) ->
	<< Acc/binary, H/binary >>;
list_join([H | T], Sep, Acc) ->
	list_join(T, Sep, << Acc/binary, H/binary, Sep >>);
list_join([], _Sep, Acc) ->
	Acc.
