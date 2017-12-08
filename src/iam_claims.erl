%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  26 Apr 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_claims).

-include_lib("jose/include/jose.hrl").

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	audience := nil | binary(),
	authorized_party := nil | binary(),
	claims := map(),
	expiration_time := nil | non_neg_integer(),
	issued_at := nil | non_neg_integer(),
	issuer := nil | binary(),
	jwt_id := nil | binary() | {hash, atom()},
	key_id := boolean() | binary(),
	not_before := nil | non_neg_integer(),
	subject := nil | binary(),
	atom() => any()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/0]).
-export([new/1]).
-export([expire_after/2]).
-export([issue/1]).
-export([issue/2]).
-export([merge/2]).
-export([not_before/1]).
-export([not_before/2]).
-export([payload/1]).
-export([put/3]).
%% JWT API
-export([encrypt/3]).
-export([seal/3]).
-export([seal/4]).
-export([sign/3]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		audience => nil,
		authorized_party => nil,
		claims => #{},
		expiration_time => nil,
		issued_at => nil,
		issuer => nil,
		jwt_id => nil,
		key_id => true,
		not_before => nil,
		subject => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

new() ->
	new(#{}).

new(BaseMap0) when is_map(BaseMap0) ->
	case maps:take(claims, BaseMap0) of
		{Claims, BaseMap} ->
			maps:fold(fun fold_put/3, '__struct__'(BaseMap), Claims);
		error ->
			'__struct__'(BaseMap0)
	end;
new(List) when is_list(List) ->
	new(maps:from_list(List)).

expire_after(Claims=#{ '__struct__' := ?MODULE, expiration_time := nil }, ExpireAfter) when is_integer(ExpireAfter) andalso ExpireAfter >= 0 ->
	Now =
		case Claims of
			#{ issued_at := nil } ->
				os:system_time(second);
			#{ issued_at := IssuedAt } ->
				IssuedAt
		end,
	ExpirationTime = Now + ExpireAfter,
	Claims#{ expiration_time := ExpirationTime }.

issue(Claims=#{ '__struct__' := ?MODULE, issued_at := nil }) ->
	issue(Claims, os:system_time(second)).

issue(Claims=#{ '__struct__' := ?MODULE, issued_at := nil }, IssuedAt) when is_integer(IssuedAt) andalso IssuedAt >= 0 ->
	Claims#{ issued_at := IssuedAt };
issue(Claims=#{ '__struct__' := ?MODULE, issued_at := nil }, nil) ->
	issue(Claims).

merge(L=#{ '__struct__' := ?MODULE, claims := LClaims }, R=#{ '__struct__' := ?MODULE, claims := RClaims }) ->
	Claims = maps:merge(LClaims, RClaims),
	M = maps:merge(L, R),
	maps:put(claims, Claims, M);
merge(L=#{ '__struct__' := ?MODULE }, Enumerable) ->
	R = ?MODULE:new(Enumerable),
	merge(L, R).

not_before(Claims=#{ '__struct__' := ?MODULE, not_before := nil }) ->
	not_before(Claims, os:system_time(second)).

not_before(Claims=#{ '__struct__' := ?MODULE, not_before := nil }, NotBefore) when is_integer(NotBefore) andalso NotBefore >= 0 ->
	Claims#{ not_before := NotBefore };
not_before(Claims=#{ '__struct__' := ?MODULE, not_before := nil }, nil) ->
	not_before(Claims).

payload(#{
	'__struct__' := ?MODULE,
	audience := Audience,
	authorized_party := AuthorizedParty,
	claims := Claims,
	expiration_time := ExpirationTime,
	issued_at := IssuedAt,
	issuer := Issuer,
	jwt_id := JWTID,
	not_before := NotBefore,
	subject := Subject
}) ->
	payload(#{}, [
		{audience, Audience},
		{authorized_party, AuthorizedParty},
		{expiration_time, ExpirationTime},
		{issued_at, IssuedAt},
		{issuer, Issuer},
		{not_before, NotBefore},
		{subject, Subject},
		{claims, Claims},
		{jwt_id, JWTID}
	]).

put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == audience orelse K == <<"aud">> ->
	Claims#{ audience := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == authorized_party orelse K == <<"azp">> ->
	Claims#{ authorized_party := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == claims ->
	Claims#{ claims := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == expiration_time orelse K == <<"exp">> ->
	Claims#{ expiration_time := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == issued_at orelse K == <<"iat">> ->
	Claims#{ issued_at := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == issuer orelse K == <<"iss">> ->
	Claims#{ issuer := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == jwt_id orelse K == <<"jti">> ->
	Claims#{ jwt_id := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == not_before orelse K == <<"nbf">> ->
	Claims#{ not_before := V };
put(Claims=#{ '__struct__' := ?MODULE }, K, V) when K == subject orelse K == <<"sub">> ->
	Claims#{ subject := V };
put(Claims=#{ '__struct__' := ?MODULE }, Key, Value) when is_atom(Key) ->
	put(Claims, erlang:atom_to_binary(Key, unicode), Value);
put(Claims=#{ '__struct__' := ?MODULE, claims := C0 }, Key, Value) when is_binary(Key) ->
	C1 = maps:put(Key, Value, C0),
	Claims#{ claims := C1 }.

%%%===================================================================
%%% JWT API functions
%%%===================================================================

encrypt(Claims=#{ '__struct__' := ?MODULE }, Encryption, JWK0) ->
	JWT0 = payload(Claims),
	JWT = iam_util:cast_jwt(JWT0),
	JWK = iam_util:cast_jwk(JWK0),
	JWE0 = iam_util:cast_jwe(Encryption),
	JWE = maybe_add_kid(Claims, JWE0, JWK),
	{_, Assertion} = jose_jwe:compact(jose_jwt:encrypt(JWK, JWE, JWT)),
	Assertion.

seal(Claims=#{ '__struct__' := ?MODULE }, Encryption, BPK0) ->
	BPK = jose_jwk:to_public(iam_util:cast_jwk(BPK0)),
	ASK = jose_jwk:generate_key(BPK),
	Assertion = seal(Claims, Encryption, BPK, ASK),
	{ok, Assertion, ASK}.

seal(Claims=#{ '__struct__' := ?MODULE }, Encryption, BPK0, ASK0) ->
	BPK = iam_util:cast_jwk(BPK0),
	ASK = iam_util:cast_jwk(ASK0),
	JWT0 = payload(Claims),
	JWT = iam_util:cast_jwt(JWT0),
	JWE = iam_util:cast_jwe(Encryption),
	{_, JWTPlainText} = jose_jwt:to_binary(JWT),
	{_, Assertion} = jose_jwe:compact(jose_jwe:block_encrypt({BPK, ASK}, JWTPlainText, JWE)),
	Assertion.

sign(Claims=#{ '__struct__' := ?MODULE }, SigningAlg, JWK0) ->
	JWT0 = payload(Claims),
	JWT = iam_util:cast_jwt(JWT0),
	JWK = iam_util:cast_jwk(JWK0),
	JWS0 = iam_util:cast_jws(SigningAlg),
	JWS = maybe_add_kid(Claims, JWS0, JWK),
	{_, Assertion} = jose_jws:compact(jose_jwt:sign(JWK, JWS, JWT)),
	Assertion.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
fold_put(Key, Value, Claims) ->
	?MODULE:put(Claims, Key, Value).

%% @private
maybe_add_kid(#{ key_id := false }, JWE=#jose_jwe{}, #jose_jwk{}) ->
	JWE;
maybe_add_kid(#{ key_id := false }, JWS=#jose_jws{}, #jose_jwk{}) ->
	JWS;
maybe_add_kid(#{ key_id := true }, JWE=#jose_jwe{}, JWK=#jose_jwk{}) ->
	case jose_jwe:to_map(JWE) of
		{_, #{ <<"kid">> := _ }} ->
			JWE;
		{_, JWEMap} ->
			KID =
				case jose_jwk:to_map(JWK) of
					{_, #{ <<"kid">> := KID0 }} ->
						KID0;
					_ ->
						jose_jwk:thumbprint(JWK)
				end,
			iam_util:cast_jwe(maps:put(<<"kid">>, KID, JWEMap))
	end;
maybe_add_kid(#{ key_id := true }, JWS=#jose_jws{}, JWK=#jose_jwk{}) ->
	case jose_jws:to_map(JWS) of
		{_, #{ <<"kid">> := _ }} ->
			JWS;
		{_, JWSMap} ->
			KID =
				case jose_jwk:to_map(JWK) of
					{_, #{ <<"kid">> := KID0 }} ->
						KID0;
					_ ->
						jose_jwk:thumbprint(JWK)
				end,
			iam_util:cast_jws(maps:put(<<"kid">>, KID, JWSMap))
	end;
maybe_add_kid(#{ key_id := KID }, JWE=#jose_jwe{}, #jose_jwk{}) when is_binary(KID) ->
	case jose_jwe:to_map(JWE) of
		{_, #{ <<"kid">> := _ }} ->
			JWE;
		{_, JWEMap} ->
			iam_util:cast_jwe(maps:put(<<"kid">>, KID, JWEMap))
	end;
maybe_add_kid(#{ key_id := KID }, JWS=#jose_jws{}, #jose_jwk{}) when is_binary(KID) ->
	case jose_jws:to_map(JWS) of
		{_, #{ <<"kid">> := _ }} ->
			JWS;
		{_, JWSMap} ->
			iam_util:cast_jws(maps:put(<<"kid">>, KID, JWSMap))
	end.

%% @private
payload(JWT, [{_, nil} | Rest]) ->
	payload(JWT, Rest);
payload(JWT, [{audience, Audience} | Rest]) when is_binary(Audience) ->
	payload(maps:put(<<"aud">>, Audience, JWT), Rest);
payload(JWT, [{authorized_party, AuthorizedParty} | Rest]) when is_binary(AuthorizedParty) ->
	payload(maps:put(<<"azp">>, AuthorizedParty, JWT), Rest);
payload(JWT, [{claims, Claims} | Rest]) when is_map(Claims) ->
	payload(maps:merge(JWT, Claims), Rest);
payload(JWT, [{expiration_time, ExpirationTime} | Rest]) when is_integer(ExpirationTime) ->
	payload(maps:put(<<"exp">>, ExpirationTime, JWT), Rest);
payload(JWT, [{issued_at, IssuedAt} | Rest]) when is_integer(IssuedAt) ->
	payload(maps:put(<<"iat">>, IssuedAt, JWT), Rest);
payload(JWT, [{issuer, Issuer} | Rest]) when is_binary(Issuer) ->
	payload(maps:put(<<"iss">>, Issuer, JWT), Rest);
payload(JWT, [{jwt_id, JWTID} | Rest]) when is_binary(JWTID) ->
	payload(maps:put(<<"jti">>, JWTID, JWT), Rest);
payload(JWT, [{jwt_id, {hash, Hash}} | Rest]) when is_atom(Hash) ->
	Message = iam_json:encode(JWT),
	Digest = crypto:hash(Hash, Message),
	JTI = iam_base64url:encode(Digest, #{ padding => false }),
	payload(maps:put(<<"jti">>, JTI, JWT), Rest);
payload(JWT, [{not_before, NotBefore} | Rest]) when is_integer(NotBefore) ->
	payload(maps:put(<<"nbf">>, NotBefore, JWT), Rest);
payload(JWT, [{subject, Subject} | Rest]) when is_binary(Subject) ->
	payload(maps:put(<<"sub">>, Subject, JWT), Rest);
payload(JWT, []) ->
	JWT.
