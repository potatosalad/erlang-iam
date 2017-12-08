%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  24 Apr 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_assert).

-include("iam_error.hrl").
-include_lib("jose/include/jose.hrl").

%% Types
-type claim_check() ::
	fun((any()) -> boolean()).

-export_type([claim_check/0]).

-type t() :: #{
	'__struct__' := ?MODULE,
	loaded := boolean(),
	validated := boolean(),
	verified := boolean(),
	assertion := nil | binary(),
	audience := nil | binary() | claim_check(),
	authorized_party := nil | binary() | claim_check(),
	checks := #{
		binary() => term() | claim_check()
	},
	claims := nil | map(),
	issuer := nil | binary() | claim_check(),
	jwt_id := nil | binary() | {hash, atom()} | claim_check(),
	max_age := nil | non_neg_integer(),
	not_before := nil | non_neg_integer() | claim_check(),
	now := nil | non_neg_integer(),
	protected := nil | map(),
	public_key := nil | map(),
	required := #{
		expiration_time := boolean(),
		issued_at := boolean(),
		not_before := boolean(),
		atom() => boolean()
	},
	subject := nil | binary() | claim_check(),
	window := non_neg_integer(),
	atom() => any()
}.

-export_type([t/0]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export([new/0]).
-export([new/1]).
-export([load/2]).
-export([check/2]).
-export([check/3]).
-export([not_before/1]).
-export([not_before/2]).
-export([require/2]).
%% JWT API
-export([authenticate/3]).
-export([authenticate/4]).
-export([decrypt/4]).
-export(['info!'/1]).
-export([info/1]).
-export(['info!'/2]).
-export([info/2]).
-export([type/1]).
-export([validate/1]).
-export([validate/2]).
-export([verify/3]).
-export([verify/4]).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		loaded => false,
		validated => false,
		verified => false,
		assertion => nil,
		audience => nil,
		authorized_party => nil,
		checks => #{},
		claims => nil,
		issuer => nil,
		jwt_id => nil,
		max_age => nil,
		not_before => nil,
		now => nil,
		protected => nil,
		public_key => nil,
		required => #{
			expiration_time => true,
			issued_at => true,
			not_before => true
		},
		subject => nil,
		window => 5
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(BaseMap0) when is_map(BaseMap0) ->
	{BaseReq, BaseMap} =
		case maps:take(required, BaseMap0) of
			{BR, BM} when is_map(BR) ->
				{BR, BM};
			{BR, BM} when is_list(BR) ->
				{maps:from_list(BR), BM};
			error ->
				{#{}, BaseMap0}
		end,
	S0 = #{ required := R0 } = '__struct__'(),
	R1 = maps:merge(R0, BaseReq),
	S1 = S0#{ required := R1 },
	maps:fold(fun maps:update/3, S1, BaseMap).

%%%===================================================================
%%% API functions
%%%===================================================================

new() ->
	new(#{}).

new(BaseMap) when is_map(BaseMap) ->
	'__struct__'(BaseMap);
new(List) when is_list(List) ->
	new(maps:from_list(List)).

load(Assert0=#{
	'__struct__' := ?MODULE,
	loaded := false,
	assertion := nil,
	claims := nil
}, Assertion) ->
	try jose_jwt:to_map(jose_jwt:peek_payload(Assertion)) of
		{_, Claims} when is_map(Claims) ->
			Assert = Assert0#{ loaded := true, assertion := Assertion, claims := Claims },
			Assert;
		_ ->
			?iam_throw(iam, invalid_assertion, Assert0)
	catch
		_:_ ->
			?iam_throw(iam, invalid_assertion, Assert0)
	end.

check(A=#{ '__struct__' := ?MODULE }, [{Key, Check} | Checks]) ->
	check(check(A, Key, Check), Checks);
check(A=#{ '__struct__' := ?MODULE }, []) ->
	A.

check(A=#{ '__struct__' := ?MODULE }, K, Check) when is_atom(K) ->
	Key =
		case K of
			audience -> <<"aud">>;
			authorized_party -> <<"azp">>;
			expiration_time -> <<"exp">>;
			issued_at -> <<"iat">>;
			issuer -> <<"iss">>;
			jwt_id -> <<"jti">>;
			not_before -> <<"nbf">>;
			subject -> <<"sub">>;
			_ -> erlang:atom_to_binary(K, unicode)
		end,
	check(A, Key, Check);
check(A=#{ checks := C }, K, V) when is_binary(K) ->
	A#{ checks := maps:put(K, V, C) }.

not_before(Assert=#{ '__struct__' := ?MODULE, not_before := nil }) ->
	not_before(Assert, os:system_time(second)).

not_before(Assert=#{ '__struct__' := ?MODULE, not_before := nil }, NotBefore) when is_integer(NotBefore) andalso NotBefore >= 0 ->
	Assert#{ not_before := NotBefore };
not_before(Assert=#{ '__struct__' := ?MODULE, not_before := nil }, nil) ->
	not_before(Assert, 0).

require(A=#{ required := R }, K) when K == audience orelse K == <<"aud">> ->
	A#{ required := maps:put(audience, true, R) };
require(A=#{ required := R }, K) when K == authorized_party orelse K == <<"azp">> ->
	A#{ required := maps:put(authorized_party, true, R) };
require(A=#{ required := R }, K) when K == expiration_time orelse K == <<"exp">> ->
	A#{ required := maps:put(expiration_time, true, R) };
require(A=#{ required := R }, K) when K == issued_at orelse K == <<"iat">> ->
	A#{ required := maps:put(issued_at, true, R) };
require(A=#{ required := R }, K) when K == issuer orelse K == <<"iss">> ->
	A#{ required := maps:put(issuer, true, R) };
require(A=#{ required := R }, K) when K == jwt_id orelse K == <<"jti">> ->
	A#{ required := maps:put(jwt_id, true, R) };
require(A=#{ required := R }, K) when K == not_before orelse K == <<"nbf">> ->
	A#{ required := maps:put(not_before, true, R) };
require(A=#{ required := R }, K) when K == subject orelse K == <<"sub">> ->
	A#{ required := maps:put(subject, true, R) };
require(A=#{ '__struct__' := ?MODULE }, K) when is_atom(K) ->
	require(A, erlang:atom_to_binary(K, unicode));
require(A=#{ required := R }, K) when is_binary(K) ->
	A#{ required := maps:put(K, true, R) };
require(A=#{ '__struct__' := ?MODULE }, [H | T]) ->
	require(require(A, H), T);
require(A=#{ '__struct__' := ?MODULE }, []) ->
	A.

%%%===================================================================
%%% JWT API functions
%%%===================================================================

authenticate(Assert0=#{
	'__struct__' := ?MODULE,
	loaded := true,
	verified := false,
	validated := false
}, AllowedSigningAlgs, JWK) ->
	Assert1 = validate(Assert0),
	Assert2 = verify(Assert1, AllowedSigningAlgs, JWK),
	Assert2.

authenticate(Assert=#{
	'__struct__' := ?MODULE,
	loaded := false
}, Assertion, Allowed, JWK) ->
	try binary:split(Assertion, << $. >>, [global]) of
		[_, _, _, _, _] ->
			validate(decrypt(Assert, Assertion, Allowed, JWK));
		[_, _, _] ->
			authenticate(load(Assert, Assertion), Allowed, JWK);
		_ ->
			?iam_throw(iam, invalid_assertion, Assert)
	catch
		_:_ ->
			?iam_throw(iam, invalid_assertion, Assert)
	end.

decrypt(Assert0=#{
	'__struct__' := ?MODULE,
	loaded := false,
	verified := false,
	validated := false,
	assertion := nil,
	claims := nil
}, Assertion, AllowedEncryption0, JWK0) when is_binary(Assertion) ->
	AllowedEncryption = [element(2, jose_jwe:to_map(iam_util:cast_jwe(AE))) || AE <- AllowedEncryption0],
	ValidEncryption =
		try jose_jwe:expand(Assertion) of
			{_, #{ <<"protected">> := EncodedProtected }} ->
				try iam_json:decode(iam_base64url:'decode!'(EncodedProtected)) of
					JWEMap when is_map(JWEMap) ->
						valid_encryption(JWEMap, AllowedEncryption);
					_ ->
						?iam_throw(iam, invalid_assertion, Assert0)
				catch
					_:_ ->
						?iam_raise(throw, iam, invalid_assertion, Assert0)
				end;
			_ ->
				?iam_throw(iam, invalid_assertion, Assert0)
		catch
			_:_ ->
				?iam_raise(throw, iam, invalid_assertion, Assert0)
		end,
	case ValidEncryption of
		true ->
			JWK = iam_util:cast_jwk(JWK0),
			try jose_jwe:block_decrypt(JWK, Assertion) of
				{JWTPlainText, JWE=#jose_jwe{}} when is_binary(JWTPlainText) ->
					try iam_json:decode(JWTPlainText) of
						Claims when is_map(Claims) ->
							{_, Protected} = jose_jwe:to_map(JWE),
							PublicKey = maps:get(<<"epk">>, Protected, nil),
							Assert1 = Assert0#{
								loaded := true,
								verified := true,
								assertion := Assertion,
								claims := Claims,
								protected := Protected,
								public_key := PublicKey
							},
							Assert1;
						_ ->
							?iam_throw(iam, invalid_assertion, Assert0)
					catch
						_:_ ->
							?iam_raise(throw, iam, invalid_assertion, Assert0)
					end;
				_ ->
					?iam_throw(iam, invalid_assertion, Assert0)
			catch
				_:_ ->
					?iam_raise(throw, iam, invalid_assertion, Assert0)
			end;
		false ->
			?iam_throw(iam, invalid_assertion, Assert0)
	end.

info(Assertion) ->
	Result =
		try jose_jws:expand(Assertion) of
			{_, #{
				<<"payload">> := JWSPayload,
				<<"protected">> := JWSProtected,
				<<"signature">> := JWSSignature
			}} ->
				try
					Signed = #{
						payload => iam_json:decode(iam_base64url:'decode!'(JWSPayload)),
						protected => iam_json:decode(iam_base64url:'decode!'(JWSProtected)),
						signature => iam_base64url:'decode!'(JWSSignature)
					},
					{ok, Signed}
				catch
					_:_ ->
						error
				end;
			_ ->
				error
		catch
			_:_ ->
				error
		end,
	case Result of
		{ok, _} ->
			Result;
		error ->
			try jose_jwe:expand(Assertion) of
				{_, #{
					<<"ciphertext">> := JWEECiphertext,
					<<"encrypted_key">> := JWEEncryptedKey,
					<<"iv">> := JWEIV,
					<<"protected">> := JWEProtected,
					<<"tag">> := JWETag
				}} ->
					Encrypted = #{
						ciphertext => iam_base64url:'decode!'(JWEECiphertext),
						encrypted_key => iam_base64url:'decode!'(JWEEncryptedKey),
						iv => iam_base64url:'decode!'(JWEIV),
						protected => iam_json:decode(iam_base64url:'decode!'(JWEProtected)),
						tag => iam_base64url:'decode!'(JWETag)
					},
					{ok, Encrypted};
				_ ->
					error
			catch
				_:_ ->
					error
			end
	end.

'info!'(Assertion) ->
	case info(Assertion) of
		{ok, Info} ->
			Info;
		error ->
			?iam_raise(throw, iam, invalid_assertion, nil)
	end.

info(Assertion, Key) ->
	case info(Assertion) of
		{ok, Info} ->
			maps:find(Key, Info);
		error ->
			error
	end.

'info!'(Assertion, Key) ->
	case info(Assertion, Key) of
		{ok, Info} ->
			Info;
		error ->
			?iam_raise(throw, iam, invalid_assertion, nil)
	end.

type(Assertion) ->
	try binary:split(Assertion, << $. >>, [global]) of
		[_, _, _, _, _] ->
			{ok, enc};
		[_, _, _] ->
			{ok, sig};
		_ ->
			error
	catch
		_:_ ->
			error
	end.

validate(Assert0=#{
	'__struct__' := ?MODULE,
	loaded := true,
	validated := false,
	claims := Claims
}) when is_map(Claims) ->
	Now =
		case Assert0 of
			#{ now := nil } ->
				os:system_time(second);
			#{ now := Now0 } ->
				Now0
		end,
	Assert1 = Assert0#{ now := Now },
	Validated =
		validate(Assert1, Claims, [
			required,
			audience,
			expiration_time,
			issued_at,
			issuer,
			max_age,
			jwt_id,
			not_before,
			subject,
			checks
		]),
	case Validated of
		true ->
			Assert = Assert1#{ validated := true },
			Assert;
		false ->
			?iam_throw(iam, invalid_assertion, Assert1)
	end.

validate(Assert=#{
	'__struct__' := ?MODULE,
	loaded := false
}, Assertion) ->
	validate(load(Assert, Assertion)).

verify(Assert0=#{
	'__struct__' := ?MODULE,
	loaded := true,
	verified := false,
	assertion := Assertion,
	claims := Claims
}, AllowedSigningAlgs, JWK0) when is_binary(Assertion) andalso is_map(Claims) ->
	JWK = iam_util:cast_jwk(JWK0),
	try jose_jwt:verify_strict(JWK, AllowedSigningAlgs, Assertion) of
		{true, JWT, JWS} ->
			{_, Claims} = jose_jwt:to_map(JWT),
			{_, Protected} = jose_jws:to_map(JWS),
			Assert = Assert0#{ verified := true, claims := Claims, protected := Protected },
			Assert;
		_ ->
			?iam_throw(iam, invalid_assertion, Assert0)
	catch
		_:_ ->
			?iam_raise(throw, iam, invalid_assertion, Assert0)
	end.

verify(Assert=#{
	'__struct__' := ?MODULE,
	loaded := false
}, Assertion, AllowedSigningAlgs, JWK) ->
	verify(load(Assert, Assertion), AllowedSigningAlgs, JWK).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
valid_encryption(JWE, [H | T]) ->
	case maps:with(maps:keys(H), JWE) of
		H ->
			true;
		_ ->
			valid_encryption(JWE, T)
	end;
valid_encryption(_, []) ->
	false.

%% @private
% audience
validate(Assert=#{ audience := Audience }, Claims, [audience | Spec]) ->
	Valid =
		case maps:find(<<"aud">>, Claims) of
			{ok, Audience} when is_binary(Audience) ->
				true;
			{ok, Challenge} when is_function(Audience, 1) ->
				Audience(Challenge);
			_ when Audience == nil ->
				true;
			{ok, _} ->
				false;
			error ->
				false
		end,
	case Valid of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% checks
validate(Assert=#{ checks := Checks }, Claims, [checks | Spec]) ->
	case validate_checks(Claims, maps:to_list(Checks)) of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% expiration_time
validate(Assert=#{ now := Now, window := Window }, Claims, [expiration_time | Spec]) ->
	case maps:find(<<"exp">>, Claims) of
		{ok, ExpirationTime} when is_integer(ExpirationTime) andalso (ExpirationTime + Window) >= Now ->
			validate(Assert, Claims, Spec);
		{ok, _} ->
			?iam_throw(iam, invalid_assertion, Assert);
		error ->
			validate(Assert, Claims, Spec)
	end;
% issued_at
validate(Assert=#{ now := Now, window := Window }, Claims, [issued_at | Spec]) ->
	case maps:find(<<"iat">>, Claims) of
		{ok, IssuedAt} when is_integer(IssuedAt) andalso IssuedAt =< (Now + Window) ->
			validate(Assert, Claims, Spec);
		{ok, _} ->
			?iam_throw(iam, invalid_assertion, Assert);
		error ->
			validate(Assert, Claims, Spec)
	end;
% issuer
validate(Assert=#{ issuer := Issuer }, Claims, [issuer | Spec]) ->
	Valid =
		case maps:find(<<"iss">>, Claims) of
			{ok, Issuer} when is_binary(Issuer) ->
				true;
			{ok, Challenge} when is_function(Issuer, 1) ->
				Issuer(Challenge);
			_ when Issuer == nil ->
				true;
			{ok, _} ->
				false;
			error ->
				false
		end,
	case Valid of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% jwt_id
validate(Assert=#{ jwt_id := JWTID }, Claims, [jwt_id | Spec]) ->
	Valid =
		case maps:find(<<"jti">>, Claims) of
			{ok, JWTID} when is_binary(JWTID) ->
				validate(Assert, Claims, Spec);
			{ok, Challenge} when is_binary(Challenge) andalso is_tuple(JWTID) andalso element(1, JWTID) == hash andalso is_atom(element(2, JWTID)) ->
				{hash, Hash} = JWTID,
				Message = iam_json:encode(maps:remove(<<"jti">>, Claims)),
				Digest = crypto:hash(Hash, Message),
				JTI = iam_base64url:encode(Digest, #{ padding => false }),
				iam_crypto:constant_time_compare(JTI, Challenge);
			{ok, Challenge} when is_function(JWTID, 1) ->
				JWTID(Challenge);
			_ when JWTID == nil ->
				true;
			{ok, _} ->
				false;
			error ->
				false
		end,
	case Valid of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% max_age
validate(Assert=#{ max_age := nil }, Claims, [max_age | Spec]) ->
	validate(Assert, Claims, Spec);
validate(Assert=#{ max_age := MaxAge, now := Now, window := Window }, Claims, [max_age | Spec]) ->
	Valid =
		case Claims of
			#{ <<"iat">> := IssuedAt, <<"exp">> := ExpirationTime } ->
				is_integer(IssuedAt) andalso is_integer(ExpirationTime) andalso (ExpirationTime - IssuedAt) =< (MaxAge + Window);
			#{ <<"exp">> := ExpirationTime } ->
				is_integer(ExpirationTime) andalso (ExpirationTime - Now) =< (MaxAge + Window);
			_ ->
				true
		end,
	case Valid of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% not_before
validate(Assert=#{ not_before := NotBefore, now := Now, window := Window }, Claims, [not_before | Spec]) ->
	NotBeforeValid =
		case maps:find(<<"nbf">>, Claims) of
			{ok, Challenge} when is_integer(NotBefore) andalso is_integer(Challenge) andalso Challenge =< (Now + Window) andalso (Challenge + Window) >= NotBefore ->
				true;
			{ok, Challenge} when is_function(NotBefore, 1) ->
				NotBefore(Challenge);
			{ok, Challenge} when NotBefore == nil andalso is_integer(Challenge) andalso Challenge =< (Now + Window) ->
				true;
			{ok, _} ->
				false;
			error when is_function(NotBefore, 1) ->
				NotBefore(nil);
			error ->
				true
		end,
	IssuedAtValid =
		case maps:find(<<"iat">>, Claims) of
			{ok, IssuedAt} when is_integer(NotBefore) andalso is_integer(IssuedAt) andalso IssuedAt =< (Now + Window) andalso (IssuedAt + Window) >= NotBefore ->
				true;
			{ok, IssuedAt} when is_function(NotBefore, 1) ->
				NotBefore(IssuedAt);
			_ when NotBefore == nil ->
				true;
			{ok, _} ->
				false;
			error ->
				false
		end,
	case NotBeforeValid andalso IssuedAtValid of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% required
validate(Assert=#{ required := Required }, Claims, [required | Spec]) ->
	case validate_required(Claims, maps:to_list(Required)) of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% subject
validate(Assert=#{ subject := Subject }, Claims, [subject | Spec]) ->
	Valid =
		case maps:find(<<"sub">>, Claims) of
			{ok, Subject} when is_binary(Subject) ->
				true;
			{ok, Challenge} when is_function(Subject, 1) ->
				Subject(Challenge);
			_ when Subject == nil ->
				true;
			{ok, _} ->
				false;
			error ->
				false
		end,
	case Valid of
		true ->
			validate(Assert, Claims, Spec);
		false ->
			?iam_throw(iam, invalid_assertion, Assert)
	end;
% validation success
validate(_Assert, _Claims, []) ->
	true;
% validation failure
validate(_Assert, _Claims, _Spec) ->
	false.

%% @private
validate_checks(Claims, [{Key, Check} | Checks]) when is_function(Check, 1) ->
	case maps:find(Key, Claims) of
		{ok, Challenge} ->
			case Check(Challenge) of
				true ->
					validate_checks(Claims, Checks);
				false ->
					false
			end;
		error ->
			false
	end;
validate_checks(Claims, [{Key, Check} | Checks]) ->
	case maps:find(Key, Claims) of
		{ok, Check} ->
			validate_checks(Claims, Checks);
		{ok, _} ->
			false;
		error ->
			false
	end;
validate_checks(_Claims, []) ->
	true.

%% @private
validate_required(Claims=#{ <<"aud">> := Audience }, [{audience, true} | Required]) when is_binary(Audience) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"azp">> := AuthorizedParty }, [{authorized_party, true} | Required]) when is_binary(AuthorizedParty) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"exp">> := ExpirationTime }, [{expiration_time, true} | Required]) when is_integer(ExpirationTime) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"iat">> := IssuedAt }, [{issued_at, true} | Required]) when is_integer(IssuedAt) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"iss">> := Issuer }, [{issuer, true} | Required]) when is_binary(Issuer) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"jti">> := JWTID }, [{jwt_id, true} | Required]) when is_binary(JWTID) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"nbf">> := NotBefore }, [{not_before, true} | Required]) when is_integer(NotBefore) ->
	validate_required(Claims, Required);
validate_required(Claims=#{ <<"sub">> := Subject }, [{subject, true} | Required]) when is_binary(Subject) ->
	validate_required(Claims, Required);
validate_required(Claims, [{_, false} | Required]) ->
	validate_required(Claims, Required);
validate_required(Claims, [{Key, true} | Required]) when is_binary(Key) ->
	case maps:is_key(Key, Claims) of
		true ->
			validate_required(Claims, Required);
		false ->
			false
	end;
validate_required(_Claims, []) ->
	true;
validate_required(_Claims, _Required) ->
	false.
