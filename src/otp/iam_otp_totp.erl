%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc RFC 6238: https://tools.ietf.org/html/rfc6238
%%%
%%% @end
%%% Created :  09 May 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_otp_totp).

%% Types
-type time() :: non_neg_integer().
-type options() :: #{
	digits => non_neg_integer(),
	hash => atom(),
	time_step_size => non_neg_integer(),
	time_step_backward => non_neg_integer(),
	time_step_forward => non_neg_integer()
}.

%% API
-export([authenticate/1]).
-export([authenticate/2]).
-export([authenticate/3]).
-export([validate/2]).
-export([validate/3]).
-export([validate/4]).

%% Macros
-define(is_time(T),
	(is_integer(T) andalso T >= 0 andalso T =< 16#ffffffffffffffff)).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec authenticate(K :: binary()) -> binary().
authenticate(K) ->
	authenticate(K, os:system_time(seconds)).

-spec authenticate(K :: binary(), TOrOpts :: time() | options()) -> binary().
authenticate(K, T) when is_binary(K) andalso ?is_time(T) ->
	authenticate(K, T, #{});
authenticate(K, Opts) when is_binary(K) andalso is_map(Opts) ->
	authenticate(K, os:system_time(seconds), Opts).

-spec authenticate(K :: binary(), T :: time(), Opts :: options()) -> binary().
authenticate(K, T, Opts) when is_binary(K) andalso ?is_time(T) andalso is_map(Opts) ->
	TimeStepSize = maps:get(time_step_size, Opts, 30),
	TC = T div TimeStepSize,
	iam_otp_hotp:authenticate(K, TC, Opts).

-spec validate(K :: binary(), TOTP :: binary()) -> boolean().
validate(K, TOTP) when is_binary(K) andalso is_binary(TOTP) ->
	validate(K, os:system_time(seconds), TOTP).

-spec validate(K :: binary(), TOrTOTP :: time() | binary(), TOTPOrOpts :: binary() | options()) -> boolean().
validate(K, T, TOTP) when is_binary(K) andalso ?is_time(T) andalso is_binary(TOTP) ->
	validate(K, T, TOTP, #{});
validate(K, TOTP, Opts) when is_binary(K) andalso is_binary(TOTP) andalso is_map(Opts) ->
	validate(K, os:system_time(seconds), TOTP, Opts).

-spec validate(K :: binary(), T :: time(), TOTP :: binary(), Opts :: options()) -> boolean().
validate(K, T, TOTP, Opts) when is_binary(K) andalso ?is_time(T) andalso is_binary(TOTP) andalso is_map(Opts) ->
	TimeStepSize = maps:get(time_step_size, Opts, 30),
	TC = T div TimeStepSize,
	case iam_otp_hotp:validate(K, TC, TOTP, Opts) of
		true ->
			true;
		false ->
			case maybe_validate_backward(K, TC, TOTP, Opts) of
				true ->
					true;
				false ->
					maybe_validate_forward(K, TC, TOTP, Opts)
			end
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
maybe_validate_backward(K, TC, TOTP, Opts=#{ time_step_backward := Step }) when TC > 0 andalso is_integer(Step) andalso Step >= 1 ->
	validate_backward(K, TC - 1, TOTP, Step - 1, Opts);
maybe_validate_backward(_, _, _, _) ->
	false.

%% @private
maybe_validate_forward(K, TC, TOTP, Opts=#{ time_step_forward := Step }) when TC > 0 andalso is_integer(Step) andalso Step >= 1 ->
	validate_forward(K, TC + 1, TOTP, Step - 1, Opts);
maybe_validate_forward(_, _, _, _) ->
	false.

%% @private
validate_backward(K, TC, TOTP, 0, Opts) ->
	iam_otp_hotp:validate(K, TC, TOTP, Opts);
validate_backward(K, TC, TOTP, Step, Opts) when Step > 0 ->
	case iam_otp_hotp:validate(K, TC, TOTP, Opts) of
		true ->
			true;
		false ->
			validate_backward(K, TC - 1, TOTP, Step - 1, Opts)
	end.

%% @private
validate_forward(K, TC, TOTP, 0, Opts) ->
	iam_otp_hotp:validate(K, TC, TOTP, Opts);
validate_forward(K, TC, TOTP, Step, Opts) when Step > 0 ->
	case iam_otp_hotp:validate(K, TC, TOTP, Opts) of
		true ->
			true;
		false ->
			validate_forward(K, TC + 1, TOTP, Step - 1, Opts)
	end.
