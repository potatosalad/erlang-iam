%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  27 Apr 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_config).

-compile({no_auto_import, [get/1]}).

%% API
-export([start_link/0]).
-export([new/1]).
-export([delete/1]).
-export([get/1]).
-export([get_and_put/2]).
-export([get_and_update/2]).
-export([put/2]).
-export([update/2]).
%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

new(Opts) ->
	Tab = ets:new(?MODULE, [
		named_table,
		ordered_set,
		public,
		{read_concurrency, true}
	]),
	true = ets:insert_new(Tab, Opts),
	Tab.

delete(?MODULE) ->
	ets:delete(?MODULE).

get(Key) ->
	case ets:lookup(?MODULE, Key) of
		[{Key, Value}] ->
			Value;
		[] ->
			nil
	end.

get_and_put(Key, Value) ->
	gen_server:call(?MODULE, {get_and_put, Key, Value}).

get_and_update(Key, GetAndUpdate) when is_function(GetAndUpdate, 1) ->
	gen_server:call(?MODULE, {get_and_update, Key, GetAndUpdate}).

put(Key, Value) ->
	gen_server:call(?MODULE, {put, Key, Value}).

update(Key, Update) when is_function(Update, 1) ->
	gen_server:call(?MODULE, {update, Key, Update}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Tab) ->
	% ETS table must be writable
	public = ets:info(Tab, protection),
	{ok, Tab}.

handle_call({get_and_put, Key, Value}, _From, Tab) ->
	Value = get(Key),
	true = ets:insert(Tab, {Key, Value}),
	{reply, Value, Tab};
handle_call({get_and_update, Key, GetAndUpdate}, _From, Tab) ->
	Value = get(Key),
	OldValue =
		case GetAndUpdate(Value) of
			pop ->
				true = ets:delete(Tab, Key),
				Value;
			{OldValue0, NewValue} ->
				true = ets:insert(Tab, NewValue),
				OldValue0
		end,
	{reply, OldValue, Tab};
handle_call({put, Key, Value}, _From, Tab) ->
	true = ets:insert(Tab, {Key, Value}),
	{reply, ok, Tab};
handle_call({update, Key, Update}, _From, Tab) ->
	Value = Update(get(Key)),
	{reply, Value, Tab}.

handle_cast(Cast, Tab) ->
	{stop, {bad_cast, Cast}, Tab}.

handle_info(_Info, Tab) ->
	{noreply, Tab}.

terminate(_Reason, _Tab) ->
	ok.

code_change(_OldVsn, Tab, _Extra) ->
	{ok, Tab}.
