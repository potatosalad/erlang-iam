%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2017, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  15 Jun 2017 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(iam_engine).

%% Types
-type t() :: #{
	'__struct__' := ?MODULE,
	modules := [module()],
	state := state()
}.

-export_type([t/0]).

-type engine() :: #{
	'__engine__' := t()
}.

-export_type([engine/0]).

-type predicate(ElementType) ::
	fun((Element :: ElementType) -> boolean()).

-export_type([predicate/1]).

-type predicate() :: predicate(term()).

-export_type([predicate/0]).

-type state() ::
	state_name() |
	term(). % For handle_event/4 callback function

-type state_name() :: atom().

-type data() :: term().

-type exception() :: #{ '__exception__' := true }.

-type event_type() ::
	'cast' | 'info' | 'internal'.

-export_type([event_type/0]).

-type action() ::
	%% All 'next_event' events are kept in a list and then
	%% inserted at state changes so the first in the
	%% action() list is the first to be delivered.
	{'next_event', % Insert event as the next to handle
		EventType :: event_type(),
		EventContent :: term()}.

-export_type([action/0]).

-type init_result(StateType) ::
	{ok, State :: StateType, Data :: data()} |
	{ok, State :: StateType, Data :: data(),
		Actions :: [action()] | action()} |
	'ignore' |
	{'stop', Reason :: term()}.

-export_type([init_result/1]).

-type state_enter_result(State) ::
	{'next_state', % {next_state,NextState,NewData,[]}
		State,
		NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
		State,
		NewData :: data(),
		Actions :: [action()] | action()} |
	state_callback_result(action()).

-export_type([state_enter_result/1]).

-type event_handler_result(StateType) ::
	{'next_state', % {next_state,NextState,NewData,[]}
		NextState :: StateType,
		NewData :: data()} |
	{'next_state', % State transition, maybe to the same state
		NextState :: StateType,
		NewData :: data(),
		Actions :: [action()] | action()} |
	state_callback_result(action()).

-export_type([event_handler_result/1]).

-type exception_handler_result(StateType) ::
	'raise' | % Raise exception
	{'raise', % Raise exception, change data
		NewData :: data()} |
	event_handler_result(StateType).

-export_type([exception_handler_result/1]).

-type state_callback_result(ActionType) ::
	{'halt_state', % {halt_state,NewData,[]}
		NewData :: data()} |
	{'halt_state', % Halt state, change data
		NewData :: data(),
		Actions :: [ActionType] | ActionType} |
	'halt_state_and_data' | % {halt_state_and_data,[]}
	{'halt_state_and_data', % Halt state and data -> only actions
		Actions :: [ActionType] | ActionType} |
	{'keep_state', % {keep_state,NewData,[]}
		NewData :: data()} |
	{'keep_state', % Keep state, change data
		NewData :: data(),
		Actions :: [ActionType] | ActionType} |
	'keep_state_and_data' | % {keep_state_and_data,[]}
	{'keep_state_and_data', % Keep state and data -> only actions
		Actions :: [ActionType] | ActionType} |
	%%
	{'repeat_state', % {repeat_state,NewData,[]}
		NewData :: data()} |
	{'repeat_state', % Repeat state, change data
		NewData :: data(),
		Actions :: [ActionType] | ActionType} |
	'repeat_state_and_data' | % {repeat_state_and_data,[]}
	{'repeat_state_and_data', % Repeat state and data -> only actions
		Actions :: [ActionType] | ActionType} |
	%%
	'stop' | % {stop,normal}
	{'stop', % Stop the server
		Reason :: term()} |
	{'stop', % Stop the server
		Reason :: term(),
		NewData :: data()}.

-type call_handler_result() ::
	'noreply' |
	{'reply', Reply :: term()} |
	call_result().

-export_type([call_handler_result/0]).

-type call_result() ::
	{'noreply', NewData :: data()} |
	{'reply', Reply :: term(), NewData :: data()}.

-export_type([call_result/0]).

-type internal_event() :: {event_type(), term()}.

-type internal_state() :: #{
	data := nil | data(),
	enter := boolean(),
	halt := boolean(),
	lock := boolean(),
	module := nil | module(),
	modules := [module()],
	queue := nil | [module()],
	skip := nil | module(),
	state := nil | state()
}.

%% Callbacks
-callback '__engine__'() -> [module()].
-callback init(Args :: term()) -> init_result(state()).
-callback handle_event('enter', OldState :: state(), State, Data :: data()) -> state_enter_result(State);
					  (event_type(), EventContent :: term(), State :: state(), Data :: data()) -> event_handler_result(state()).
-callback handle_call(Request :: term(), State :: state(), Data :: data()) -> call_handler_result().
-callback handle_exception(Exception :: exception(), State :: state(), Data :: data()) -> exception_handler_result(state()).
-callback terminate(Reason :: 'normal' | 'shutdown' | {'shutdown', term()} | term(), State :: state(), Data :: data()) -> any().

-optional_callbacks(['__engine__'/0]).
-optional_callbacks([init/1]).
-optional_callbacks([handle_event/4]).
-optional_callbacks([handle_call/3]).
-optional_callbacks([handle_exception/3]).
-optional_callbacks([terminate/3]).

%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
%% API
-export(['__resolve__'/1]).
-ignore_xref(['__resolve__'/1]).
-export([new/1]).
-export([new/2]).
-export([apply/3]).
-export([apply/4]).
-export([apply/5]).
-export([call/2]).
-export([call_if/2]).
-export([call_if/3]).
-export([call_while/2]).
-export([call_while/3]).
-export([cast/2]).
-export([info/2]).
-export([resolve/1]).
%% Private API
-export([is_true/1]).

%% Macros
-define(MATCH_ENGINE, #{ '__engine__' := #{ '__struct__' := ?MODULE } }).
-define(STACKTRACE(),
	try erlang:throw(ok) catch _ -> erlang:get_stacktrace() end).

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

-spec '__struct__'() -> t().
'__struct__'() ->
	#{
		'__struct__' => ?MODULE,
		modules => nil,
		state => nil
	}.

-spec '__struct__'(Enumerable :: [{atom(), term()}] | #{ atom() => term() }) -> t().
'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @private
-spec '__resolve__'(Module :: module()) -> Modules :: [module()].
'__resolve__'(Module) when is_atom(Module) ->
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, '__engine__', 0) of
		true ->
			case Module:'__engine__'() of
				List when is_list(List) ->
					case lists:member(Module, List) of
						true ->
							List;
						false ->
							[Module | List]
					end
			end;
		false ->
			[Module]
	end.

-spec new(Module :: module()) -> Engine :: engine().
new(Module) ->
	new(Module, nil).

-spec new(Module :: module(), Args :: term()) -> Engine :: engine().
new(Module, Args) ->
	Modules = resolve(Module),
	S = #{
		data => nil,
		enter => false,
		halt => false,
		lock => false,
		module => nil,
		modules => Modules,
		queue => nil,
		skip => nil,
		state => nil
	},
	init(S, Args).

-spec apply(Engine :: engine(), Function :: atom(), Arguments :: [term()]) -> nil | term().
apply(Engine=?MATCH_ENGINE, Function, Arguments) ->
	?MODULE:apply(Engine, Function, Arguments, fun ?MODULE:is_true/1).

-spec apply(Engine :: engine(), Function :: atom(), Arguments :: [term()], Predicate :: predicate()) -> nil | term().
apply(Engine=?MATCH_ENGINE, Function, Arguments, Predicate) when is_atom(Function) andalso is_list(Arguments) andalso is_function(Predicate, 1) ->
	?MODULE:apply(Engine, Function, Arguments, Predicate, nil).

-spec apply(Engine :: engine(), Function :: atom(), Arguments :: [term()], Predicate :: predicate(), Default) -> Default | term().
apply(#{ '__engine__' := #{ '__struct__' := ?MODULE, modules := Modules } }, Function, Arguments, Predicate, Default) when is_atom(Function) andalso is_list(Arguments) andalso is_function(Predicate, 1) ->
	apply_until(Modules, Function, Arguments, Predicate, Default).

-spec cast(Engine :: engine(), Message :: term()) -> NewEngine :: engine().
cast(Engine=?MATCH_ENGINE, Message) ->
	S = global_wrap(Engine),
	global_event(S, [], {cast, Message}).

-spec call(Engine :: engine(), Request :: term()) -> call_result().
call(Engine=#{ '__engine__' := #{ '__struct__' := ?MODULE, modules := Modules, state := State } }, Request) ->
	call_until_reply(Modules, Request, State, Engine).

-spec call_if(Engine :: engine(), Request :: term()) -> {boolean(), NewEngine :: engine()}.
call_if(Engine=?MATCH_ENGINE, Request) ->
	Predicate = fun ?MODULE:is_true/1,
	call_if(Engine, Request, Predicate).

-spec call_if(Engine :: engine(), Request :: term(), Predicate :: predicate()) -> {boolean(), NewEngine :: engine()}.
call_if(Engine=?MATCH_ENGINE, Request, Predicate) when is_function(Predicate, 1) ->
	case call(Engine, Request) of
		{reply, Result, NewEngine} ->
			case Predicate(Result) of
				true ->
					{true, NewEngine};
				false ->
					{false, NewEngine}
			end;
		{noreply, NewEngine} ->
			{false, NewEngine}
	end.

-spec call_while(Engine :: engine(), Requests :: [term()]) -> NewEngine :: engine().
call_while(Engine=?MATCH_ENGINE, Requests) when is_list(Requests) ->
	Predicate = fun ?MODULE:is_true/1,
	call_while(Engine, Requests, Predicate).

-spec call_while(Engine :: engine(), Requests :: [term()], Predicate :: predicate()) -> NewEngine :: engine().
call_while(Engine=?MATCH_ENGINE, [Request | Requests], Predicate) when is_function(Predicate, 1) ->
	case call(Engine, Request) of
		{reply, Result, NewEngine} ->
			case Predicate(Result) of
				true ->
					call_while(NewEngine, Requests, Predicate);
				false ->
					NewEngine
			end;
		{noreply, NewEngine} ->
			NewEngine
	end;
call_while(Engine=?MATCH_ENGINE, [], Predicate) when is_function(Predicate, 1) ->
	Engine.

-spec info(Engine :: engine(), Info :: term()) -> NewEngine :: engine().
info(Engine=?MATCH_ENGINE, Info) ->
	S = global_wrap(Engine),
	global_event(S, [], {info, Info}).

-spec resolve(Module :: module()) -> Modules :: [module()].
resolve(Module) when is_atom(Module) ->
	Info = '__resolve__'(Module),
	Tree = maps:put(Module, Info, maps:new()),
	Skip = maps:put(Module, [], maps:new()),
	resolve(Info, Tree, Skip, []).

%%%===================================================================
%%% Private API functions
%%%===================================================================

%% @private
-spec is_true(term()) -> boolean().
is_true(true) -> true;
is_true(_) -> false.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
-spec apply_until([module()], atom(), [term()], predicate(), Default) -> Default | term().
apply_until([Module | Modules], Function, Arguments, Predicate, Default) ->
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, Function, length(Arguments)) of
		true ->
			Result = erlang:apply(Module, Function, Arguments),
			case Predicate(Result) of
				false ->
					apply_until(Modules, Function, Arguments, Predicate, Default);
				true ->
					Result
			end;
		false ->
			apply_until(Modules, Function, Arguments, Predicate, Default)
	end;
apply_until([], _Function, _Arguments, _Predicate, Default) ->
	Default.

%% @private
-spec call_state_function(internal_state(), event_type(), term(), state(), data()) -> {ok, term()} | {term(), term(), term()}.
call_state_function(#{ module := Module }, Type, Content, State, Data) ->
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, handle_event, 4) of
		true ->
			try Module:handle_event(Type, Content, State, Data) of
				Result ->
					{ok, Result}
			catch
				Class:Reason ->
					{Class, Reason, erlang:get_stacktrace()}
			end;
		false ->
			{ok, keep_state_and_data}
	end.

%% @private
-spec deduplicate([term()]) -> [term()].
deduplicate(List) when is_list(List) ->
	deduplicate(List, #{}, []).

%% @private
-spec deduplicate([term()], #{ term() => [] }, [term()]) -> [term()].
deduplicate([H | T], D, AccR) ->
	case maps:is_key(H, D) of
		true ->
			deduplicate(T, D, AccR);
		false ->
			deduplicate(T, maps:put(H, [], D), [H | AccR])
	end;
deduplicate([], _, AccR) ->
	lists:reverse(AccR).

%% @private
-spec event_type(term()) -> boolean().
event_type(Type) ->
	case Type of
		cast -> true;
		info -> true;
		internal -> true;
		_ -> false
	end.

%% @private
-spec call_until_reply([module()], state(), term(), engine()) -> call_result().
call_until_reply([Module | Modules], Request, State, Data) ->
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, handle_call, 3) of
		true ->
			% error_logger:info_msg("~w:handle_call(~p, ~p, ...)~n", [Module, Request, State]),
			io:format("~w:handle_call(~p, ~p, ...)~n", [Module, Request, State]),
			case Module:handle_call(Request, State, Data) of
				noreply ->
					call_until_reply(Modules, Request, State, Data);
				{noreply, NewData=?MATCH_ENGINE} ->
					call_until_reply(Modules, Request, State, NewData);
				{reply, Reply} ->
					{reply, Reply, Data};
				{reply, Reply, NewData=?MATCH_ENGINE} ->
					{reply, Reply, NewData};
				Result ->
					erlang:raise(error, {bad_return_from_call_function, Result}, ?STACKTRACE())
			end;
		false ->
			call_until_reply(Modules, Request, State, Data)
	end;
call_until_reply([], _Request, _State, Data) ->
	{noreply, Data}.

%% @private
-spec global_enter(S :: internal_state(), State :: state(), Data :: data(), Actions :: [action()] | action()) -> NewData :: data().
global_enter(S=#{ modules := Modules }, State, Data, Actions) ->
	Events = [],
	Event = {internal, init_state},
	Engine = '__struct__'(#{
		modules => Modules,
		state => State
	}),
	NewData = maps:put('__engine__', Engine, Data),
	NewS = S#{ data := NewData, state := State },
	global_event_actions(NewS, Events, Event, State, NewData, Actions, true).

%% @private
-spec global_event(S :: internal_state(), Events :: [internal_event()], Event :: internal_event()) -> NewData :: data().
global_event(S0=#{ modules := Modules, queue := nil }, Events, Event) ->
	S1 = S0#{ queue := Modules },
	global_event(S1, Events, Event);
global_event(S0=#{ queue := [Module | Modules], skip := Skip }, Events, Event) when Module =:= ?MODULE orelse Module =:= Skip ->
	S1 = S0#{ module := Module, queue := Modules },
	global_event(S1, Events, Event);
global_event(S0=#{ queue := [Module | Modules], state := State }, Events, Event) ->
	S1 = S0#{ module := Module, queue := Modules },
	case global_unlock(loop_event(global_lock(S1), Events, Event)) of
		S2=#{ halt := true } ->
			global_unwrap(S2);
		S2=#{ enter := true, state := NextState, data := NewData } ->
			S3 = S2#{ enter := false, queue := nil, skip := Module, state := State },
			global_event_enter(S3, [], Event, NextState, NewData, []);
		S2=#{ enter := false, queue := nil } ->
			global_unwrap(S2);
		S2=#{ enter := false } ->
			global_event(S2, [], Event)
	end;
global_event(S=#{ queue := [] }, [], _Event) ->
	global_unwrap(S).

%% @private
-spec global_event_actions(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), NextState :: state(), NewData :: data(), Actions :: [action()] | action(), EnterCall :: boolean()) -> NewerData :: data().
global_event_actions(S0=#{ module := Module }, Events, Event, NextState, NewData, Actions, EnterCall) when Module =/= nil ->
	case global_unlock(loop_event_actions(global_lock(S0), Events, Event, NextState, NewData, Actions, EnterCall)) of
		S1=#{ halt := true } ->
			global_unwrap(S1);
		S1=#{ enter := true, state := NewerNextState, data := NewerData } ->
			S2 = S1#{ enter := false, queue := nil, skip := Module, state := NextState },
			global_event_enter(S2, [], Event, NewerNextState, NewerData, []);
		S1=#{ enter := false, state := NextState, data := NewerData } when EnterCall ->
			S2 = S1#{ enter := false, queue := nil, skip := Module, state := NextState },
			global_event_enter(S2, [], Event, NextState, NewerData, []);
		S1=#{ enter := false, state := NextState } ->
			global_unwrap(S1)
	end.

%% @private
-spec global_event_enter(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), NextState :: state(), NewData :: data(), NextEventsR :: [internal_event()]) -> NewerData :: data().
global_event_enter(S0=#{ modules := Modules, queue := nil }, Events, Event, NextState, NewData, NextEventsR) ->
	S1 = S0#{ queue := Modules },
	global_event_enter(S1, Events, Event, NextState, NewData, NextEventsR);
global_event_enter(S0=#{ queue := [Module | Modules], skip := Skip }, Events, Event, NextState, NewData, NextEventsR) when Module =:= ?MODULE orelse Module =:= Skip ->
	S1 = S0#{ module := Module, queue := Modules },
	global_event_enter(S1, Events, Event, NextState, NewData, NextEventsR);
global_event_enter(S0=#{ queue := [Module | Modules], state := State }, Events, Event, NextState, NewData, NextEventsR) ->
	S1 = S0#{ module := Module, queue := Modules },
	case global_unlock(loop_event_enter(global_lock(S1), Events, Event, NextState, NewData, NextEventsR)) of
		S2=#{ halt := true } ->
			global_unwrap(S2);
		S2=#{ enter := true, state := NewerNextState, data := NewerData } ->
			S3 = S2#{ enter := false, queue := nil, skip := Module, state := NextState },
			global_event_enter(S3, [], Event, NewerNextState, NewerData, []);
		S2=#{ enter := false, queue := nil } ->
			global_unwrap(S2);
		S2=#{ enter := false, state := NextState, data := NewerData } ->
			S3 = S2#{ state := State },
			global_event_enter(S3, [], Event, NextState, NewerData, [])
	end;
global_event_enter(S0=#{ queue := [], data := NewData }, [], _Event, NextState, NewData, []) ->
	S1 = S0#{ state := NextState },
	global_unwrap(S1).

%% @private
-spec global_exception(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), State :: state(), Data :: data(), Class :: term(), Exception :: exception(), Stacktrace :: term()) -> NewerData :: data().
global_exception(S0=#{ modules := Modules, queue := nil }, Events, Event, State, Data, Class, Exception, Stacktrace) ->
	S1 = S0#{ queue := Modules },
	global_exception(S1, Events, Event, State, Data, Class, Exception, Stacktrace);
global_exception(S0=#{ queue := [Module | Modules], skip := Skip }, Events, Event, State, Data, Class, Exception, Stacktrace) when Module =:= ?MODULE orelse Module =:= Skip ->
	S1 = S0#{ module := Module, queue := Modules },
	global_exception(S1, Events, Event, State, Data, Class, Exception, Stacktrace);
global_exception(S0=#{ queue := [Module | Modules] }, Events, Event, State, Data, Class, Exception, Stacktrace) ->
	S1 = S0#{ module := Module, queue := Modules },
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, handle_exception, 3) of
		true ->
			try Module:handle_exception(Exception, State, Data) of
				raise ->
					global_exception(S1, Events, Event, State, Data, Class, Exception, Stacktrace);
				{raise, NewData} ->
					global_exception(S1, Events, Event, State, NewData, Class, Exception, Stacktrace);
				Result ->
					{S2, NextState, NewData, Actions, EnterCall} =
						parse_event_result(true, S1, Events, Event, State, Data, Result),
					S3 = S2#{ queue := nil },
					global_event_actions(S3, Events, Event, NextState, NewData, Actions, EnterCall)
			catch
				C:R ->
					S2 = S1#{ data := Data, state := State },
					global_terminate(global_reset(S2), [Event | Events], C, R, erlang:get_stacktrace())
			end;
		false ->
			global_exception(S1, Events, Event, State, Data, Class, Exception, Stacktrace)
	end;
global_exception(S0=#{ queue := [] }, Events, Event, State, Data, Class, Exception, Stacktrace) ->
	S1 = S0#{ data := Data, state := State },
	global_terminate(global_reset(S1), [Event | Events], Class, Exception, Stacktrace).

%% @private
global_lock(S=#{ lock := false }) ->
	S#{ lock := 0 };
global_lock(S=#{ lock := Lock }) when is_integer(Lock) andalso Lock >= 0 ->
	S#{ lock := Lock + 1 }.

%% @private
-spec global_reset(S :: internal_state()) -> NewS :: internal_state().
global_reset(S=#{ queue := nil, module := nil }) ->
	S;
global_reset(S) ->
	S#{ queue := nil, module := nil }.

% %% @private
% -spec global_sync(S :: internal_state(), NextState :: state()) -> NewS :: internal_state().
% global_sync(S=#{ data := #{ '__engine__' := #{ state := NextState } } }, NextState) ->
% 	S;
% global_sync(S0=#{ data := D0=#{ '__engine__' := E0 } }, NextState) ->
% 	E1 = E0#{ state := NextState },
% 	D1 = D0#{ '__engine__' := E1 },
% 	S1 = S0#{ data := D1 }

%% @private
-spec global_terminate(S :: internal_state(), Events :: [internal_event()], Class :: term(), Reason :: term(), Stacktrace :: term()) -> no_return().
global_terminate(S0=#{ modules := Modules, queue := nil }, Events, Class, Reason, Stacktrace) ->
	S1 = S0#{ queue := Modules },
	global_terminate(S1, Events, Class, Reason, Stacktrace);
global_terminate(S0=#{ queue := [Module | Modules], skip := Skip }, Events, Class, Reason, Stacktrace) when Module =:= ?MODULE orelse Module =:= Skip ->
	S1 = S0#{ module := Module, queue := Modules },
	global_terminate(S1, Events, Class, Reason, Stacktrace);
global_terminate(S0=#{ queue := [Module | Modules], state := State, data := Data }, Events, Class, Reason, Stacktrace) ->
	S1 = S0#{ module := Module, queue := Modules },
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, terminate, 3) of
		true ->
			try Module:terminate(Reason, State, Data) of
				_ ->
					global_terminate(S1, Events, Class, Reason, Stacktrace)
			catch
				C:R ->
					ST = erlang:get_stacktrace(),
					erlang:raise(C, R, ST)
			end;
		false ->
			global_terminate(S1, Events, Class, Reason, Stacktrace)
	end;
global_terminate(#{ queue := [] }, _Events, Class, Reason, []) ->
	erlang:Class(Reason);
global_terminate(S0=#{ queue := [], data := #{ '__struct__' := DataType } }, _Events, Class, Exception=#{ '__exception__' := true, '__struct__' := iam_error, data := #{ '__struct__' := DataType } }, Stacktrace) ->
	Data = global_unwrap(S0#{ lock := false }),
	NewException = Exception#{ data := Data },
	erlang:raise(Class, NewException, Stacktrace);
global_terminate(#{ queue := [] }, _Events, Class, Reason, Stacktrace) ->
	erlang:raise(Class, Reason, Stacktrace).

%% @private
global_unlock(S=#{ lock := false }) ->
	S;
global_unlock(S=#{ lock := 0 }) ->
	S#{ lock := false };
global_unlock(S=#{ lock := Lock }) when is_integer(Lock) andalso Lock > 0 ->
	S#{ lock := Lock - 1 }.

%% @private
-spec global_unwrap(S :: internal_state()) -> engine() | internal_state().
% global_unwrap(S=#{ lock := Lock, data := #{ '__engine__' := #{ '__struct__' := ?MODULE, state := State } }, state := State }) when is_integer(Lock) andalso Lock >= 0 ->
% 	S;
% global_unwrap(S0=#{ lock := Lock, data := Data=#{ '__engine__' := Engine=#{ '__struct__' := ?MODULE } }, state := State }) when is_integer(Lock) andalso Lock >= 0 ->
% 	NewEngine = Engine#{ state := State },
% 	NewData = Data#{ '__engine__' := NewEngine },
% 	S1 = S0#{ data := NewData },
% 	S1;
global_unwrap(S=#{ lock := Lock }) when is_integer(Lock) andalso Lock >= 0 ->
	S;
global_unwrap(#{ data := Data=#{ '__engine__' := #{ '__struct__' := ?MODULE, state := State } }, state := State }) ->
	Data;
global_unwrap(#{ data := Data=#{ '__engine__' := Engine=#{ '__struct__' := ?MODULE } }, state := State }) ->
	NewEngine = Engine#{ state := State },
	NewData = Data#{ '__engine__' := NewEngine },
	NewData.

%% @private
-spec global_wrap(Data :: engine()) -> S :: internal_state().
global_wrap(Data=#{ '__engine__' := #{ '__struct__' := ?MODULE, modules := Modules, state := State } }) ->
	#{
		data => Data,
		enter => false,
		halt => false,
		lock => false,
		module => nil,
		modules => Modules,
		queue => nil,
		skip => nil,
		state => State
	}.

%% @private
-spec init(S :: internal_state(), Args :: term()) -> data() | no_return().
init(S=#{ modules := Modules, queue := nil }, Args) ->
	init(S#{ queue := Modules }, Args);
init(S=#{ queue := [Module | Modules] }, Args) ->
	_ = code:ensure_loaded(Module),
	case erlang:function_exported(Module, init, 1) of
		true ->
			try Module:init(Args) of
				Result ->
					init_result(S#{ module := Module, queue := nil }, Result)
			catch
				Class:Reason ->
					Stacktrace = erlang:get_stacktrace(),
					erlang:raise(Class, Reason, Stacktrace)
			end;
		false ->
			init(S#{ queue := Modules }, Args)
	end;
init(#{ queue := [], modules := Modules }, _Args) ->
	Error = {bad_init_state, Modules},
	erlang:exit(Error).

%% @private
-spec init_result(S :: internal_state(), Result :: term()) -> data() | no_return().
init_result(S, Result) ->
	case Result of
		{ok, State, Data} when is_map(Data) ->
			global_enter(S, State, Data, []);
		{ok, State, Data, Actions} when is_map(Data) ->
			global_enter(S, State, Data, Actions);
		{stop, Reason} ->
			erlang:exit(Reason);
		ignore ->
			erlang:exit(normal);
		_ ->
			Error = {bad_return_from_init, Result},
			erlang:exit(Error)
	end.

%% @private
-spec listify(Item :: [term()] | term()) -> [term()].
listify(Item) when is_list(Item) ->
	Item;
listify(Item) ->
	[Item].

%% @private
-spec loop_event(S :: internal_state(), Events :: [internal_event()], Event :: internal_event()) -> Data :: data().
loop_event(S0=#{ data := Data=#{ '__struct__' := DataType }, state := State }, Events, Event={Type, Content}) ->
	case call_state_function(S0, Type, Content, State, Data) of
		{ok, Result} ->
			{S1, NextState, NewData, Actions, EnterCall} =
				parse_event_result(true, S0, Events, Event, State, Data, Result),
			S2 =
				case S1 of
					#{ enter := true } ->
						S1;
					_ ->
						S1#{ enter := EnterCall }
				end,
			loop_event_actions(S2, Events, Event, NextState, NewData, Actions, EnterCall);
		{Class, Exception=#{ '__exception__' := true, '__struct__' := iam_error, data := NewData=#{ '__struct__' := DataType } }, Stacktrace} ->
			global_exception(global_reset(S0), Events, Event, State, NewData, Class, Exception, Stacktrace);
		{Class, Exception=#{ '__exception__' := true }, Stacktrace} ->
			global_exception(global_reset(S0), Events, Event, State, Data, Class, Exception, Stacktrace);
		{Class, Reason, Stacktrace} ->
			global_terminate(global_reset(S0), [Event | Events], Class, Reason, Stacktrace)
	end.

%% @private
-spec loop_event_actions(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), NextState :: state(), NewData :: data(), Actions :: [action()] | action(), EnterCall :: boolean()) -> NewerData :: data().
loop_event_actions(S0=#{ state := State }, Events, Event, NextState, NewData, Actions, EnterCall) ->
	case parse_actions(S0, State, Actions) of
		{ok, NextEventsR} when EnterCall ->
			loop_event_enter(S0, Events, Event, NextState, NewData, NextEventsR);
		{ok, NextEventsR} ->
			loop_event_result(S0, Events, Event, NextState, NewData, NextEventsR);
		{Class, Reason, Stacktrace} ->
			S1 = S0#{ state := NextState, data := NewData },
			global_terminate(global_reset(S1), [Event | Events], Class, Reason, Stacktrace)
	end.

%% @private
-spec loop_event_enter(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), NextState :: state(), NewData :: data(), NextEventsR :: [internal_event()]) -> NewerData :: data().
loop_event_enter(S0=#{ state := State }, Events, Event, NextState, NewData0=#{ '__struct__' := DataType }, NextEventsR) ->
	NewData = sync_data(NewData0, NextState),
	case call_state_function(S0, enter, State, NextState, NewData) of
		{ok, Result} ->
			{S1, NextState, NewerData, Actions, EnterCall} =
				parse_event_result(false, S0, Events, Event, NextState, NewData, Result),
			loop_event_enter_actions(S1, Events, Event, NextState, NewerData, NextEventsR, Actions, EnterCall);
		{Class, Exception=#{ '__exception__' := true, '__struct__' := iam_error, data := NewerData=#{ '__struct__' := DataType } }, Stacktrace} ->
			global_exception(global_reset(S0), Events, Event, State, NewerData, Class, Exception, Stacktrace);
		{Class, Exception=#{ '__exception__' := true }, Stacktrace} ->
			global_exception(global_reset(S0), Events, Event, State, NewData, Class, Exception, Stacktrace);
		{Class, Reason, Stacktrace} ->
			S1 = S0#{ state := NextState, data := NewData },
			global_terminate(global_reset(S1), [Event | Events], Class, Reason, Stacktrace)
	end.

%% @private
-spec loop_event_enter_actions(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), NextState :: state(), NewData :: data(), NextEventsR :: [internal_event()], Actions :: [action()] | action(), EnterCall :: boolean()) -> NewerData :: data().
loop_event_enter_actions(S0, Events, Event, NextState, NewData, NextEventsR, Actions, EnterCall) ->
	case parse_enter_actions(S0, NextState, Actions, NextEventsR) of
		{ok, NewNextEventsR} when EnterCall ->
			loop_event_enter(S0, Events, Event, NextState, NewData, NewNextEventsR);
		{ok, NewNextEventsR} ->
			loop_event_result(S0, Events, Event, NextState, NewData, NewNextEventsR);
		{Class, Reason, Stacktrace} ->
			S1 = S0#{ state := NextState, data := NewData },
			global_terminate(global_reset(S1), [Event | Events], Class, Reason, Stacktrace)
	end.

%% @private
-spec loop_event_result(S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), NextState :: state(), NewData :: data(), NextEventsR :: [internal_event()]) -> NewerData :: data().
loop_event_result(S0, Events0, _Event, NextState, NewData, NextEventsR) ->
	%% Place next events last in reversed queue
	Events1 = lists:reverse(Events0, NextEventsR),
	S1 = S0#{ data := NewData, state := NextState },
	case lists:reverse(Events1) of
		[] ->
			S1;
		[Event | Events] ->
			loop_event(S1, Events, Event)
	end.

%% @private
-spec parse_actions(S :: internal_state(), State :: state(), Actions :: [action()] | action()) -> {ok, [internal_event()]} | {error, term(), term()}.
parse_actions(S, State, Actions) ->
	NextEventsR = [],
	parse_actions(S, State, listify(Actions), NextEventsR).

%% @private
-spec parse_actions(S :: internal_state(), State :: state(), Actions :: [action()] | action(), NextEventsR :: [internal_event()]) -> {ok, [internal_event()]} | {error, term(), term()}.
parse_actions(_S, _State, [], NextEventsR) ->
	{ok, NextEventsR};
parse_actions(S, State, [Action | Actions], NextEventsR) ->
	case Action of
		{next_event, Type, Content} ->
			case event_type(Type) of
				true ->
					parse_actions(S, State, Actions, [{Type, Content} | NextEventsR]);
				_ ->
					{error, {bad_action_from_state_function, Action}, ?STACKTRACE()}
			end;
		_ ->
			{error, {bad_action_from_state_function, Action}, ?STACKTRACE()}
	end.

%% @private
-spec parse_enter_actions(S :: internal_state(), State :: state(), Actions :: [action()] | action(), NextEventsR :: [internal_event()]) -> {ok, [internal_event()]} | {error, term(), term()}.
parse_enter_actions(S, State, Actions, NextEventsR) ->
	parse_actions(S, State, Actions, NextEventsR).

%% @private
-spec parse_event_result(AllowStateChange :: boolean(), S :: internal_state(), Events :: [internal_event()], Event :: internal_event(), State :: state(), Data :: data(), Result :: term()) -> {NewS :: internal_state(), NextState :: state(), NewData :: data(), Actions :: [action()] | action(), EnterCall :: boolean()} | no_return().
parse_event_result(AllowStateChange, S0, Events, Event, State, Data, Result) ->
	case Result of
		stop ->
			S1 = S0#{ state := State, data := Data },
			global_terminate(global_reset(S1), [Event | Events], exit, normal, ?STACKTRACE());
		{stop, Reason} ->
			S1 = S0#{ state := State, data := Data },
			global_terminate(global_reset(S1), [Event | Events], exit, Reason, ?STACKTRACE());
		{stop, Reason, NewData} when is_map(NewData) ->
			S1 = S0#{ state := State, data := NewData },
			global_terminate(global_reset(S1), [Event | Events], exit, Reason, ?STACKTRACE());
		%%
		{next_state, State, NewData} when is_map(NewData) ->
			{S0, State, NewData, [], false};
		{next_state, State, NewData, Actions} when is_map(NewData) ->
			{S0, State, NewData, Actions, false};
		{next_state, NextState, NewData} when AllowStateChange andalso is_map(NewData) ->
			{S0, NextState, NewData, [], true};
		{next_state, NextState, NewData, Actions} when AllowStateChange andalso is_map(NewData) ->
			{S0, NextState, NewData, Actions, true};
		%%
		{keep_state, NewData} when is_map(NewData) ->
			{S0, State, NewData, [], false};
		{keep_state, NewData, Actions} when is_map(NewData) ->
			{S0, State, NewData, Actions, false};
		keep_state_and_data ->
			{S0, State, Data, [], false};
		{keep_state_and_data, Actions} ->
			{S0, State, Data, Actions, false};
		%%
		{repeat_state, NewData} when is_map(NewData) ->
			{S0, State, NewData, [], true};
		{repeat_state, NewData, Actions} when is_map(NewData) ->
			{S0, State, NewData, Actions, true};
		repeat_state_and_data ->
			{S0, State, Data, [], true};
		{repeat_state_and_data, Actions} ->
			{S0, State, Data, Actions, true};
		%%
		{halt_state, NewData} when is_map(NewData) ->
			S1 = S0#{ halt := true },
			{S1, State, NewData, [], false};
		{halt_state, NewData, Actions} when is_map(NewData) ->
			S1 = S0#{ halt := true },
			{S1, State, NewData, Actions, false};
		halt_state_and_data ->
			S1 = S0#{ halt := true },
			{S1, State, Data, [], false};
		{halt_state_and_data, Actions} ->
			S1 = S0#{ halt := true },
			{S1, State, Data, Actions, false};
		%%
		_ ->
			S1 = S0#{ state := State, data := Data },
			global_terminate(global_reset(S1), [Event | Events], error, {bad_return_from_state_function, Result}, ?STACKTRACE())
	end.

%% @private
-spec resolve([module()], #{ module() => [module()] }, #{ module() => [] }, [module()]) -> [module()].
resolve([Module | Modules], Tree, Skip, AccR) ->
	case maps:is_key(Module, Skip) of
		true ->
			resolve(Modules, Tree, Skip, [Module | AccR]);
		false ->
			case maps:find(Module, Tree) of
				{ok, Info} ->
					NewModules = deduplicate(Modules ++ Info),
					resolve(NewModules, Tree, Skip, AccR);
				error ->
					NewInfo = '__resolve__'(Module),
					NewTree = maps:put(Module, NewInfo, Tree),
					NewSkip = maps:put(Module, [], Skip),
					NewModules = deduplicate(Modules ++ NewInfo),
					resolve(NewModules, NewTree, NewSkip, AccR)
			end
	end;
resolve([], _Tree, _Skip, AccR) ->
	deduplicate(lists:reverse(AccR)).

%% @private
-spec sync_data(Data :: engine(), NextState :: state()) -> NewData :: engine().
sync_data(Data=#{ '__engine__' := #{ state := NextState } }, NextState) ->
	Data;
sync_data(Data=#{ '__engine__' := Engine }, NextState) ->
	NewEngine = Engine#{ state := NextState },
	NewData = Data#{ '__engine__' := NewEngine },
	NewData.
