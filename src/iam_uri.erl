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
-module(iam_uri).

%% Types
-type maplike() :: binary() | [{atom() | binary(), iodata()}] | #{(atom() | binary()) => iodata()}.

-type t() :: #{
	'__struct__' := 'Elixir.URI',
	'scheme' := nil | binary(),
	'path' := nil | binary(),
	'query' := nil | binary(),
	'fragment' := nil | binary(),
	'authority' := nil | binary(),
	'userinfo' := nil | binary(),
	'host' := nil | binary(),
	'port' := nil | inet:port_number()
}.

-export_type([t/0]).

-type transform() ::
	fun((binary() | t()) -> t()).

%% API
-export([append_fragment/2]).
-export([append_path/2]).
-export([append_query/2]).
-export([cast_fragment/1]).
-export([cast_path/1]).
-export([cast_query/1]).
-export(['equal?'/2]).
-export(['match?'/2]).
-export(['match?'/3]).
-export([strip/1]).
%% Elixir API
-export(['__struct__'/0]).
-export(['__struct__'/1]).
-export([default_port/1]).
-export([default_port/2]).
-export([encode_query/1]).
-export([decode_query/1]).
-export([decode_query/2]).
-export(['char_reserved?'/1]).
-export(['char_unreserved?'/1]).
-export(['char_unescaped?'/1]).
-export([encode/1]).
-export([encode/2]).
-export([encode_www_form/1]).
-export([decode/1]).
-export([decode_www_form/1]).
-export([parse/1]).
-export([to_string/1]).
-export([merge/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec append_fragment(binary() | t(), nil | binary() | maplike()) -> t().
append_fragment(String, Fragment) when is_binary(String) ->
	append_fragment(parse(String), Fragment);
append_fragment(URI=#{ '__struct__' := 'Elixir.URI', 'fragment' := nil }, Fragment) ->
	URI#{ 'fragment' := cast_fragment(Fragment) };
append_fragment(URI=#{ '__struct__' := 'Elixir.URI', 'fragment' := OldFragment }, Fragment0) ->
	Fragment1 = cast_fragment(Fragment0),
	case Fragment1 of
		nil ->
			URI;
		_ ->
			NewFragment = << OldFragment/binary, $&, Fragment1/binary >>,
			URI#{ 'fragment' := NewFragment }
	end.

-spec append_path(binary() | t(), nil | binary()) -> t().
append_path(String, Path) when is_binary(String) ->
	append_path(parse(String), Path);
append_path(URI=#{ '__struct__' := 'Elixir.URI', path := nil }, Path) ->
	URI#{ path := cast_path(Path) };
append_path(URI=#{ '__struct__' := 'Elixir.URI', path := OldPath }, Path0) ->
	Path1 = cast_path(Path0),
	case Path1 of
		nil ->
			URI;
		_ ->
			#{
				'__struct__' := 'Elixir.URI',
				path := Path,
				'query' := Query,
				fragment := Fragment
			} = parse(Path1),
			NewPath = cast_path(<< (strip_trailing_slash(OldPath))/binary, Path/binary >>),
			append_fragment(append_query(URI#{ path := NewPath }, Query), Fragment)
	end.

-spec append_query(binary() | t(), nil | maplike()) -> t().
append_query(String, Query) when is_binary(String) ->
	append_query(parse(String), Query);
append_query(URI=#{ '__struct__' := 'Elixir.URI', 'query' := nil }, Query) ->
	URI#{ 'query' := cast_query(Query) };
append_query(URI=#{ '__struct__' := 'Elixir.URI', 'query' := OldQuery }, Query0) ->
	Query1 = cast_query(Query0),
	case Query1 of
		nil ->
			URI;
		_ ->
			NewQuery = << OldQuery/binary, $&, Query1/binary >>,
			URI#{ 'query' := NewQuery }
	end.

-spec cast_fragment(nil | binary() | maplike()) -> nil | binary().
cast_fragment(Fragment) when Fragment == nil orelse Fragment == <<>> orelse Fragment == [] orelse Fragment == #{} ->
	nil;
cast_fragment(Fragment) when is_binary(Fragment) ->
	Fragment;
cast_fragment(Fragment) when is_list(Fragment) orelse is_map(Fragment) ->
	encode_query(Fragment).

-spec cast_path(nil | binary()) -> nil | binary().
cast_path(Path) when Path == nil orelse Path == <<>> ->
	nil;
cast_path(Path) when is_binary(Path) ->
	case cast_path(Path, <<>>) of
		<<>> ->
			nil;
		NewPath = << $/, _/binary >> ->
			NewPath;
		NewPath ->
			<< $/, NewPath/binary >>
	end.

%% @private
cast_path(Acc = << $/ >>, <<>>) ->
	Acc;
cast_path(<< $/ >>, Acc) ->
	Acc;
cast_path(<< $/, Rest0/binary >>, Acc) ->
	case strip_leading_slash(Rest0) of
		<<>> ->
			Acc;
		Rest = << $?, _/binary >> ->
			<< Acc/binary, Rest/binary >>;
		Rest ->
			cast_path(Rest, << Acc/binary, $/ >>)
	end;
cast_path(Rest = << $?, _/binary >>, Acc) ->
	<< Acc/binary, Rest/binary >>;
cast_path(<< C, Rest/binary >>, Acc) ->
	cast_path(Rest, << Acc/binary, C >>);
cast_path(<<>>, Acc) ->
	Acc.

-spec cast_query(nil | binary() | maplike()) -> nil | binary().
cast_query(Query) when Query == nil orelse Query == <<>> orelse Query == [] orelse Query == #{} ->
	nil;
cast_query(Query) when is_binary(Query) ->
	Query;
cast_query(Query) when is_list(Query) orelse is_map(Query) ->
	encode_query(Query).

-spec 'equal?'(binary() | t(), binary() | t()) -> boolean().
'equal?'(A, B) ->
	to_string(A) =:= to_string(B).

-spec 'match?'(binary() | t(), [binary() | t()]) -> boolean().
'match?'(URI, List) ->
	'match?'(URI, List, strip).

-spec 'match?'(binary() | t(), [binary() | t()], exact | strip | transform()) -> boolean().
'match?'(URI0, List, Transform) when Transform == exact orelse Transform == strip orelse is_function(Transform, 1) ->
	URI = match_transform(URI0, Transform),
	Predicate = fun(Element) ->
		?MODULE:'equal?'(URI, match_transform(Element, Transform))
	end,
	_ = code:ensure_loaded('Elixir.Enum'),
	case erlang:function_exported('Elixir.Enum', 'any?', 2) of
		false ->
			enum_any(List, Predicate);
		true ->
			'Elixir.Enum':'any?'(List, Predicate)
	end.

%% @private
match_transform(URI, exact) ->
	URI;
match_transform(URI, strip) ->
	strip(URI);
match_transform(URI, Transform) ->
	Transform(URI).

%% @private
enum_any([H | T], Predicate) ->
	case Predicate(H) of
		false ->
			enum_any(T, Predicate);
		true ->
			true
	end;
enum_any([], _Predicate) ->
	false.

-spec strip(binary() | t()) -> t().
strip(URI=#{ '__struct__' := 'Elixir.URI' }) ->
	case URI of
		#{ fragment := nil, 'query' := nil, userinfo := nil, path := nil } ->
			URI;
		#{ fragment := nil, 'query' := nil, userinfo := nil, path := Path } when is_binary(Path) ->
			URI#{ path := strip_trailing_slash(Path) };
		_ ->
			strip(URI#{ fragment := nil, 'query' := nil, userinfo := nil })
	end;
strip(String) when is_binary(String) ->
	strip(parse(String)).

%% @private
strip_leading_slash(<< $/, Rest/binary >>) ->
	strip_leading_slash(Rest);
strip_leading_slash(Rest) ->
	Rest.

%% @private
strip_trailing_slash(Path = <<>>) ->
	Path;
strip_trailing_slash(Path) ->
	case binary:last(Path) of
		$/ ->
			strip_trailing_slash(binary:part(Path, 0, byte_size(Path) - 1));
		_ ->
			Path
	end.

%%%===================================================================
%%% Elixir API functions
%%%===================================================================

'__struct__'() ->
	#{
		'__struct__' => 'Elixir.URI',
		'scheme' => nil,
		'path' => nil,
		'query' => nil,
		'fragment' => nil,
		'authority' => nil,
		'userinfo' => nil,
		'host' => nil,
		'port' => nil
	}.

'__struct__'(List) when is_list(List) ->
	'__struct__'(maps:from_list(List));
'__struct__'(Map) when is_map(Map) ->
	maps:fold(fun maps:update/3, '__struct__'(), Map).

-spec default_port(binary()) -> nil | non_neg_integer().
default_port(Scheme) when is_binary(Scheme) ->
	_ = code:ensure_loaded(elixir_config),
	case erlang:function_exported(elixir_config, get, 1) of
		false ->
			case iam_config:get({uri, Scheme}) of
				nil ->
					case Scheme of
						<<"ftp">> -> 21;
						<<"http">> -> 80;
						<<"https">> -> 443;
						<<"ldap">> -> 389;
						<<"sftp">> -> 22;
						<<"tftp">> -> 69;
						_ -> nil
					end;
				Port ->
					Port
			end;
		true ->
			elixir_config:get({uri, Scheme})
	end.

-spec default_port(binary(), non_neg_integer()) -> ok.
default_port(Scheme, Port) when is_binary(Scheme) andalso is_integer(Port) andalso Port >= 0 ->
	_ = code:ensure_loaded(elixir_config),
	case erlang:function_exported(elixir_config, put, 2) of
		false ->
			iam_config:put({uri, Scheme}, Port);
		true ->
			elixir_config:put({uri, Scheme}, Port)
	end.

-spec encode_query(term()) -> binary().
encode_query(Enumerable) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', encode_query, 1) of
		false ->
			do_encode_query(Enumerable);
		true ->
			'Elixir.URI':encode_query(Enumerable)
	end.

%% @private
do_encode_query(List) when is_list(List) ->
	iam_util:list_join([encode_kv_pair({Key, Value}) || {Key, Value} <- List], $&);
do_encode_query(Map) when is_map(Map) ->
	do_encode_query(lists:sort(maps:to_list(Map))).

%% @private
encode_kv_pair({Key, _}) when is_list(Key) ->
	erlang:error(#{
		'__exception__' => true,
		'__struct__' => 'Elixir.ArgumentError',
		message => iolist_to_binary(io_lib:format("encode_query/1 keys cannot be lists, got: ~p", [Key]))
	});
encode_kv_pair({_, Value}) when is_list(Value) ->
	erlang:error(#{
		'__exception__' => true,
		'__struct__' => 'Elixir.ArgumentError',
		message => iolist_to_binary(io_lib:format("encode_query/1 values cannot be lists, got: ~p", [Value]))
	});
encode_kv_pair({Key, Value}) ->
	<<
		(encode_www_form(any_to_string(Key)))/binary,
		$=,
		(encode_www_form(any_to_string(Value)))/binary
	>>.

%% @private
any_to_string(Binary) when is_binary(Binary) ->
	Binary;
any_to_string(List) when is_list(List) ->
	erlang:iolist_to_binary(List);
any_to_string(Atom) when is_atom(Atom) ->
	erlang:atom_to_binary(Atom, unicode).

-spec decode_query(binary()) -> map().
decode_query(Query) ->
	decode_query(Query, #{}).

-spec decode_query(binary(), map()) -> map().
decode_query(Query, Map) when is_binary(Query) andalso is_map(Map) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', decode_query, 2) of
		false ->
			decode_query_into_map(Query, Map);
		true ->
			'Elixir.URI':decode_query(Query, Map)
	end.

%% @private
decode_query_into_map(Query, Map) ->
	case decode_next_query_pair(Query) of
		nil ->
			Map;
		{{Key, Value}, Rest} ->
			decode_query_into_map(Rest, maps:put(Key, Value, Map))
	end.

%% @private
decode_next_query_pair(<<>>) ->
	nil;
decode_next_query_pair(Query) ->
	{UndecodedNextPair, Rest} =
		case binary:split(Query, << $& >>) of
			[NextPair, NextRest] -> {NextPair, NextRest};
			[NextPair] -> {NextPair, <<>>}
		end,
	DecodedNextPair =
		case binary:split(UndecodedNextPair, << $= >>) of
			[Key, Value] -> {decode_www_form(Key), decode_www_form(Value)};
			[Key] -> {decode_www_form(Key), nil}
		end,
	{DecodedNextPair, Rest}.

-spec 'char_reserved?'(char()) -> boolean().
'char_reserved?'(Char) when Char >= 0 andalso Char =< 16#10FFFF ->
	lists:member(Char, ":/?#[]@!$&'()*+,;=").

-spec 'char_unreserved?'(char()) -> boolean().
'char_unreserved?'(Char) when Char >= 0 andalso Char =< 16#10FFFF ->
	((Char >= $0 andalso Char =< $9)
		orelse (Char >= $a andalso Char =< $z)
		orelse (Char >= $A andalso Char =< $Z)
		orelse lists:member(Char, "~_-.")).

-spec 'char_unescaped?'(char()) -> boolean().
'char_unescaped?'(Char) when Char >= 0 andalso Char =< 16#10FFFF ->
	('char_reserved?'(Char) orelse 'char_unreserved?'(Char)).

-spec encode(binary()) -> binary().
encode(String) ->
	encode(String, fun 'char_unescaped?'/1).

-spec encode(binary(), fun((byte()) -> boolean())) -> binary().
encode(String, Predicate) when is_binary(String) andalso is_function(Predicate, 1) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', encode, 2) of
		false ->
			<< << (percent(Char, Predicate))/binary >> || << Char >> <= String >>;
		true ->
			'Elixir.URI':encode(String, Predicate)
	end.

-spec encode_www_form(binary()) -> binary().
encode_www_form(String) when is_binary(String) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', encode_www_form, 1) of
		false ->
			<<
				<<
					(case percent(Char, fun 'char_unreserved?'/1) of
						<<"%20">> -> <<"+">>;
						Percent -> Percent
					end)/binary
				>> || << Char >> <= String
			>>;
		true ->
			'Elixir.URI':encode_www_form(String)
	end.

%% @private
percent(Char, Predicate) ->
	case Predicate(Char) of
		true ->
			<< Char >>;
		false ->
			<< $%, (hex(Char bsr 4)), (hex(Char band 15)) >>
	end.

%% @private
hex(N) when N =< 9 -> N + $0;
hex(N) -> N + $A - 10.

-spec decode(binary()) -> binary().
decode(URI) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', decode, 1) of
		false ->
			try
				unpercent(URI, <<>>, false)
			catch
				throw:malformed_uri ->
					erlang:error(#{
						'__exception__' => true,
						'__struct__' => 'Elixir.ArgumentError',
						message => iolist_to_binary(io_lib:format("malformed URI ~p", [URI]))
					})
			end;
		true ->
			'Elixir.URI':decode(URI)
	end.

-spec decode_www_form(binary()) -> binary().
decode_www_form(String) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', decode_www_form, 1) of
		false ->
			try
				unpercent(String, <<>>, true)
			catch
				throw:malformed_uri ->
					erlang:error(#{
						'__exception__' => true,
						'__struct__' => 'Elixir.ArgumentError',
						message => iolist_to_binary(io_lib:format("malformed URI ~p", [String]))
					})
			end;
		true ->
			'Elixir.URI':decode_www_form(String)
	end.

%% @private
unpercent(<< $+, Tail/binary >>, Acc, Spaces = true) ->
	unpercent(Tail, << Acc/binary, $\s >>, Spaces);
unpercent(<< $%, Hex1, Hex2, Tail/binary >>, Acc, Spaces) ->
	unpercent(Tail, << Acc/binary, ((hex_to_dec(Hex1) bsl 4) + hex_to_dec(Hex2)) >>, Spaces);
unpercent(<< Head, Tail/binary >>, Acc, Spaces) ->
	unpercent(Tail, << Acc/binary, Head >>, Spaces);
unpercent(<<>>, Acc, _Spaces) ->
	Acc.

%% @private
hex_to_dec(N) when N >= $A andalso N =< $F -> N - $A + 10;
hex_to_dec(N) when N >= $a andalso N =< $f -> N - $a + 10;
hex_to_dec(N) when N >= $0 andalso N =< $9 -> N - $0;
hex_to_dec(_N) -> erlang:throw(malformed_uri).

-spec parse(t() | binary()) -> t().
parse(URI = #{ '__struct__' := 'Elixir.URI' }) ->
	URI;
parse(String) when is_binary(String) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', parse, 1) of
		false ->
			do_parse(String);
		true ->
			'Elixir.URI':parse(String)
	end.

%% @private
destructure_parts([_, _, Scheme]) ->
	{Scheme, nil, nil, nil, nil};
destructure_parts([_, _, Scheme, _, Authority]) ->
	{Scheme, Authority, nil, nil, nil};
destructure_parts([_, _, Scheme, _, Authority, Path]) ->
	{Scheme, Authority, Path, nil, nil};
destructure_parts([_, _, Scheme, _, Authority, Path, _, Query]) ->
	{Scheme, Authority, Path, Query, nil};
destructure_parts([_, _, Scheme, _, Authority, Path, _, Query, _, Fragment | _]) ->
	{Scheme, Authority, Path, Query, Fragment};
destructure_parts(_) ->
	{nil, nil, nil, nil, nil}.

%% @private
do_parse(String) ->
	Regex =
		case re:compile(<<"^(([a-z][a-z0-9\\+\\-\\.]*):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?">>, [caseless]) of
			{ok, Regex0} ->
				Regex0;
			{error, {Reason, At}} ->
				erlang:error(#{
					'__exception__' => true,
					'__struct__' => 'Elixir.Regex.CompileError',
					message => iolist_to_binary(io_lib:format("~s at position ~p", [Reason, At]))
				})
		end,
	{match, Parts} = nillify(re:run(String, Regex, [{capture, all, binary}])),
	{MatchScheme, Authority, Path, Query, Fragment} = destructure_parts(Parts),
	{Userinfo, Host, MatchPort} = split_authority(Authority),
	Scheme =
		case MatchScheme of
			nil ->
				nil;
			_ ->
				list_to_binary(string:to_lower(binary_to_list(MatchScheme)))
		end,
	Port =
		case MatchPort of
			nil ->
				case Scheme of
					nil ->
						nil;
					_ ->
						default_port(Scheme)
				end;
			_ ->
				MatchPort
		end,
	'__struct__'(#{
		scheme => Scheme,
		path => Path,
		'query' => Query,
		fragment => Fragment,
		authority => Authority,
		userinfo => Userinfo,
		host => Host,
		port => Port
	}).

%% @private
destructure_components([_, _, Userinfo]) ->
	{Userinfo, nil, nil};
destructure_components([_, _, Userinfo, Host]) ->
	{Userinfo, Host, nil};
destructure_components([_, _, Userinfo, Host, _, Port | _]) ->
	{Userinfo, Host, Port};
destructure_components(_) ->
	{nil, nil, nil}.

%% @private
split_authority(String) ->
	Regex =
		case re:compile(<<"(^(.*)@)?(\\[[a-zA-Z0-9:.]*\\]|[^:]*)(:(\\d*))?">>, [caseless]) of
			{ok, Regex0} ->
				Regex0;
			{error, {Reason, At}} ->
				erlang:error(#{
					'__exception__' => true,
					'__struct__' => 'Elixir.Regex.CompileError',
					message => iolist_to_binary(io_lib:format("~s at position ~p", [Reason, At]))
				})
		end,
	{match, Components} = nillify(re:run(case String of nil -> <<>>; _ -> String end, Regex, [{capture, all, binary}])),
	{Userinfo, MatchHost, MatchPort} = destructure_components(Components),
	Host =
		case MatchHost of
			nil ->
				nil;
			_ ->
				trim_trailing(trim_leading(MatchHost, $[), $])
		end,
	Port =
		case MatchPort of
			nil ->
				nil;
			_ ->
				binary_to_integer(MatchPort)
		end,
	{Userinfo, Host, Port}.

%% @private
nillify({match, List}) ->
	{match, [begin
		case byte_size(String) of
			0 ->
				nil;
			_ ->
				String
		end
	end || String <- List]}.

%% @private
trim_leading(<< C, Rest/binary >>, C) ->
	trim_leading(Rest, C);
trim_leading(Rest, _C) ->
	Rest.

%% @private
trim_trailing(<<>>, _C) ->
	<<>>;
trim_trailing(Rest, C) ->
	case binary:last(Rest) of
		C ->
			trim_trailing(binary:part(Rest, 0, byte_size(Rest) - 1), C);
		_ ->
			Rest
	end.

-spec to_string(t()) -> binary().
to_string(URI=#{ '__struct__' := 'Elixir.URI' }) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', to_string, 1) of
		false ->
			do_to_string(URI);
		true ->
			'Elixir.URI':to_string(URI)
	end.

%% @private
do_to_string(URI0=#{
	scheme := Scheme, port := Port, path := Path, 'query' := Query,
	fragment := Fragment
}) ->
	URI =
		case Scheme of
			nil ->
				URI0;
			_ ->
				case default_port(Scheme) of
					Port ->
						URI0#{ port := nil };
					_ ->
						URI0
				end
		end,
	Authority = extract_authority(URI),
	<<
		(case Scheme of nil -> <<>>; _ -> << Scheme/binary, $: >> end)/binary,
		(case Authority of nil -> <<>>; _ -> << $/, $/, Authority/binary >> end)/binary,
		(case Path of nil -> <<>>; _ -> Path end)/binary,
		(case Query of nil -> <<>>; _ -> << $?, Query/binary >> end)/binary,
		(case Fragment of nil -> <<>>; _ -> << $#, Fragment/binary >> end)/binary
	>>;
do_to_string(Any) ->
	any_to_string(Any).

%% @private
extract_authority(#{ host := nil, authority := Authority }) ->
	Authority;
extract_authority(#{ host := Host, userinfo := Userinfo, port := Port }) ->
	<<
		(case Userinfo of nil -> <<>>; _ -> << Userinfo/binary, $@ >> end)/binary,
		(case binary:match(Host, << $: >>) of nomatch -> Host; _ -> << $[, Host/binary, $] >> end)/binary,
		(case Port of nil -> <<>>; _ -> << $:, (integer_to_binary(Port))/binary >> end)/binary
	>>.

-spec merge(t() | binary(), t() | binary()) -> t().
merge(URI, Rel) ->
	_ = code:ensure_loaded('Elixir.URI'),
	case erlang:function_exported('Elixir.URI', merge, 2) of
		false ->
			do_merge(URI, Rel);
		true ->
			'Elixir.URI':merge(URI, Rel)
	end.

%% @private
do_merge(#{ '__struct__' := 'Elixir.URI', authority := nil }, _Rel) ->
	erlang:error(#{
		'__exception__' => true,
		'__struct__' => 'Elixir.ArgumentError',
		message => <<"you must merge onto an absolute URI">>
	});
do_merge(_base, Rel=#{ '__struct__' := 'Elixir.URI', scheme := RelScheme }) when RelScheme =/= nil ->
	Rel;
do_merge(#{ '__struct__' := 'Elixir.URI', scheme := Scheme }, Rel=#{ '__struct__' := 'Elixir.URI', authority := Authority }) when Authority =/= nil ->
	Rel#{ scheme := Scheme };
do_merge(Base=#{ '__struct__' := 'Elixir.URI', 'query' := BaseQuery }, #{ '__struct__' := 'Elixir.URI', path := RelPath, 'query' := RelQuery, fragment := RelFragment }) when RelPath == <<>> orelse RelPath == nil ->
	Query =
		case RelQuery of
			nil ->
				BaseQuery;
			_ ->
				RelQuery
		end,
	Base#{ 'query' := Query, fragment := RelFragment };
do_merge(Base=#{ '__struct__' := 'Elixir.URI', path := BasePath }, #{ '__struct__' := 'Elixir.URI', path := RelPath, 'query' := RelQuery, fragment := RelFragment }) ->
	NewPath = merge_paths(BasePath, RelPath),
	Base#{ path := NewPath, 'query' := RelQuery, fragment := RelFragment };
do_merge(Base, Rel) ->
	do_merge(parse(Base), parse(Rel)).

%% @private
merge_paths(nil, RelPath) ->
	merge_paths(<<"/">>, RelPath);
merge_paths(_, RelPath = << $/, _/binary >>) ->
	RelPath;
merge_paths(BasePath, RelPath) ->
	[_ | BaseSegments] = path_to_segments(BasePath),
	iam_util:list_join(remove_dot_segments(path_to_segments(RelPath) ++ BaseSegments, []), $/).

%% @private
remove_dot_segments([], [Head, <<"..">> | Acc]) ->
	remove_dot_segments([], [Head | Acc]);
remove_dot_segments([], Acc) ->
	Acc;
remove_dot_segments([<<".">> | Tail], Acc) ->
	remove_dot_segments(Tail, Acc);
remove_dot_segments([Head | Tail], [<<"..">>, <<"..">> | _] = Acc) ->
	remove_dot_segments(Tail, [Head | Acc]);
remove_dot_segments(Segments, [_, <<"..">> | Acc]) ->
	remove_dot_segments(Segments, Acc);
remove_dot_segments([Head | Tail], Acc) ->
	remove_dot_segments(Tail, [Head | Acc]).

%% @private
path_to_segments(Path) ->
	[Head | Tail] = binary:split(Path, $/),
	reverse_and_discard_empty(Tail, [Head]).

%% @private
reverse_and_discard_empty([], Acc) ->
	Acc;
reverse_and_discard_empty([Head], Acc) ->
	[Head | Acc];
reverse_and_discard_empty([<<>> | Tail], Acc) ->
	reverse_and_discard_empty(Tail, Acc);
reverse_and_discard_empty([Head | Tail], Acc) ->
	reverse_and_discard_empty(Tail, [Head | Acc]).
