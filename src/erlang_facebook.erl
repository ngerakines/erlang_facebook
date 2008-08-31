%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.1r1
%% @doc A simple Facebook Platform API interface in Erlang.
%% @todo Add support for more Facebook Platform API methods.
%% @todo Documentation.
%% @todo Add xml response parsing support.
%% @todo Export fewer things.
%% @todo Sprinkle better error handling through module.
-module(erlang_facebook).
-behaviour(gen_server).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.1r1").

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-compile(export_all).

-record(erlang_facebook, {api_key, secret, lastcall}).

-define(USER_AGENT, "erlang_facebook/0.1").

start() ->
    inets:start(),
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

info() ->
    gen_server:call({global, ?MODULE}, {info}, infinity).

update(ApiKey, Secret) ->
    gen_server:call({global, ?MODULE}, {set_application, ApiKey, Secret}, infinity).

call(Method, Args) ->
    gen_server:call({global, ?MODULE}, {Method, Args}, infinity).

init(_) ->
    {ok, #erlang_facebook{
        api_key = "",
        secret = "",
        lastcall = 0
    }}.

handle_call({info}, _From, State) ->
    {reply, State, State};

handle_call({set_application, API, Secret}, _From, State) ->
    {reply, ok, State#erlang_facebook{ api_key = API, secret = Secret }};

handle_call({Method, Args}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    MethodArgs = [State#erlang_facebook.secret, [{"api_key", State#erlang_facebook.api_key} | Args]],
    Response = try apply(erlang_facebook, Method, MethodArgs)
    catch
        _X:_Y ->
            {error, unsupported_method}
    end,
    {reply, Response, State#erlang_facebook{ lastcall = Now }};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ---

application_getpublicinfo(Secret, Args) ->
    CoreArgs = build_args([{"method", "facebook.application.getPublicInfo"} | Args]),
    Sig = create_signature(dict:from_list(CoreArgs), Secret),
    Url = build_url([{"sig", Sig} | CoreArgs]),
    submit_request(Url, fun parse_json/1).

profile_getinfo(Secret, Args) ->
    CoreArgs = build_args([{"method", "facebook.profile.getInfo"} | Args]),
    Sig = create_signature(dict:from_list(CoreArgs), Secret),
    Url = build_url([{"sig", Sig} | CoreArgs]),
    submit_request(Url, fun parse_json/1).

users_isappuser(Secret, Args) ->
    CoreArgs = build_args([{"method", "facebook.users.isAppUser"} | Args]),
    Sig = create_signature(dict:from_list(CoreArgs), Secret),
    Url = build_url([{"sig", Sig} | CoreArgs]),
    submit_request(Url, fun parse_json/1).

custom(Secret, Args) ->
    CoreArgs = build_args(Args),
    Sig = create_signature(dict:from_list(CoreArgs), Secret),
    Url = build_url([{"sig", Sig} | CoreArgs]),
    submit_request(Url, fun parse_json/1).

%% ---
validate_args(Secret, Args, Sig) ->
    FBArgs = collect_fb_args(Args, []),
    NewSig = create_signature(dict:from_list(FBArgs), Secret),
    Sig == NewSig.

collect_fb_args([], Acc) -> Acc;
collect_fb_args([{"fb_sig_" ++ Key, Value} | Args], Acc) ->
    collect_fb_args(Args, [{Key, Value} | Acc]);
collect_fb_args([_ | Args], Acc) ->
    collect_fb_args(Args, Acc).

from_args("fb_sig", Args) ->
    case lists:keysearch("fb_sig", 1, Args) of
        {value, {"fb_sig", Value}} -> Value;
        _ -> none
    end;
from_args(Key, Args) ->
    case lists:keysearch("fb_sig_" ++ Key, 1, Args) of
        {value, {_, Value}} -> Value;
        _ -> none
    end.
    

%% ---
build_url(Args) ->
    QueryString = build_querystring(Args),
    lists:concat(["http://api.facebook.com/restserver.php", QueryString]).

build_args(Args) -> [
        {"call_id", integer_to_list(s3now())},
        {"v", "1.0"},
        {"format", "JSON"} | Args
    ].

submit_request(Url, ParseFun) ->
    try http:request(get, {Url, [{"User-Agent", ?USER_AGENT}]}, [], []) of
        {ok, {_Status, _Headers, Body}} -> ParseFun(Body);
        F -> {error, F}
    catch
        _:_ -> {error, something_caught}
    end.

parse_json(Body) ->
    case rfc4627:decode(Body) of
        {ok, Response, _} -> Response;
        _ -> {error, parse_error}
    end.

parse_xml(_Body) -> {error, unimplemented}.

create_signature(Dict, Secret) ->
    Keys = lists:sort(dict:fetch_keys(Dict)),
    PreHash = lists:concat([[begin
        Value = dict:fetch(Key, Dict),
        lists:concat([Key, "=", yaws_api:url_encode(Value)])
    end || Key <- Keys], [Secret]]),
    hashme(PreHash).

hashme(List) ->
    BinMd5 = binary_to_list( erlang:md5(List) ),
    Md5 = lists:flatten(io_lib:format("~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b~2.16.0b", BinMd5)),
    Md5.

s3now() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

build_querystring(List) -> build_querystring(List, []).

build_querystring([], Acc) -> Acc;
build_querystring([{Key, Value} | Tail], []) ->
    Acc = lists:concat(["?", Key, "=", yaws_api:url_encode(Value)]),
    build_querystring(Tail, Acc);
build_querystring([{Key, Value} | Tail], Acc) ->
    NewAcc = lists:concat([Acc, "&", Key, "=", yaws_api:url_encode(Value)]),
    build_querystring(Tail, NewAcc).

facebook_fun(Args) ->
    FBArgs = collect_fb_args(Args, []),
    facebook_fun(FBArgs, 1).

facebook_fun(Args, 1) ->
    fun (in_canvas) ->
            case from_args("in_canvas", Args) of none -> false; _ -> true end;
        (sig) ->
            case from_args("fb_sig", Args) of none -> ""; Sig -> Sig end;
        (friends) ->
            case from_args("friends", Args) of none -> []; List -> string:tokens(List, ",") end;
        (added) ->
            case from_args("added", Args) of none -> false; _ -> true end;
        (user) ->
            case from_args("user", Args) of
                none -> from_args("canvas_user", Args);
                CU -> CU
            end
    end.
