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
%% Change Log:
%% * 2009-02-04 ngerakines
%%   - Added fql_query/3
%% * 2009-01-01 ngerakines
%%   - Updated call_id generation function
%% * 2008-12-13 ngerakines
%%   - Added more documentation
%%   - Updated version references throughout module
%% * v0.3.2 ngerakines
%%   - Bug fix release
%% * v0.3 ngerakines
%%   - Added additional modules, moved completely away form all yaws deps
%% * v0.2 2008-11-15: ngerakines
%%   - Substantial code rewrite and reorg.
%%   - Move away from the gen_server model.
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.3.2
%% @todo Add support for more Facebook Platform API methods.
%% @doc A simple Facebook Platform API interface in Erlang.
%% This module requires Erlang/OTP 12-5.
-module(erlang_facebook).
-author("Nick Gerakines <nick@gerakines.net>").
-version("0.3.2").

-export([ %% API exports
	application_getpublicinfo/3,
	fbml_refreshrefurl/3,
	fql_query/3,
	users_hasapppermission/3,
	users_isappuser/3,
	users_setstatus/3,
	users_getinfo/3,
    users_getloggedinuser/3,
	custom/4
]).

-export([ %% API Deprecated exports
    feed_publishuseraction/3,
    profile_getinfo/3,
    profile_setfbml/3,
    profile_setinfo/3,
    profile_setinfooptions/3   
]).

-export([ %% utility exports
    facebook_fun/1,
    validate_args/3
]).

-define(USER_AGENT, "erlang_facebook/0.3.2").

%% @private
%% @doc Returns the Request URL for the Facebook API.
build_url(Args) ->
    QueryString = build_querystring(Args),
    erlang:list_to_binary(["/restserver.php", QueryString]).

%% @private
%% @doc Returns a default list of Args used by the Facebook API.
build_args(Args) -> [
        {"call_id", integer_to_list(utime())},
        {"v", "1.0"},
        {"format", "JSON"} | Args
    ].

%% @private
%% @doc Create an HTTP request to the Facebook Platform. This is done through
%% a raw gen_tcp connection instead of inets to be fask and light weight.
%% Please be aware that the erlang:decode_packet/3 function call is only
%% available in 12-5 and later.
raw_request(Type, URI, Body) ->
    {ok, Socket} = gen_tcp:connect("api.facebook.com", 80, [binary, {active, false}, {packet, 0}]),
    Req = build_request(Type, URI, Body),
    gen_tcp:send(Socket, Req),
    {ok, Resp} = do_recv(Socket, []),
    gen_tcp:close(Socket),
    case erlang:decode_packet(http, Resp, []) of
        {ok, _, ResponseBody} ->
            parse_json(parse_response(ResponseBody));
        _ ->
            {error, parse_error}
    end.

%% @private
do_recv(Socket, Bs) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, B} ->
            do_recv(Socket, [Bs | B]);
        _ ->
            {ok, erlang:iolist_to_binary(Bs)}
    end.

%% @private
%% @doc Parse the response from the Facebook Platform, disregarding
%% everything but the actual body/payload of the response.
parse_response(<<13,10,13,10,Data/binary>>) -> binary_to_list(Data);
parse_response(<<_X:1/binary,Data/binary>>) -> parse_response(Data).

%% @private
%% @doc Create an HTTP 1.0 valid request for a given method, uri and body.
build_request(Type, URI, []) ->
    erlang:iolist_to_binary([
        Type, " ", URI, " HTTP/1.0\r\n"
        "Content-Type: application/json\r\n\r\n"
    ]);

build_request(Type, URI, Body) ->
    erlang:iolist_to_binary([
        Type, " ", URI, " HTTP/1.0\r\n"
        "Content-Length: ", erlang:iolist_size(Body), "\r\n"
        "Content-Type: application/json\r\n\r\n",
        Body
    ]).


%% @private
%% @doc Parse some json or return an error.
parse_json(Body) ->
    try mochijson2:decode(Body) of
        Response -> Response
    catch
        _:_ -> {error, parse_error}
    end.

%% @private
%% @doc Create a signature from a dicationary of parameters.
%% @todo Get rid of the dict dependancy.
create_signature(Dict, Secret) ->
    Keys = lists:sort(dict:fetch_keys(Dict)),
    PreHash = erlang:iolist_to_binary([begin
        Value = dict:fetch(Key, Dict),
        [Key, "=", Value]
    end || Key <- Keys] ++ [Secret]),
    hashme(PreHash).

%% @private
%% @doc Create a md5 hash from a string.
hashme(List) ->
    Md5_list = binary_to_list(erlang:md5(List)),
    lists:flatten(list_to_hex(Md5_list)).

%% @private
list_to_hex(L) -> lists:map(fun(X) -> int_to_hex(X) end, L).

%% @private
int_to_hex(N) when N < 256 -> [hex(N div 16), hex(N rem 16)].

%% @private
hex(N) when N < 10 -> $0 + N;
hex(N) when N >= 10, N < 16 -> $a + (N - 10).

%% @private
%% @doc Return a unique incrementing integer.
utime() ->
    {Mes, S, Mis} = erlang:now(), Mes * S + Mis.

%% @private
%% @doc Prepare an API request. At this point the request signature is
%% created and the raw HTTP request is sent and processed.
prepare_request(ApiKey, Secret, Method, Args) ->
    CoreArgs = build_args([{"method", Method}, {"api_key", ApiKey} | Args]),
    Sig = create_signature(dict:from_list(CoreArgs), Secret),
    Url = build_url([{"sig", Sig} | CoreArgs]),
    raw_request("GET", Url, []).

%% @doc Create an application.getPublicInfo API request.
application_getpublicinfo(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.application.getPublicInfo", Args).

%% @doc Deprecated. Create a profile.getInfo API request.
profile_getinfo(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.profile.getInfo", Args).

%% @doc Deprecated. Create a feed.publishUserAction API request.
feed_publishuseraction(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.feed.publishUserAction", Args).

%% @doc Create a user.hasAppPermission API request.
users_hasapppermission(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.users.hasAppPermission", Args).

%% @doc Create a users.isAppUser API request.
users_isappuser(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.users.isAppUser", Args).

%% @doc Create a users.setStatus API request.
users_setstatus(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.users.setStatus", Args).

%% @doc Create a users.getInfo API request.
users_getinfo(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.users.getInfo", Args).

%% @doc Create a users.getLoggedInUser request
users_getloggedinuser(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.users.getLoggedInUser", Args).

%% @doc Deprecated. Create a profile.setFBML API request.
profile_setfbml(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.profile.setFBML", Args).

%% @doc Deprecated. Create a profile.setFBML API request.
fbml_refreshrefurl(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.fbml.refreshRefUrl", Args).

%% @doc Create a fql.query API request.
fql_query(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.fql.query", Args).

%% @doc Deprecated. Create a profile.setInfo API request.
profile_setinfo(ApiKey, Secret, Args) ->
    prepare_request(ApiKey, Secret, "facebook.profile.setInfo", Args).

%% @doc Deprecated. Create a profile.setInfoOptions request.
profile_setinfooptions(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.profile.setInfoOptions", Args).

%% @doc Create a custom API request.
custom(ApiKey, Secret, Method, Args) ->
    prepare_request(ApiKey, Secret, Method, Args).

%% @doc Validate a set of parameters against a signature.
validate_args(Secret, Args, Sig) ->
    FBArgs = collect_fb_args(Args, []),
    NewSig = create_signature(dict:from_list(FBArgs), Secret),
    Sig == NewSig.

%% @private 
%% @doc Parse Facebook specific parameters from a list of parameters.
collect_fb_args([], Acc) -> Acc;
collect_fb_args([{"fb_sig_" ++ Key, Value} | Args], Acc) ->
    collect_fb_args(Args, [{Key, Value} | Acc]);
collect_fb_args([{"fb_sig", Value} | Args], Acc) ->
    collect_fb_args(Args, [{"fb_sig", Value} | Acc]);
collect_fb_args([_ | Args], Acc) ->
    collect_fb_args(Args, Acc).

%% @private
%% @doc Parse fb_sig_* variables from a Facebook Platform request.
from_args("fb_sig", Args) ->
    case lists:keysearch("fb_sig", 1, Args) of
        {value, {"fb_sig", Value}} -> Value;
        _ -> none
    end;
from_args(Key, Args) ->
    case lists:keysearch(Key, 1, Args) of
        {value, {_, Value}} -> Value;
        _ -> none
    end.

%% @private
build_querystring(List) -> build_querystring(List, []).

%% @private
build_querystring(Args, _) -> 
    lists:concat(["?", mochiweb_util:urlencode(Args)]).

%% @doc Create a function representing a Facebook request's data.
facebook_fun(Args) ->
    FBArgs = collect_fb_args(Args, []),
    facebook_fun(FBArgs, 1).

%% @doc Returns an anonymous function that represents several important
%% variables from a Facebook Platform request. At this point the only
%% variables returned include 'in_canvas' (bool), 'sig' (string),
%% 'friends' (list of strings), 'added' (bool), 'session' (string),
%% and 'user' (string).
facebook_fun(Args, 1) ->
    fun (in_canvas) ->
            case from_args("in_canvas", Args) of none -> false; _ -> true end;
        (sig) ->
            case from_args("fb_sig", Args) of none -> ""; Sig -> Sig end;
        (friends) ->
            case from_args("friends", Args) of none -> []; List -> string:tokens(List, ",") end;
        (added) ->
            case from_args("added", Args) of none -> false; _ -> true end;
        (session) ->
            from_args("session_key", Args);
        (user) ->
            case from_args("user", Args) of
                none -> from_args("canvas_user", Args);
                CU -> CU
            end
    end.
