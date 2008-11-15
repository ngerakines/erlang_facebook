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
%% * v0.2 2008-11-15: ngerakines
%%   - Substantial code rewrite and reorg.
%%   - Move away from the gen_server model.
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.2
%% @doc A simple Facebook Platform API interface in Erlang.
%% @todo Add support for more Facebook Platform API methods.
-module(erlang_facebook).
-author("Nick Gerakines <nick@gerakines.net>").
-version("0.2").

-export([ %% API exports
	application_getpublicinfo/3,
	feed_publishuseraction/3,
	profile_getinfo/3,
	profile_setfbml/3,
	profile_setinfo/3,
	users_hasapppermission/3,
	users_isappuser/3,
	custom/4
]).

-export([ %% utility exports
	facebook_fun/1,
	validate_args/3
]).

-define(USER_AGENT, "erlang_facebook/0.2").

%% @private
%% @doc Returns the Request URL for the Facebook API.
build_url(Args) ->
    QueryString = build_querystring(Args),
    lists:concat(["http://api.facebook.com/restserver.php", QueryString]).

%% @private
%% @doc Returns a default list of Args used by the Facebook API.
build_args(Args) -> [
        {"call_id", integer_to_list(epochnow())},
        {"v", "1.0"},
        {"format", "JSON"} | Args
    ].

raw_request(Type, URI, Body) ->
    {ok, Socket} = gen_tcp:connect("api.facebook.com", 80, [binary, {active, false}, {packet, 0}]),
    Req = build_request(Type, URI, Body),
    gen_tcp:send(Socket, Req),
    {ok, Resp} = do_recv(Socket, []),
    gen_tcp:close(Socket),
    {ok,_, ResponseBody} = erlang:decode_packet(http, Resp, []),
    parse_json(parse_response(ResponseBody)).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs | B]);
        {error, closed} ->
            {ok, erlang:iolist_to_binary(Bs)}
    end.

parse_response(<<13,10,13,10,Data/binary>>) -> binary_to_list(Data);
parse_response(<<_X:1/binary,Data/binary>>) -> parse_response(Data).

build_request(Type, URI, []) ->
    list_to_binary(lists:concat([Type, " ", URI, " HTTP/1.0\r\nContent-Type: application/json\r\n\r\n"]));

build_request(Type, URI, Body) ->
    erlang:iolist_to_binary([
        lists:concat([Type, " ", URI, " HTTP/1.0\r\n"
            "Content-Length: ", erlang:iolist_size(Body), "\r\n"
            "Content-Type: application/json\r\n\r\n"
        ]),
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
    PreHash = lists:concat([[begin
        Value = dict:fetch(Key, Dict),
        lists:concat([Key, "=", mochiweb_util:quote_plus(Value)])
    end || Key <- Keys], [Secret]]),
    hashme(PreHash).

%% @private
%% @doc Create a md5 hash from a string.
hashme(List) ->
	Data = binary_to_list(erlang:md5(List)),
	lists:flatten([io_lib:format("~.16b", [X]) || X <- Data]).

%% @private
%% @doc Return the current time as a unix epoch timestamp.
epochnow() ->
    calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

%% @private
%% @doc Prepare an API request.
prepare_request(ApiKey, Secret, Method, Args) ->
	CoreArgs = build_args([{"method", Method}, {"api_key", ApiKey} | Args]),
    Sig = create_signature(dict:from_list(CoreArgs), Secret),
    Url = build_url([{"sig", Sig} | CoreArgs]),
	raw_request("GET", Url, []).

%% @doc Create an application.getPublicInfo API request.
application_getpublicinfo(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.application.getPublicInfo", Args).

%% @doc Create a profile.getInfo API request.
profile_getinfo(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.profile.getInfo", Args).

%% @doc Create a feed.publishUserAction API request.
feed_publishuseraction(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.feed.publishUserAction", Args).

%% @doc Create a user.hasAppPermission API request.
users_hasapppermission(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.users.hasAppPermission", Args).

%% @doc Create a users.isAppUser API request.
users_isappuser(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.users.isAppUser", Args).

%% @doc Create a profile.setFBML API request.
profile_setfbml(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.profile.setFBML", Args).

%% @doc Create a profile.setInfo API request.
profile_setinfo(ApiKey, Secret, Args) ->
	prepare_request(ApiKey, Secret, "facebook.profile.setInfo", Args).

%% @doc Create a custom API request.
custom(ApiKey, Secret, Method, Args) ->
	prepare_request(ApiKey, Secret, Method, Args).

%% @private
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
build_querystring([], Acc) -> Acc;
build_querystring([{Key, Value} | Tail], []) ->
    Acc = lists:concat(["?", Key, "=", mochiweb_util:quote_plus(Value)]),
    build_querystring(Tail, Acc);
build_querystring([{Key, Value} | Tail], Acc) ->
    NewAcc = lists:concat([Acc, "&", Key, "=", mochiweb_util:quote_plus(Value)]),
    build_querystring(Tail, NewAcc).

%% @doc Create a function representing a Facebook request's data.
facebook_fun(Args) ->
    FBArgs = collect_fb_args(Args, []),
    facebook_fun(FBArgs, 1).

%% @private
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
