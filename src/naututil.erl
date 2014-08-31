%%
%% erlang module for playing around with nautilus
%% it is expected that the auth endpoint is set via NAUT_AUTH_URI environment variable
%%

-module(naututil).
-compile(export_all).
        
-define(NAUT_AUTH_URI, "NAUT_AUTH_URI").
-define(USER_ROUTE, "/user").
-define(TOKEN_ROUTE, "/token").
-define(ACCESS_ROUTE, "/httpbin").
-define(PORTAL_ROUTE, "/portal").
-define(REGISTER_SERVICE_ROUTE, "/service").
-define(BASIC_USER, "foo").
-define(BASIC_PASS, "bar").


start() ->
    inets:start(), 
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> ok
    end.


get_config_value(Default) ->
    case os:getenv(?NAUT_AUTH_URI) of
	    false -> Default;    
        Value -> Value
    end.

%%
%% just in case you forgot to set it in your os shell
%%
set_auth_uri(Auth_uri) ->
    os:putenv(?NAUT_AUTH_URI, Auth_uri).


%%
%% every json library in existence works only on bitstrings
%%
ensure(X) ->
    case not erlang:is_bitstring(X) of
        true -> erlang:list_to_binary(X);
        _    -> X
    end.

desure(X) ->
    case erlang:is_bitstring(X) of
        true -> erlang:binary_to_list(X);
        _    -> X
    end.

decode(Headers, Body) ->
    case is_json(Headers) of
        true    -> jsxn:decode(Body);
        false   -> Body
    end.


uri() ->
    ?NAUT_AUTH_URI.

ix(Key, Acc) ->
    V = maps:get(Key, Acc, undefined),
    %io:format("key: ~p~nacc: ~p~nval: ~p~n~n~n",[Key, Acc, V]),
    V.

index(Map, Indexes) ->
    lists:foldl(fun ?MODULE:ix/2, Map, Indexes).

%
% lets roll our own url encoding utiliy
%
url_encode(Data) ->
    url_encode(Data,"").

url_encode([],Acc) ->
    Acc;
url_encode([{Key,Value}|R],"") ->
    url_encode(R, edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value));
url_encode([{Key,Value}|R],Acc) ->
    url_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(Key) ++ "=" ++ edoc_lib:escape_uri(Value)).

%
% I suppose equivalents live in some http utils lib somewhere
%
basic_auth_header(User, Pass) ->
    Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
    {"Authorization","Basic " ++ Encoded}.

basic_auth_header() ->
    basic_auth_header(?BASIC_USER, ?BASIC_PASS).

bearer_auth_header(Token) ->
{"Authorization", "Bearer " ++ desure(Token)}.

is_json(Header) ->
    case re:run(proplists:get_value("content-type", Header, ""),  "application/json") of
        {match, _ } -> true;
        _           -> false
    end.

portalid_auth_header(Id) ->
{"X-Portal-Id", desure(Id)}.

create_user_(N, E, P) ->
    Name = ensure(N),
    Email = ensure(E),
    Password = ensure(P),
    Header = [],
    Body = jsxn:encode([{<<"email">>, Email}, {<<"name">>, Name}, {<<"password">>, Password}]),
    Method = post,    
    URL = string:strip(get_config_value([]), right, $/) ++ ?USER_ROUTE,
    Type = "application/json",
    HTTPOptions = [],
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    {ReturnCode, State, ?MODULE:decode(Head, RBody)}.



create_user(User, Email, Password) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> create_user_(User, Email, Password)
    end.

get_token(User, Password) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> get_token_(User, Password)
    end.


get_token_(User, Password) ->
    Method=post,
    Header = [basic_auth_header()],
    Type = "application/x-www-form-urlencoded",
    URL = string:strip(get_config_value([]), right, $/) ++ ?TOKEN_ROUTE,
    Body = url_encode([{"username",User},{"password",Password}, {"grant_type","password"}]),
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header, Type, Body}, [], Options),
    %R.
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    #{returncode => ReturnCode, state => State, headers => Head, body => ?MODULE:decode(Head, RBody)}.


create_portal(Token, Service, Route) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> create_portal_(Token, Service, Route)
    end.

create_portal_(Token, Service, Route) ->
    Method  = post,
    Header  = [bearer_auth_header(Token)],
    Type    = "application/octet-stream",
    URL     = string:strip(get_config_value([]), right, $/)  ++ 
              "/" ++ string:strip(desure(Service), both, $/) ++
              "/" ++ string:strip(desure(Route), both, $/),
    
    io:format("URL: ~s~n",[URL]),
    Body = "",
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header, Type, Body}, [], Options),
    io:format("sent request~n",[]),
    %R.
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    #{returncode => ReturnCode, state => State, headers => Head, body => ?MODULE:decode(Head, RBody)}.


get_portal(Uri, Id) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> get_portal_(Uri, Id)
    end.

get_portal_(Uri, Id) ->
    Method=post,
    Header = [portalid_auth_header(Id)],
    Type = "application/octet-stream",
    URL = Uri, 
    io:format("URL: ~s~n",[URL]),
    Body = "",
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header, Type, Body}, [], Options),
    io:format("sent request~n",[]),
    %R.
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    #{returncode => ReturnCode, state => State, headers => Head, body => ?MODULE:decode(Head, RBody)}.


register_service(ServiceName, Hostname) ->
    Method=post,
    Header = [basic_auth_header()],
    Type = "application/json",
    URL = string:strip(get_config_value([]), right, $/) ++ ?REGISTER_SERVICE_ROUTE,
    io:format("URL: ~s~n",[URL]),
    Body = jsxn:encode(#{<<"service">> => ensure(ServiceName), <<"host">> => ensure(Hostname)}),
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header, Type, Body}, [], Options),
    io:format("sent request~n",[]),
    %Body.
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    #{returncode => ReturnCode, state => State, headers => Head, body => ?MODULE:decode(Head, RBody)}.



