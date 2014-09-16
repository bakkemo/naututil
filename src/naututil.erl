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

%%
%% plenty of other things don't on bitstrings
%%
desure(X) ->
    case erlang:is_bitstring(X) of
        true -> erlang:binary_to_list(X);
        _    -> X
    end.

%%
%% We only want to "decode" the body if it's JSON
%%
decode(Headers, Body) ->
    case is_json(Headers) of
        true    -> jsxn:decode(Body);
        false   -> Body
    end.


%%
%% Environment variable to set with the nautilus authentication
%% endpoint
%%
env() ->
    ?NAUT_AUTH_URI.


%%
%% fold function that strips off a layer of a map
%%
ix(Key, Acc) ->
    V = maps:get(Key, Acc, undefined),
    V.


%%
%% fold ix over a list of map keys to "follow"
%%
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


%%
%% use the default user/password
%%
basic_auth_header() ->
    basic_auth_header(?BASIC_USER, ?BASIC_PASS).


%%
%% comment all the things!
%%
bearer_auth_header(Token) ->
{"Authorization", "Bearer " ++ desure(Token)}.


%%
%% search for "application/json" in the header
%%
is_json(Header) ->
    case re:run(proplists:get_value("content-type", Header, ""),  "application/json") of
        {match, _ } -> true;
        _           -> false
    end.


%%
%% create portal id for request header
%%
portalid_auth_header(Id) ->
{"X-Portal-Id", desure(Id)}.



%%
%% Create User
%%
%% Code dealing with user creation
%%

%%
%% create_user_(Name, Email, Password)
%% 
%% create a new user in the system. Username is currently ingnored
%%
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


%%
%% wrapper to alert on unset variables
%%
%%
create_user(User, Email, Password) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> create_user_(User, Email, Password)
    end.


%%
%% Code for user access token management 
%%
%%

%%
%% wrapper to alert on unset variables
%%
%%
get_token(User, Password) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> get_token_(User, Password)
    end.


%%
%% get an portal access token (for a user) valid till next token request
%% or password change
%%
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

%%
%% assumes standard response Map.
%%
%%
token(Response) ->
    desure(index(Response, [body, <<"access_token">>])).




%%
%% Create a portal. Given our access token, create a portal to named Service and
%% Route. 
%%

%%
%% wrapper to alert on unset variables
%%
%%
create_portal(Token, Service, Route) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> create_portal_(Token, Service, Route, 0)
    end.

create_portal(Token, Service, Route, TTL) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> create_portal_(Token, Service, Route, TTL)
    end.


%%
%% Create a portal. Given our access token, create a portal to named Service and
%% route. will return portal ID, plus the portal handler uri
%%
create_portal_(Token, Service, Route, TTL) ->
    Method  = post,
    Header  = [bearer_auth_header(Token)],
    Type    = "application/octet-stream",
    
    S = case TTL of
        0 -> "";
        _ -> io_lib:format("?ttl=~p",[TTL])
    end,
    
    URL     = string:strip(get_config_value([]), right, $/)  ++ 
              "/" ++ string:strip(desure(Service), both, $/) ++
              "/" ++ string:strip(desure(Route), both, $/) ++ S,
    
    io:format("URL: ~s~n",[URL]),
    Body = "",
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header, Type, Body}, [], Options),
    io:format("sent request~n",[]),
    %R.
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    #{returncode => ReturnCode, state => State, headers => Head, body => ?MODULE:decode(Head, RBody)}.


portalid(Response) ->
    desure(index(Response, [body, <<"headers">>, <<"X-Portal-Id">>])).

portaluri(Response) ->
    desure(index(Response, [body, <<"uri">>])).



get_portal(Uri, Id) ->
    Method=get,
    Header = [portalid_auth_header(Id)],
    URL = Uri, 
    io:format("URL: ~s~n",[URL]),
    Options = [{body_format,binary}],
    R = httpc:request(Method, {URL, Header}, [], Options),
    io:format("sent request~n",[]),
    %R.
    {ok, {{"HTTP/1.1",ReturnCode, State}, Head, RBody}} = R,
    #{returncode => ReturnCode, state => State, headers => Head, body => ?MODULE:decode(Head, RBody)}.


%%
%% wrapper to alert on unset variables
%%
%%
post_portal(Uri, Id) ->
    case get_config_value(none) of
        none -> io:format("~nno authentication uri set in environment~nset it via ~s~n~n",[?NAUT_AUTH_URI]);
        _   -> post_portal_(Uri, Id)
    end.


%%
%% currently acts like a get
%%
%%
post_portal_(Uri, Id) ->
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


%%
%% bind a rest Hostname rest endpoint to a ServiceName
%%
%%
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



