-module(naututil_tests).
%-define(NOTEST, 1).
-include_lib("eunit/include/eunit.hrl").

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    ChrsSize = size(Chrs),
    F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).

random_email() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random_string(10) ++ "@tld.com".


test_user(UserName, Email, Password) ->
    {RC, S, _B} = naututil:create_user(UserName, Email, Password),     
    ?assertEqual(201, RC),
    ?assertEqual("Created", S).
    

name_test() ->
    inets:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    N = random_email(),
    
    % we expect this user creation to work
    {RC, S, _B} = naututil:create_user("foo", N, "password"),     
    ?assertEqual(201, RC),
    ?assertEqual("Created", S),
    % trying to create the same user is expected to fail
    
    {RC2, S2, B2} = naututil:create_user("foo", N, "password"), 
    ?assertEqual(400, RC2),
    ?assertEqual("Bad Request", S2),
    ?assertEqual(<<"User exists">>, naututil:index(B2, [<<"error_decription">>])),
    N.

service_test() ->
    UserName = name_test(),
    io:format("~p~n", [UserName]),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    SrvName = random_string(5),
    
    % first service creation should work
    R = naututil:register_service(SrvName,  "http://httpbin.com"), 
    ?assertEqual(201, naututil:index(R, [returncode])),
    ?assertEqual("Created", naututil:index(R, [state])),

    % 2nd service creation should fail
    R2 = naututil:register_service(SrvName,  "http://httpbin.com"), 
    ?assertEqual(400, naututil:index(R2, [returncode])),
    ?assertEqual("Bad Request", naututil:index(R2, [state])),
    ?assertEqual(<<"invalid_request">>, naututil:index(R2, [body, <<"error">>])),
    ?assertEqual(<<"Service exists">>, naututil:index(R2, [body, <<"error_decription">>])),
    {UserName, SrvName}.

portal_test() ->
    % get a newly created email and registered service pointing at httpbin 
    {Email, ServiceName} = service_test(),
    
    
    TR = naututil:get_token(Email, "password"),
    ?assertEqual(201, naututil:index(TR, [returncode])),
    ?assertEqual("Created", naututil:index(TR, [state])),
    ?assertEqual(<<"Bearer">>, naututil:index(TR, [body, <<"token_type">>])),
    
    % lets get an access token and get a portal 
    Tok = naututil:index(TR, [body, <<"access_token">>]),
    PR = naututil:create_portal(Tok,ServiceName,"get",0),
    % PR.    
    ?assertEqual(201, naututil:index(TR, [returncode])),
    ?assertEqual("Created", naututil:index(TR, [state])),
    
    % portal id, uri   
    PID = naututil:desure(naututil:index(PR, [body, <<"headers">>, <<"X-Portal-Id">>])),
    PUri = naututil:desure(naututil:index(PR, [body, <<"uri">>])),
    
    % get from the portal where we know something in the body
    GR = naututil:get_portal(PUri, PID),
    ?assertEqual(200, naututil:index(GR, [returncode])),
    ?assertEqual("OK", naututil:index(GR, [state])),
    Body = naututil:desure(naututil:index(GR, [body])),
	?assert(re:run(Body,  "http://httpbin.com:80/get") /=  nomatch),	
    	
    % get a new token    
    TR2 = naututil:get_token(Email, "password"),
    ?assertEqual(201, naututil:index(TR2, [returncode])),
    ?assertEqual("Created", naututil:index(TR2, [state])),
    ?assertEqual(<<"Bearer">>, naututil:index(TR2, [body, <<"token_type">>])).
    %Tok2
