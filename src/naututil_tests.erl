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
    %?assertEqual(201, RC),
    %?assertEqual("Created", S).
    {RC,S}.

name_test() ->
    inets:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    N = random_email(),
    
    % we expect this user creation to work
    {RC, S, _B} = naututil:create_user("foo", N, "password"),     
    ?assertEqual({201, "Created"}, {RC,S}),
    % trying to create the same user is expected to fail
    
    {RC2, S2, B2} = naututil:create_user("foo", N, "password"), 
    ?assertEqual(400, RC2),
    ?assertEqual({400, "Bad Request"}, {RC2,S2}),
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
    ?assertEqual(201, naututil:index(PR, [returncode])),
    ?assertEqual("Created", naututil:index(PR, [state])),
    
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
    ?assertEqual(<<"Bearer">>, naututil:index(TR2, [body, <<"token_type">>])),
    Tok2 = naututil:index(TR2, [body, <<"access_token">>]),

    % assignment of a new token means the old one should be invalid

    PR2 = naututil:create_portal(Tok,ServiceName,"get",0),
    ?assertEqual(401, naututil:index(PR2, [returncode])),
    ?assertEqual("Unauthorized", naututil:index(PR2, [state])),
    ?assertEqual(<<"access_denied">>, naututil:index(PR2, [body, <<"error">>])),
    ?assertEqual(<<"Invalid Bearer token">>, naututil:index(PR2, [body, <<"error_decription">>])),

    % yet the previous portal should work till it expires
    
    GR2 = naututil:get_portal(PUri, PID),
    ?assertEqual(200, naututil:index(GR2, [returncode])),
    ?assertEqual("OK", naututil:index(GR2, [state])),
    Body2 = naututil:desure(naututil:index(GR2, [body])),
    ?assert(re:run(Body2,  "http://httpbin.com:80/get") /=  nomatch),
    
    % now a portal with short expiry (10 secs)

    PR3 = naututil:create_portal(Tok2,ServiceName,"get",250),
    ?assertEqual(201, naututil:index(PR3, [returncode])),
    ?assertEqual("Created", naututil:index(PR3, [state])),
    
    PID2  = naututil:desure(naututil:index(PR3, [body, <<"headers">>, <<"X-Portal-Id">>])),
    PUri2 = naututil:desure(naututil:index(PR3, [body, <<"uri">>])),
    
    % should still be a good portal
    GR3 = naututil:get_portal(PUri2, PID2),
   
    ?assertEqual(200, naututil:index(GR3, [returncode])),
    ?assertEqual("OK", naututil:index(GR3, [state])),
    Body3 = naututil:desure(naututil:index(GR3, [body])),
    ?assert(re:run(Body3,  "http://httpbin.com:80/get") /=  nomatch),
    
    timer:sleep(250),
    		
    GR4 = naututil:get_portal(PUri2, PID2),
    ?assertEqual(410, naututil:index(GR4, [returncode])),
    ?assertEqual("Gone", naututil:index(GR4, [state])),
    ?assertEqual(<<"invalid_request">>, naututil:index(GR4, [body, <<"error">>])),
    ?assertEqual(<<"Expired portal">>, naututil:index(GR4, [body, <<"error_decription">>])).
    


