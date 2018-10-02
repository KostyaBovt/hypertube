-module(matcha_oauth2).
-author("aklimchu").

-export([dispatch/4,
         get_github_user_email/1]).

-import(matcha_lib, [gv/2, gv/3]).

-type network() :: {Name::binary(), Opts::proplists:proplist()}.

-define(NETWORKS,
    [{<<"google">>,  % https://code.google.com/apis/console/b/0/
     [{client_id, <<"40383793141-uoavqdrld3rv30rprodrt43doc3k7bf6.apps.googleusercontent.com">>},
      {client_secret, <<"mS6TNyW761oUxQvkAcDiSLTE">>},
      {callback_uri, <<"/api/auth/google/callback">>},
      {scope, <<"https://www.googleapis.com/auth/userinfo.email ",
                "https://www.googleapis.com/auth/userinfo.profile" >>},
      {authorize_uri, <<"https://accounts.google.com/o/oauth2/auth">>},
      {token_uri, "https://accounts.google.com/o/oauth2/token"},
      {user_profile_url_composer,
       fun(AccessToken) ->
           <<"https://www.googleapis.com/oauth2/v1/userinfo?access_token=", AccessToken/binary>>
       end},
      {fields, [<<"id">>, <<"email">>, <<"name">>, <<"picture">>, <<"gender">>, <<"locale">>]}
    ]},
    {<<"facebook">>, % https://developers.facebook.com/apps/
     [{client_id, <<"">>},
      {client_secret, <<"">>},
      {callback_uri, <<"/api/auth/facebook/callback">>},
      {scope, <<"email">>},
      {authorize_uri, <<"https://www.facebook.com/dialog/oauth">>},
      {token_uri, "https://graph.facebook.com/oauth/access_token"},
      {user_profile_url_composer,
       fun(AccessToken) ->
           <<"https://graph.facebook.com/me?access_token=", AccessToken/binary,
             "&fields=id,email,name,picture,gender,locale">>
       end},
      {fields,
       [<<"id">>, <<"email">>, <<"name">>,
        {<<"picture">>, fun(Val) -> gv(<<"url">>, gv(<<"data">>, Val)) end},
        <<"gender">>, <<"locale">>]}
     ]},
    {<<"github">>, [ % https://github.com/settings/developers
      {client_id, <<"59532d0aa06ab6e47e75">>},
      {client_secret, <<"72592639546cdd3c47ec112b489a4b130b025062">>},
      {callback_uri, <<"/api/auth/github/callback">>},
      {scope, <<"user">>},
      {authorize_uri, <<"https://github.com/login/oauth/authorize">>},
      {token_uri, "https://github.com/login/oauth/access_token"},
      {user_profile_url_composer,
       fun(AccessToken) ->
           <<"https://api.github.com/user?access_token=", AccessToken/binary>>
       end},
      {fields, [<<"id">>, <<"email">>, <<"name">>, <<"avatar_url">>, <<"gender">>, <<"locale">>]}
    ]}]).

%% API

-spec dispatch(LocalUrlPrefix::binary(), NetName::binary(), Action::binary(), Qs::binary()) ->
    {redirect, binary()} | {send_html, binary()} | {profile, list()} | {error, term()}.
dispatch(LocalUrlPrefix, NetName, Action, Qs) ->
    Gets = parse_gets(Qs),
    case lists:keyfind(NetName, 1, ?NETWORKS) of
        false -> {error, unknown_network};
        Network when Action =:= <<"login">> -> compose_redirect_uri(LocalUrlPrefix, Gets, Network);
        Network when Action =:= <<"callback">> ->
            case gv(<<"error">>, Gets) of
                null ->
                    case gv(<<"code">>, Gets) of
                        null -> check_access_token(Gets, Network);
                        Code -> get_access_token(Code, LocalUrlPrefix, Network)
                    end;
                Error -> {error, Error}
            end;
        _ -> {error, unknow_action}
    end.

%% INNER

-spec compose_redirect_uri(LocalUrlPrefix::binary(), Gets::list(), network()) -> {redirect, binary()}.
compose_redirect_uri(LocalUrlPrefix, Gets, {_, NetOpts}) ->
    Path = gv(authorize_uri, NetOpts),
    Qs = qs_encode([{client_id, gv(client_id, NetOpts)},
                    {redirect_uri, <<LocalUrlPrefix/binary, (gv(callback_uri, NetOpts))/binary>>},
                    {response_type, gv(<<"response_type">>, Gets, <<"code">>)},
                    {scope, gv(scope, NetOpts)}, {state, gv(<<"state">>, Gets, <<>>)}]),
    {redirect, <<Path/binary, "?", Qs/binary>>}.

-spec check_access_token(Gets::list(), Network::network()) ->
    {send_html, binary()} | {profile, list()} | {error, term()}.
check_access_token(Gets, Network) ->
    case gv(<<"access_token">>, Gets) of
        null ->
            {send_html, <<"<!--script>",
                          "window.location.replace(window.location.href.replace('#','?'))",
                          "</script-->">>};
        Token -> get_profile_info(Network, Token, Gets)
    end.

-spec get_access_token(Code::binary(), CallbackUriPrefix::binary(), Network::network()) ->
    {profile, list()} | {error, term()}.
get_access_token(Code, CallbackUriPrefix, {_, NetOpts} = Network) ->
    Qs = qs_encode([{code, Code},
                    {client_id, gv(client_id, NetOpts)},
                    {client_secret, gv(client_secret, NetOpts)},
                    {redirect_uri, <<CallbackUriPrefix/binary, (gv(callback_uri, NetOpts))/binary>>},
                    {grant_type, <<"authorization_code">>}]),
    Request = {gv(token_uri, NetOpts), [{"User-Agent", ""}], "application/x-www-form-urlencoded", Qs},
    http_request_json(post, Request,
        fun(Response) ->
            case gv(<<"access_token">>, Response) of
                null -> {error, gv(<<"error">>, Response, token_missed)};
                Token -> get_profile_info(Network, Token, Response)
            end
        end).

-spec get_profile_info(Network::network(), AccessToken::binary(), Auth::list()) ->
    {profile, list()} | {error, term()}.
get_profile_info({NetName, NetOpts}, AccessToken, Auth) ->
    UrlComposerFun = gv(user_profile_url_composer, NetOpts) ,
    Url = binary_to_list(UrlComposerFun(AccessToken)),
    OnSuccessFun =
        fun(Profile0) ->
            lager:info("Profile: ~p", [Profile0]),
            Fields = lists:zip([<<"id">>, <<"email">>, <<"name">>, <<"picture">>,
                                <<"gender">>, <<"locale">>], gv(fields, NetOpts)),
            Profile1 =
                [case Field of
                     {N, {PField, Fun}} -> {N, Fun(gv(PField, Profile0))};
                     {N, PField} -> {N, gv(PField, Profile0)}
                 end || Field <- Fields],
            Profile2 = Auth ++ [{<<"provider">>, NetName} | Profile1],
            {profile, Profile2}
        end,
    http_request_json(get, {Url, [{"User-Agent", ""}]}, OnSuccessFun).

-spec parse_gets(binary()) -> list().
parse_gets(<<>>) -> [];
parse_gets(GetString) ->
    [{list_to_binary(K), list_to_binary(V)}
     || {K, V} <- httpd:parse_query(binary_to_list(GetString))].

-spec http_request_json(Method::atom(), Request::tuple(), OnSuccess::fun((binary()) -> any())) ->
    any() | {error, any()}.
http_request_json(Method, Request, OnSuccess) ->
    case httpc:request(Method, Request,
                       [{timeout, 10000}, {connect_timeout, 20000}, {autoredirect, true}],
                       [{body_format, binary}, {full_result, false}]) of
        {ok, {200, Body}} ->
            case parse_body(Body) of
                {error, _} = E -> E;
                L -> OnSuccess(L)
            end;
        {ok, {Code, _}} -> {error, {bad_response_code, Code}};
        {error, Reason} -> {error, Reason}
    end.

-spec parse_body(Body::binary()) -> list() | {error, term()}.
parse_body(Body) ->
    try jsone:decode(Body, [{object_format, proplist}]) catch
        error:badarg ->
            Parsed = parse_gets(Body),
            case lists:all(fun({K, V}) -> K =/= <<>> andalso V =/= <<>> end, Parsed) of
                true -> Parsed;
                false -> {error, {invalid_body, Body}}
            end
    end.

-spec qs_encode(Data::list()) -> binary().
qs_encode(Data) -> qs_encode(Data, "").

-spec qs_encode(Data::list(), Acc::list()) -> binary().
qs_encode([], Acc) -> list_to_binary(Acc);
qs_encode([{Key,Value}|R], "") ->
    qs_encode(R, edoc_lib:escape_uri(atom_to_list(Key)) ++ "=" ++
        edoc_lib:escape_uri(binary_to_list(Value)));
qs_encode([{Key,Value}|R], Acc) ->
    qs_encode(R, Acc ++ "&" ++ edoc_lib:escape_uri(atom_to_list(Key)) ++ "=" ++
        edoc_lib:escape_uri(binary_to_list(Value))).

%% ADDITIONAL

-spec get_github_user_email(Profile::map() | proplists:proplist()) ->
    binary() | null | {error, term()}.
get_github_user_email(Profile) ->
    AuthHeader = {"Authorization", "token " ++ binary_to_list(gv(<<"access_token">>, Profile))},
    http_request_json(get, {"https://api.github.com/user/emails", [{"User-Agent", "" }, AuthHeader]},
                      fun(Response) -> gv(<<"email">>, hd(Response), null) end).
