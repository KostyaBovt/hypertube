-module(hyper).
-author("aklimchu").

-include("hyper.hrl").

%% API
-export([start/0]).

-export([get_user/1,
         get_profile/1,
         update_profile/6,
         update_pass/3,
         update_locale/2,
         create_comment/3,
         update_email/1, update_email/3,
         get_comments/1,
         prefix_avatar_path/1,
         import_social_avatar/1]).

-import(hyper_lib, [gv/2, gv/3]).

-define(PHOTOS_PATH, "/static/photos/").

%% API

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(hyper),
    ok.

-spec get_user(Uname::binary()) -> hyper_http:handler_ret().
get_user(Uname) ->
    case hyper_db:get_user_by_uname(Uname) of
        {ok, User} ->
            {ok, maps:with([<<"uname">>, <<"fname">>, <<"lname">>, <<"bio">>, <<"locale">>, <<"avatar">>],
                            prefix_avatar_path(User))};
        null -> {error, 404}
    end.

-spec get_profile(UId::binary()) -> hyper_http:handler_ret().
get_profile(UId) ->
    case hyper_db:get_user_by_id(UId) of
        {ok, User} ->
            {ok, maps:with([<<"uname">>, <<"fname">>, <<"lname">>, <<"bio">>, <<"locale">>, <<"avatar">>,
                            <<"email">>, <<"social_provider">>], prefix_avatar_path(User))};
        null -> {error, 404}
    end.

-spec update_profile(UId::non_neg_integer(), Uname::binary() | null, Fname::binary() | null, Lname::binary() | null,
                     Bio::binary() | null, Avatar::binary() | null) ->
    hyper_http:handler_ret().
update_profile(_, null, null, null, null, null) -> ok;
update_profile(UId, Uname, Fname, Lname, Bio, <<_:1/binary, _/binary>> = Avatar0) ->
    case binary:split(Avatar0, <<"base64,">>) of
        [_, Avatar1] ->
            case process_photo(base64:decode(Avatar1)) of
                {ok, PhotoName} ->
                    {ok, U} = hyper_db:update_user(UId, Uname, Fname, Lname, Bio, PhotoName),
					{ok, prefix_avatar_path(U)};
                _ ->
                    {error, #{<<"avatar">> => <<"invalid format">>}}
            end;
        _ ->
            {error, #{<<"avatar">> => <<"invalid format">>}}
    end;
update_profile(UId, Uname, Fname, Lname, Bio, Avatar) ->
    hyper_db:update_user(UId, Uname, Fname, Lname, Bio, Avatar).

-spec update_pass(UId::non_neg_integer(), OldPass::binary(), NewPass::binary()) ->  hyper_http:handler_ret().
update_pass(UId, OldPass, NewPass) ->
    case hyper_db:get_user_by_id(UId) of
	{ok, #{<<"social_provider">> := null, <<"password">> := PassHash}} ->
    	    case erlpass:change(OldPass, PassHash, NewPass, 12) of
        	{error, bad_password} -> {error, #{<<"old_password">> => <<"incorrect">>}};
        	NewPassHash ->
            	    true = hyper_db:update_user_password(UId, NewPassHash),
            	    ok
    	    end;
	{ok, _} -> {error, <<"social_account">>}
    end.

-spec update_locale(UData::map(), Locale::binary()) -> hyper_http:handler_ret().
update_locale(#{<<"loc">> := Locale}, Locale) -> ok;
update_locale(#{<<"iss">> := UId} = UData, NewLocale) ->
    true = hyper_db:update_user_locale(UId, NewLocale),
    hyper_auth:update_token_locale(UData, NewLocale).

-spec create_comment(UId::non_neg_integer(), ImdbId::binary(), Text::binary()) -> hyper_http:handler_ret().
create_comment(UId, ImdbId, Text) ->
    {ok, C} = hyper_db:create_comment(UId, ImdbId, Text),
    {ok, prefix_avatar_path(C)}.

-spec get_comments(Qs::proplists:proplist()) -> hyper_http:handler_ret().
get_comments(Qs) ->
    {ok, Comments} = hyper_db:get_comments(gv(<<"id">>, Qs)),
    {ok, [prefix_avatar_path(C) || C <- Comments]}.

-spec update_email(Qs::proplists:proplist()) -> hyper_http:handler_ret().
update_email(Qs) ->
    Token = gv(<<"token">>, Qs),
    case hyper_mnesia:get_email_updating_state(Token) of
        #email_updating_state{uid = UId, email = Email} ->
            hyper_db:update_user_email(UId, Email),
            hyper_mnesia:delete_email_updating_state(Token),
            {redirect, <<?FRONTEND_ORIGIN, "/profile">>};
        null ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.

-spec update_email(UId::non_neg_integer(), Email::binary(), BaseUrl::binary()) -> ok.
update_email(UId, Email, BaseUrl) ->
    {ok, #{<<"uname">> := Uname}} = hyper_db:get_user_by_id(UId),
    Token = hyper_lib:rand_str(16),
    hyper_mnesia:create_email_updating_state(UId, Email, Token),
    Url = <<BaseUrl/binary, "/api/profile/email/confirm?token=", Token/binary>>,
    {ok, _} = erlydtl:compile_file("priv/templates/email_updating.html", email_updating),
    {ok, Body} = email_updating:render([{username, Uname}, {link, Url}, {out_dir, "priv/templates"}]),
    <<_/binary>> = hyper_lib:send_email(<<"HyperTube -- email changing">>, iolist_to_binary(Body), Email),
    ok.

-spec import_social_avatar(UId::non_neg_integer()) -> {ok, map()} | {error, binary()}.
import_social_avatar(UId) ->
     case hyper_db:get_user_by_id(UId) of
         {ok, #{<<"social_provider">> := Provider, <<"social_token">> := Token}} when Provider =/= null ->
            case hyper_oauth2:get_profile_info(Provider, Token) of
                {profile, P} ->
                    case gv(<<"picture">>, P) of
                        <<AvatarUrl/binary>> -> update_profile_with_social_avatar(UId, AvatarUrl);
                        null -> {error, <<"not available">>}
                    end;
                E ->
                    io:format("hyper_oauth2:get_profile_info(~p, ~p) error: ~p~n", [Provider, Token, E]),
                    {error, <<"not available">>}
            end;
         {ok, _} ->
             {error, <<"not social account">>}
     end.

-spec prefix_avatar_path(Data::map()) -> map().
prefix_avatar_path(Data) ->
    maps:update_with(<<"avatar">>, fun(<<_:1/binary, _/binary>> = P) -> <<?PHOTOS_PATH, P/binary>>; (E) -> E end, Data).

%% Inner

-spec update_profile_with_social_avatar(UId::non_neg_integer(), AvatarUrl::binary()) -> {ok, map()} | {error, binary()}.
update_profile_with_social_avatar(UId, AvatarUrl) ->
    case hyper_lib:get_img_by_url(AvatarUrl) of
        {ok, Body} ->
            case process_photo(Body) of
                {ok, PhotoName} ->
                    {ok, User} = hyper_db:update_user(UId, null, null, null, null, PhotoName),
                    {ok, #{<<"avatar">> => gv(<<"avatar">>, prefix_avatar_path(User))}};
                error ->
                    {error, <<"not available">>}
            end;
        {error, _} ->
            {error, <<"not available">>}
    end.


-spec process_photo(Photo::binary() | null) -> {ok, Name::binary()} | error.
process_photo(null) -> null;
process_photo(Photo) ->
    case eimp:convert(Photo, jpeg) of
        {ok, Converted} ->
            Name = <<(hyper_lib:md5(Converted))/binary, ".jpeg">>,
            Path = <<"priv", ?PHOTOS_PATH, Name/binary>>,
            case filelib:is_regular(Path) of
                true -> ok;
                false -> file:write_file(Path, Converted)
            end,
            {ok, Name};
        E ->
            io:format("Format convertion failed: ~p~n", [E]),
            error
    end.
