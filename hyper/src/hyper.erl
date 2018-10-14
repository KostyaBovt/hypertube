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
         update_email/1, update_email/3]).

-import(hyper_lib, [gv/2, gv/3]).

-define(PHOTOS_PATH, "/static/photos/").



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
        null -> {ok, <<"not found">>}
    end.

-spec get_profile(UId::binary()) -> hyper_http:handler_ret().
get_profile(UId) ->
    case hyper_db:get_user_by_id(UId) of
        {ok, User} ->
            {ok, maps:with([<<"uname">>, <<"fname">>, <<"lname">>, <<"bio">>, <<"locale">>, <<"avatar">>, <<"email">>],
                           prefix_avatar_path(User))};
        null -> {ok, <<"not found">>}
    end.

-spec prefix_avatar_path(Data::map()) -> map().
prefix_avatar_path(Data) ->
    maps:update_with(<<"avatar">>, fun(P) when is_binary(P) -> <<?PHOTOS_PATH, P/binary>> end, Data).

-spec update_profile(UId::non_neg_integer(), Uname::binary() | null, Fname::binary() | null, Lname::binary() | null,
                     Bio::binary() | null, Avatar::binary() | null) ->
    hyper_http:handler_ret().
update_profile(_, null, null, null, null, null) -> ok;
update_profile(UId, Uname, Fname, Lname, Bio, Avatar) ->
    case process_photo(Avatar) of
        {ok, PhotoName} ->
            {ok, _} =  hyper_db:update_user(UId, Uname, Fname, Lname, Bio, PhotoName);
        _ ->
            {error, #{<<"avatar">> => <<"invalid format">>}}
    end.

-spec process_photo(Photo::binary() | null) -> {ok, binary()} | error.
process_photo(null) -> null;
process_photo(Photo0) ->
    case binary:split(Photo0, <<"base64,">>) of
        [_, Photo1] ->
            case eimp:convert(base64:decode(Photo1), jpeg) of
                {ok, Converted} ->
                    Name = <<(hyper_lib:md5(Converted))/binary, ".jpeg">>,
                    Path = <<"priv/static/photos/", Name/binary>>,
                    case filelib:is_regular(Path) of
                        true -> ok;
                        false -> file:write_file(Path, Converted)
                    end,
                    {ok, Name};
                _ -> error
            end;
        _ -> error
    end.

-spec update_pass(UId::non_neg_integer(), OldPass::binary(), NewPass::binary()) ->  hyper_http:handler_ret().
update_pass(UId, OldPass, NewPass) ->
    {ok, #{<<"password">> := PassHash}} = hyper_db:get_user_by_id(UId),
    case erlpass:change(OldPass, PassHash, NewPass, 12) of
        {error, bad_password} -> {error, #{<<"old_password">> => incorrect}};
        NewPassHash ->
            true = hyper_db:update_user_password(UId, NewPassHash),
            ok
    end.

-spec update_locale(UData::map(), Locale::binary()) -> hyper_http:handler_ret().
update_locale(#{<<"loc">> := Locale}, Locale) -> ok;
update_locale(#{<<"iss">> := UId} = UData, NewLocale) ->
    true = hyper_db:update_locale(UId, NewLocale),
    hyper_auth:update_locale(UData, NewLocale).

-spec create_comment(UId::non_neg_integer(), ImdbId::binary(), Text::binary()) -> hyper_http:handler_ret().
create_comment(UId, ImdbId, Text) ->
    true = hyper_db:create_comment(UId, ImdbId, Text),
    ok.

-spec update_email(Qs::proplists:proplist()) -> hyper_http:handler_ret().
update_email(Qs) ->
    Token = gv(<<"token">>, Qs),
    case hyper_mnesia:get_email_updating_state(Token) of
        #email_updating_state{uid = UId, email = Email} ->
            hyper_db:update_user_email(UId, Email),
            hyper_mnesia:delete_email_updating_state(Token),
            {redirect, <<"/">>};
        null ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.

-spec update_email(Email::binary(), UId::non_neg_integer(), BaseUrl::binary()) ->
    hyper_http:handler_ret().
update_email(UId, Email, BaseUrl) ->
    {ok, #{<<"uname">> := Uname}} = hyper_db:get_user_by_id(UId),
    Token = hyper_lib:rand_str(16),
    hyper_mnesia:create_email_updating_state(UId, Email, Token),
    Url = <<BaseUrl/binary, "/api/profile/email/confirm?token=", Token/binary>>,
    {ok, _} = erlydtl:compile_file("priv/templates/email_updating.html", email_updating),
    {ok, Body} = email_updating:render([{username, Uname}, {link, Url}, {out_dir, "priv/templates"}]),
    <<_/binary>> = hyper_lib:send_email(<<"HyperTube -- email changing">>, iolist_to_binary(Body), Email),
    ok.