-module(matcha_user).
-author("aklimchu").

%% API
-export([get_profile/2,
         get_profiles/2,
         get_visit_history/1,
         get_likes/1,
         get_chats/1,
         get_chat_history/2,
         update_profile/2,
         update_password/2,
         update_email/1, update_email/3,
         like/2, unlike/2,
         rate/2,
         block/2, unblock/2,
         report/2,
         send/2]).

-include("matcha.hrl").
-import(matcha_lib, [gv/2, gv/3]).

get_profile(Data0, #session{uid = MyUId, uname = Uname}) ->
    VSchema =  #{<<"uid">> => [required, string]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"uid">> := <<"me">>}} ->
            {ok, Profile} = matcha_db:get_user(MyUId),
            {ok, matcha_lib:conv_locaction(add_photos(Profile, MyUId))};
        {ok, #{<<"uid">> := UId0}} ->
            UId1 = binary_to_integer(UId0),
            case matcha_db:get_user(UId1, MyUId) of
                {ok, Profile0} ->
                    Profile1 = Profile0#{<<"status">> => matcha_lib:is_subscribed(UId1)},
                    true = matcha_db:add_visit(UId1, MyUId),
                    matcha_lib:send_notification(UId1, #{type => visits, text => <<"Your profile visited by ", Uname/binary>>}),
                    {ok, add_photos(Profile1, UId1)};
                null -> {error, not_found}
            end;
        E -> E
    end.

get_profiles(Data0, #session{interests = MyInterests, location = MyLocation, uid = MyUid}) ->
    VSchema =
        #{<<"sort_by">> => [required, {one_of, [<<"distance">>, <<"common_interests">>, <<"age">>, <<"rating">>]}],
          <<"sort_type">> => [required, {one_of, [<<"asc">>, <<"desc">>]}],
          <<"age_range">> => [required],
          <<"rating_range">> => [required],
          <<"max_distance">> => [required, integer],
          <<"interests">> => [required, {list_of, [string]}],
          <<"gender">> => [required, {one_of, [<<"m">>, <<"f">>, <<"b">>]}],
          <<"limit">> => [required, positive_integer],
          <<"offset">> => [required, integer]},
	io:format("Data: ~p~n", [Data0]),
    case liver:validate(VSchema, Data0) of
        {ok, Data1} -> 
		{ok, Res} = matcha_db:get_profiles(Data1, MyInterests, MyLocation, MyUid),
		{ok, [maps:update_with(<<"avatar">>, fun(P) when is_binary(P) -> <<"/static/photos/", P/binary>>; (E) -> E end, U) || U <- Res]};
        E -> E
    end.

add_photos(Profile, UID) ->
    {ok, Photos} = matcha_db:get_user_photos(UID),
    D= #{<<"avatar">> => null, <<"photo1">> => null, <<"photo2">> => null, <<"photo3">> => null, <<"photo4">> => null},
    Profile1 = maps:merge(Profile, D),
    lists:foldl(fun(#{<<"label">> := L, <<"name">> := N}, P) ->
		    P#{L => <<"/static/photos/", N/binary>>}
		end, Profile1, Photos).

add_photo(L) ->
	[maps:update_with(<<"photo">>, fun(P) when is_binary(P) -> <<"/static/photos/", P/binary>>;
					  (E) -> E
					end, U) || U <- L].

get_visit_history(#session{uid = MyUId}) ->
    {ok, Res} = matcha_db:get_visit_history(MyUId),
    {ok, add_photo(Res)}.

get_likes(#session{uid = MyUId}) ->
    {ok, Res} = matcha_db:get_likes(MyUId),
    {ok, add_photo(Res)}.

get_chats(#session{uid = MyUId}) ->
    {ok, Res} = matcha_db:get_chats(MyUId),
    {ok, add_photo(Res)}.

get_chat_history(Data0, #session{uid = MyUId}) ->
    VSchema = #{<<"chat_id">> => [required, integer]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"chat_id">> := ChatId}} ->
            case matcha_db:is_chat_member(ChatId, MyUId) of
                true ->
                    io:format("CHAT ID: ~p~n", [ChatId]),
                    {ok, _} = matcha_db:get_chat_messages(ChatId);
                false -> #{<<"chat_id">> => <<"invalid">>}
            end;
        E -> E
    end.

update_profile(Data0, #session{uid = MyUId, uname = MyUname} = S) ->
    VSchema = #{<<"gender">> => [required, {one_of, [<<"m">>, <<"f">>]}],
                <<"sexual_preference">> => [required, {one_of, [<<"m">>, <<"f">>, <<"b">>]}],
                <<"bio">> => [string],
                <<"interests">> => [required, {list_of, [string]}],
                <<"avatar">> => [{'or', [is_null, string]}],
                <<"photo1">> => [{'or', [is_null, string]}],
                <<"photo2">> => [{'or', [is_null, string]}],
                <<"photo3">> => [{'or', [is_null, string]}],
                <<"photo4">> => [{'or', [is_null, string]}],
                <<"first_name">> => [required, {min_length, ?MIN_FNAME_LENGTH}, {max_length, ?MAX_FNAME_LENGTH}],
                <<"last_name">> => [required, {min_length, ?MIN_LNAME_LENGTH}, {max_length, ?MAX_LNAME_LENGTH}],
                <<"username">> => [required, {'or', [[{eq, MyUname}], [unique_uname]]}],
                <<"birthdate">> => [required, iso_date],
                <<"location">> => [required, {nested_object, #{
                    <<"lat">> => [required, decimal],
                    <<"lng">> => [required, decimal]}}]
    },
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"gender">> := Gender, <<"sexual_preference">> := SPref,
               <<"interests">> := Interests, <<"first_name">> := Fname, <<"last_name">> := Lname,
               <<"username">> := Uname, <<"birthdate">> := Birthdate,
	       <<"location">> := #{<<"lat">> := Lat, <<"lng">> := Lng}} = R} ->
            Avatar = gv(<<"avatar">>, R),
            P1 = gv(<<"photo1">>, R),
            P2 = gv(<<"photo2">>, R),
            P3 = gv(<<"photo3">>, R),
            P4 = gv(<<"photo4">>, R),
            Bio = gv(<<"bio">>, R, <<>>),
	    Location = {Lat, Lng},
            case update_profile(MyUId, Gender, SPref, Bio, Interests, Avatar, P1, P2, P3, P4, Fname, Lname, Uname,
                                Birthdate, Location) of
                {ok, P} ->
                    matcha_mnesia:update_session(S, Uname, Interests, Location),
                    {ok, matcha_lib:conv_locaction(add_photos(P, MyUId))};
                E -> E
            end;
        E -> E
    end.

update_profile(UID, Gender, SPref, Bio, Interests, Avatar, P1, P2, P3, P4, Fname, Lname, Uname, Birthdate, Location) ->
    Res = [{convert_photo(Avatar), <<"avatar">>},
           {convert_photo(P1), <<"photo1">>}, {convert_photo(P2), <<"photo2">>},
           {convert_photo(P3), <<"photo3">>}, {convert_photo(P4), <<"photo4">>}],
    case lists:foldl(fun({error, L}, Acc) -> Acc#{L => <<"invalid photo">>};
                        (_, Acc) -> Acc
                     end, #{}, Res) of
        R when map_size(R) =/= 0 -> {error, R};
        _ ->
            [update_photo(UID, PhotoBin, Label) || {PhotoBin, Label} <- Res, PhotoBin =/= null],
            {ok, _} = matcha_db:update_profile(UID, Gender, SPref, Bio, Interests,
                                               Fname, Lname, Uname, Birthdate, Location)
    end.

convert_photo(null) -> null;
convert_photo(Photo0) ->
    case binary:split(Photo0, <<"base64,">>) of
        [_, Photo1] ->
            Decoded = base64:decode(Photo1),
            case eimp:convert(Decoded, jpeg) of
                {ok, Converted} -> Converted;
                _ -> error
            end;
        _ -> error
    end.


update_photo(UID, PhotoBin, Label) ->
    Name = <<(matcha_lib:md5(PhotoBin))/binary, ".jpeg">>,
    Path = <<"priv/static/photos/", Name/binary>>,
    case filelib:is_regular(Path) of
        true -> ok;
        false -> file:write_file(Path, PhotoBin)
    end,
    true = matcha_db:update_photo(UID, Name, Label).

update_password(Data0, #session{uid = UId}) ->
    VSchema = #{<<"old_pass">> => [required, string],
                <<"new_pass">> => [required, strong_password]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"old_pass">> := OldPass, <<"new_pass">> := NewPass}} ->
            {ok, #{<<"password">> := PassHash}} = matcha_db:get_user_password(UId),
            case erlpass:change(OldPass, PassHash, NewPass, 12) of
                {error, bad_password} -> {error, #{<<"old_pass">> => <<"invalid password">>}};
                NewPassHash ->
                    true = matcha_db:update_user_password(UId, NewPassHash),
                    ok
            end;
        E -> E
    end.

update_email(Qs) ->
    Token = gv(<<"token">>, Qs),
    case matcha_mnesia:get_email_updating_state(Token) of
        #email_updating_state{uid = UId, email = Email} ->
            matcha_db:update_user_email(UId, Email),
            matcha_mnesia:delete_email_updating_state(Token),
            {redirect, <<"/">>};
        null ->
            {redirect, ?LINK_EXPIRED_REDIRECT_PATH}
    end.

update_email(Data0, #session{uname = Uname, uid = UId}, BaseUrl) ->
    VSchema = #{<<"email">> => [required, unique_email]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"email">> := Email}} ->
            Token = matcha_lib:rand_str(16),
            matcha_mnesia:create_email_updating_state(UId, Email, Token),
            Url = <<BaseUrl/binary, "/api/update_profile/confirm_email?token=", Token/binary>>,
            {ok, _} = erlydtl:compile_file("priv/templates/email_updating.html", email_updating),
            {ok, Body} = email_updating:render([{username, Uname}, {link, Url}, {out_dir, "priv/templates"}]),
            <<_/binary>> = matcha_lib:send_email(<<"MATCHA -- email changing">>, iolist_to_binary(Body), Email),
            ok;
        E -> E
    end.

like(Data, #session{uid = MyUId, uname = MyUname}) ->
    VSchema = #{<<"uid">> => [required, string]},
    case liver:validate(VSchema, Data) of
        {ok, #{<<"uid">> := UId0}} ->
            UId1 = binary_to_integer(UId0),
            true = matcha_db:create_like(MyUId, UId1),
            case matcha_db:like_exists(UId1, MyUId) of
                true ->
                    true = matcha_db:create_chat(UId1, MyUId),
                    Msg = <<"You liked back, now you can chat with ", MyUname/binary>>,
                    matcha_lib:send_notification(UId1, #{type => chats, text => Msg});
                false ->
                    Msg = <<"Your liked by ", MyUname/binary>>,
                    matcha_lib:send_notification(UId1, #{type => likes, text => Msg})
            end;
        E -> E
    end.

unlike(Data, #session{uid = MyUId, uname = MyUname}) ->
    VSchema = #{<<"uid">> => [required, string]},
    case liver:validate(VSchema, Data) of
        {ok, #{<<"uid">> := UId0}} ->
            UId1 = binary_to_integer(UId0),
            matcha_db:delete_like(MyUId, UId1),
            Msg = <<"User ", MyUname/binary, " unliked you">>,
            case matcha_db:like_exists(UId1, MyUId) of
                true ->
                    matcha_lib:send_notification(UId1, #{type => chats, text => Msg}),
                    true = matcha_db:delete_chat(UId1, MyUId),
                    ok;
                false ->
                    matcha_lib:send_notification(UId1, #{type => likes, text => Msg}),
                    ok
            end;
        E -> E
    end.

rate(Data0, #session{uid = MyUId}) ->
    VSchema = #{<<"rating">> => [required, integer],
                <<"uid">> => [required, string]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"rating">> := Rating, <<"uid">> := UId0}} when Rating >= 0 andalso Rating =< 10 ->
            UId1 = binary_to_integer(UId0),
            case matcha_db:get_user(UId1, MyUId) of
                {ok, _} ->  {ok, _} = matcha_db:set_rating(MyUId, UId1, Rating);
                null -> {error, #{<<"uid">> => <<"invalid value">>}}
            end;
        {ok, _} -> {error, #{<<"rating">> => <<"invalid value">>}};
        E -> E
    end.

send(Data0, #session{uid = MyUId, uname = MyUname}) ->
    VSchema = #{<<"chat_id">> => [required, integer],
                <<"message">> => [required, not_empty, string]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"chat_id">> := ChatId, <<"message">> := Msg}} ->
            case matcha_db:get_potential_lover(ChatId, MyUId) of
                {ok, #{<<"user_id">> := UId}} ->
                    {ok, M} = Res = matcha_db:save_message(ChatId, MyUId, MyUname, Msg),
                    matcha_lib:send_notification(UId, M#{type => chats}),
                    Res;
                null -> {error, #{<<"chat_id">> => <<"invalid">>}}
            end;
        {ok, _} -> {error, #{<<"rating">> => <<"invalid value">>}};
        E -> E
    end.

block(Data0, #session{uid = MyUid}) ->
    VSchema = #{<<"uid">> => [required, string]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"uid">> := UId0}} ->
            true = matcha_db:block_user(MyUid, binary_to_integer(UId0)),
            ok;
        E -> E
    end.

unblock(Data0, #session{uid = MyUid}) ->
    VSchema = #{<<"uid">> => [required, string]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"uid">> := UId0}} ->
            true = matcha_db:unblock_user(MyUid, binary_to_integer(UId0)),
            ok;
        E -> E
    end.

report(Data0, #session{uid = MyUid}) ->
    VSchema = #{<<"uid">> => [required, string]},
    case liver:validate(VSchema, Data0) of
        {ok, #{<<"uid">> := UId0}} ->
            true = matcha_db:report_user(MyUid, binary_to_integer(UId0)),
            ok;
        E -> E
    end.
