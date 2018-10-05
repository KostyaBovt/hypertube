-module(hyper_db).

%% API
-export([get_user_by_username/1,
         user_with_email_exists/1,
         user_with_uname_exists/1,
         create_user/5,
         create_user_from_social_info/5,
         get_user_by_social_info/2,
         get_user_by_email/1,
         update_user_password/2,
         get_user/1, get_user/2,
         get_user_photos/1,
         is_chat_member/2,
         get_chat_messages/1,
         update_user_email/2,
         update_profile/10,
         update_photo/3,
         get_user_password/1]).


get_user_by_username(Uname) ->
    db_val(q("SELECT id, email, username, first_name, last_name, to_char(birthdate, 'YYYY-MM-DD') AS birthdate,
		     gender::TEXT, sexual_preference::TEXT, bio, interests, is_complete, location, password, rating
              FROM users WHERE username = $1", [Uname])).

get_user_by_social_info(Provider, SocialId) ->
    db_val(q("SELECT * FROM users WHERE social_provider = $1 AND social_id = $2", [Provider, SocialId])).

get_user(MyUId) ->
    db_val(q("SELECT id, email, username, first_name, last_name, to_char(birthdate, 'YYYY-MM-DD') AS birthdate,
		     gender::TEXT, sexual_preference::TEXT, bio, interests, is_complete, location, rating
              FROM users
              WHERE id = $1 ",
             [MyUId])).

get_user_password(MyUId) ->
    db_val(q("SELECT password FROM users WHERE id = $1", [MyUId])).

get_user(UId, MyUId) ->
    db_val(q("SELECT u.id, u.username, u.first_name, u.last_name, to_char(u.birthdate, 'YYYY-MM-DD') AS birthdate,
		     u.gender::TEXT, u.sexual_preference::TEXT, u.bio, u.interests, u.last_seen_at, u.rating AS total_rating,
                     (ul.liked_user_id IS NOT NULL) AS is_liked,
                     (ub.blocked_user_id IS NOT NULL) AS is_blocked,
                     COALESCE(ur.rating, 0)
              FROM users u
              LEFT JOIN users_likes ul ON (liked_user_id, liker_id) = ($1, $2)
              LEFT JOIN users_blocks ub ON (blocked_user_id, blocker_id) = ($1, $2)
              LEFT JOIN users_rating ur ON (rated_user, rater) = ($1, $2)
              WHERE u.id = $1 AND u.is_complete = true
                AND NOT EXISTS (SELECT id FROM users_blocks
                                WHERE (blocker_id, blocked_user_id) = ($1, $2)
                                UNION
                                SELECT id FROM users_reports
                                WHERE (reporter, reported) = ($1, $2) OR (reporter, reported) = ($2, $1))",
             [UId, MyUId])).

get_user_photos(UID) ->
    db_vals(q("SELECT name, label FROM photos WHERE user_id = $1", [UID])).

get_user_by_email(Email) ->
    db_val(q("SELECT * FROM users WHERE email = $1", [Email])).

user_with_email_exists(Email) -> db_bool(q("SELECT id FROM users WHERE email = $1", [Email])).

user_with_uname_exists(Uname) -> db_bool(q("SELECT id FROM users WHERE username = $1", [Uname])).

create_user(Uname, Fname, Lname, Pass, Email) ->
    db_val(q("INSERT INTO users (username, first_name, last_name, password, email)
              VALUES ($1, $2, $3, $4, $5)
              RETURNING username, id",
             [Uname, Fname, Lname, Pass, Email])).

create_user_from_social_info(Provider, SocialId, Uname, Fname, Lname) ->
    db_val(q("INSERT INTO users (social_provider, social_id, username, first_name, last_name)
              VALUES ($1, $2, $3, $4, $5)
              RETURNING username, id",
             [Provider, SocialId, Uname, Fname, Lname])).

update_user_password(UId, NewPassword) ->
    db_bool(q("UPDATE users SET password = $1 WHERE id = $2", [NewPassword, UId])).

update_user_email(UId, NewEmail) ->
    db_bool(q("UPDATE users SET email = $1 WHERE id = $2", [NewEmail, UId])).

get_chat_messages(ChatId) ->
    db_vals(q("SELECT id, sender_id, sender_username, text, dt
               FROM chats_messages
               WHERE chat_id = $1
               ORDER BY dt",
              [ChatId])).

is_chat_member(CId, UId) ->
    db_bool(q("SELECT id FROM chats WHERE id = $1 AND user_id1 = $2 OR user_id2 = $2", [CId, UId])).


update_profile(UId, Gender, SPref, Bio, Interests, Fname, Lname, Uname, Birthdate, Location) ->
    db_val(q("UPDATE users
              SET gender = $2, sexual_preference = $3, bio = $4, interests = $5,
                  first_name = $6, last_name = $7, username = $8, birthdate = $9,
		          location = $10, is_complete = true
              WHERE id = $1
              RETURNING id, email, username, first_name, last_name, to_char(birthdate, 'YYYY-MM-DD') AS birthdate,
			gender::TEXT, sexual_preference::TEXT, bio, interests, is_complete, location",
             [UId, Gender, SPref, Bio, Interests, Fname, Lname, Uname, Birthdate, Location])).

update_photo(UID, Name, Label) ->
    db_bool(q("INSERT INTO photos (user_id, name, label)
               VALUES ($1, $2, $3)
	       ON CONFLICT (user_id, label) DO UPDATE SET name = $2",
	      [UID, Name, Label])).




%% Internal

q(Sql, Args) -> pgapp:equery(Sql, Args).

-spec serialize(list(), list()) -> [map()].
serialize(Columns, Rows) ->
    [lists:foldl(fun({_,Key,_,_,_,_,_}, Acc) ->
                     Idx = map_size(Acc) + 1,
                     maps:put(Key, element(Idx, Values), Acc)
                 end, #{}, Columns) || Values <- Rows].

db_bool({ok, 0}) -> false;
db_bool({ok, _, []}) -> false;
db_bool({ok, 0, _, _}) -> false;
db_bool({error, _} = E) -> E;
db_bool(_) -> true.

db_val(Ret) -> db_val(Ret, map).
db_val({ok, _, Cols, [_] = Rows}, map) -> {ok, hd(serialize(Cols, Rows))};
db_val({ok, Cols, [_] = Rows}, map)    -> {ok, hd(serialize(Cols, Rows))};
db_val({ok, _, _, [Res]}, tuple) -> {ok, Res};
db_val({ok, _, [Res]}, tuple)    -> {ok, Res};
db_val({ok, _, _, []}, _)        -> null;
db_val({ok, _, []}, _)           -> null;
db_val(E, _)                     -> db_error(E).

db_vals(Ret) -> db_vals(Ret, map).
db_vals({ok, _, Cols, Rows}, map) -> {ok, serialize(Cols, Rows)};
db_vals({ok, Cols, Rows}, map)    -> {ok, serialize(Cols, Rows)};
db_vals({ok, _, _, Res}, tuple)   -> {ok, Res};
db_vals({ok, _, Res}, tuple)      -> {ok, Res};
db_vals(E, _)                     -> db_error(E).

db_error({error, _} = E) -> E;
db_error(E) -> {error, E}.

