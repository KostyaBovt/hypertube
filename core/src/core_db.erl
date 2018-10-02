-module(core_db).

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
         get_visit_history/1,
         add_visit/2,
         get_likes/1,
         get_chats/1,
         is_chat_member/2,
         get_chat_messages/1,
         create_like/2, delete_like/2, like_exists/2,
         create_chat/2, delete_chat/2,
         update_user_email/2,
         update_profile/10,
         update_photo/3,
         get_profiles/4,
         set_rating/3,
         get_potential_lover/2,
         save_message/4,
         update_last_seen_at/1,
         block_user/2, unblock_user/2,
         report_user/2,
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

get_visit_history(UId) ->
    db_vals(q("SELECT uv.visitor_id AS id, uv.dt, u.username, u.first_name, u.last_name, p.name AS photo
               FROM users_visits uv
               JOIN users u ON uv.visitor_id = u.id
               LEFT JOIN photos p ON u.id = p.user_id AND p.label = 'avatar'
               WHERE uv.visited_user_id = $1",
              [UId])).

add_visit(Visited, Visitor) ->
    db_bool(q("INSERT INTO users_visits (visited_user_id, visitor_id) VALUES ($1, $2)", [Visited, Visitor])).

get_likes(UId) ->
    db_vals(q("WITH liked_by_me AS (SELECT liked_user_id
	     		   	 	            FROM users_likes
			   	 	                WHERE liker_id = $1)
               SELECT ul.liker_id AS id, ul.dt, u.username, u.first_name, u.last_name, p.name AS photo
               FROM users_likes ul
               JOIN users u ON ul.liker_id = u.id
               LEFT JOIN photos p ON u.id = p.user_id AND p.label = 'avatar'
               WHERE ul.liked_user_id = $1
                 AND ul.liker_id NOT IN (select * from liked_by_me)",
              [UId])).

get_chats(UId) ->
    db_vals(q("WITH c AS (SELECT id AS chat_id,
                                 (CASE WHEN user_id1 = $1 THEN user_id2
                                       ELSE user_id1
                                  END) AS user_id
                          FROM chats
                          WHERE user_id1 = $1 OR user_id2 = $1)
               SELECT c.chat_id, c.user_id, u.username, p.name AS photo
               FROM c
               JOIN users u ON c.user_id = u.id
               LEFT JOIN photos p ON u.id = p.user_id AND p.label = 'avatar'",
              [UId])).

get_chat_messages(ChatId) ->
    db_vals(q("SELECT id, sender_id, sender_username, text, dt
               FROM chats_messages
               WHERE chat_id = $1
               ORDER BY dt",
              [ChatId])).

is_chat_member(CId, UId) ->
    db_bool(q("SELECT id FROM chats WHERE id = $1 AND user_id1 = $2 OR user_id2 = $2", [CId, UId])).

get_potential_lover(CId, UId) ->
    db_val(q("SELECT (CASE WHEN user_id1 = $2 THEN user_id2
                           ELSE user_id1
                      END) AS user_id
              FROM chats
              WHERE id = $1 AND (user_id1 = $2 OR user_id2 = $2)",
             [CId, UId])).

create_like(LikerUId, LikedUId) ->
    db_bool(q("INSERT INTO users_likes (liker_id, liked_user_id) VALUES ($1, $2)", [LikerUId, LikedUId])).

delete_like(LikerUId, LikedUId) ->
    db_bool(q("DELETE FROM users_likes WHERE (liker_id, liked_user_id) = ($1, $2)", [LikerUId, LikedUId])).

like_exists(LikerUId, LikedUId) ->
    db_bool(q("SELECT id FROM users_likes WHERE (liker_id, liked_user_id) = ($1, $2)", [LikerUId, LikedUId])).

create_chat(UId1, UId2) ->
    db_bool(q("INSERT INTO chats (user_id1, user_id2) VALUES ($1, $2)", [UId1, UId2])).

delete_chat(UId1, UId2) ->
    db_bool(q("WITH c AS (DELETE FROM chats
                          WHERE (user_id1, user_id2) = ($1, $2) OR (user_id1, user_id2) = ($2, $1)
                          RETURNING id)
               DELETE FROM chats_messages
               WHERE chat_id = (SELECT id FROM c)",
              [UId1, UId2])).

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

set_rating(Rater, Rated, Rating) ->
    true = db_bool(q("INSERT INTO users_rating (rater, rated_user, rating) VALUES ($1, $2, $3)
                      ON CONFLICT (rater, rated_user)
                      DO UPDATE SET rating = EXCLUDED.rating",
                     [Rater, Rated, Rating])),
    db_val(q("UPDATE users
              SET rating = (SELECT SUM(rating) / COUNT(*)
                            FROM users_rating
                            WHERE rated_user = $1)
              WHERE id = $1
              RETURNING rating AS total_rating",
              [Rated])).

save_message(ChatId, UId, Uname, Msg) ->
    db_val(q("INSERT INTO chats_messages (chat_id, sender_id, sender_username, text, dt)
              VALUES ($1, $2, $3, $4, NOW())
              RETURNING id, chat_id, sender_id, sender_username, text, dt",
              [ChatId, UId, Uname, Msg])).

update_last_seen_at(UId) ->
    db_bool(q("UPDATE users SET last_seen_at = NOW() WHERE id = $1", [UId])).

%%#{<<"sort_by">> => <<"age">>, <<"sort_type">> => <<"desc">>,
%%  <<"age_range">> => #{<<"min">> => 0, <<"max">> => 100},
%%  <<"rating_range">> => #{<<"min">> => 0, <<"max">> => 10},
%%  <<"max_distance">> => 1000000000,
%%  <<"interests">> => [],
%%  <<"gender">> => <<"b">>,
%%  <<"limit">> => 5, <<"offset">> => 0}
get_profiles(#{<<"sort_by">> := SortBy, <<"sort_type">> := SortType,
               <<"age_range">> := #{<<"min">> := MinAge, <<"max">> := MaxAge},
               <<"rating_range">> := #{<<"min">> := MinRating, <<"max">> := MaxRating},
               <<"interests">> := Interests,
               <<"gender">> := Gender, <<"max_distance">> := MaxDist,
               <<"limit">> := Limit, <<"offset">> := Offset}, MyInterests, {Lat, Long}, MyUId) ->
    io:format("INPUT: ~p~n", [ [MinAge, MaxAge, MinRating, MaxRating, Gender, MaxDist,
        Interests, MyInterests, Lat, Long, Limit, Offset, MyUId]]),
    db_vals(q(<<"SELECT *
                 FROM (SELECT u.id, u.username, u.first_name, u.last_name, u.rating,
                              u.gender, u.interests, u.is_complete, p.name AS avatar,
			                  EXTRACT(YEAR FROM AGE(u.birthdate)) AS age,
		                      array_intersect(u.interests, $8) AS common_interests,
		                      gc_dist($9, $10, u.location[0], u.location[1]) as distance,
		                      ub.blocker_id, ur.reporter
                       FROM users u
                       INNER JOIN photos p ON u.id = p.user_id AND p.label = 'avatar'
                       LEFT JOIN users_blocks ub ON (u.id, $13) = (ub.blocker_id, ub.blocked_user_id)
                       LEFT JOIN users_reports ur ON (u.id, $13) = (ur.reporter, ur.reported)
                                                  OR (u.id, $13) = (ur.reported, ur.reporter)) as _
                 WHERE blocker_id IS NULL AND reporter IS NULL AND is_complete = true
                   AND age >= $1 AND age <= $2
                   AND rating >= $3 AND rating <= $4
                   AND ($5 = 'b' OR $5 = gender::TEXT)
                   AND distance <= $6::BIGINT
                   AND interests @> $7
                 ORDER BY ", (order_cond(SortBy))/binary, " ", SortType/binary, "
                 LIMIT $11 OFFSET $12">>,
              [MinAge, MaxAge, MinRating, MaxRating, Gender, MaxDist,
               Interests, MyInterests, Lat, Long, Limit, Offset, MyUId])).

order_cond(<<"common_interests">>) -> <<"ARRAY_LENGTH(common_interests, 1)">>;
order_cond(ColName) -> ColName.

block_user(Blocker, Blocked) ->
    db_bool(q("INSERT INTO users_blocks (blocker_id, blocked_user_id) VALUES ($1, $2)
               ON CONFLICT DO NOTHING",
              [Blocker, Blocked])).

unblock_user(Blocker, Blocked) ->
    db_bool(q("DELETE FROM users_blocks WHERE (blocker_id, blocked_user_id) = ($1, $2)", [Blocker, Blocked])).


report_user(Reporter, Reported) ->
    db_bool(q("INSERT INTO users_reports (reporter, reported) VALUES ($1, $2)
               ON CONFLICT DO NOTHING", [Reporter, Reported])).


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

