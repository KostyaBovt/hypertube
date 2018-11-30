-module(hyper_db).

%% API
-export([get_user_by_id/1,
         get_user_by_uname/1,
         get_user_by_email/1,
         get_user_by_social/2,
         user_with_email_exists/1,
         user_with_uname_exists/1,
         create_user/6,
         create_user_from_social_info/7,
         update_user/6,
         update_user_password/2,
         update_user_locale/2,
         update_user_email/2,
         get_comments/1,
         create_comment/3,
         update_social_token/2]).

get_user_by_id(Id) -> get_user("id = $1", [Id]).

get_user_by_uname(Uname) -> get_user("uname = $1", [Uname]).

get_user_by_email(Email) -> get_user("email = $1", [Email]).

get_user_by_social(Provider, Id) -> get_user("social_provider = $1 AND social_id = $2", [Provider, Id]).

get_user(SearchCondition, Args) ->
    db_val(q(["SELECT id, uname, fname, lname, bio, email, password, locale, avatar,
                      social_provider, social_id, social_token
               FROM users WHERE ", SearchCondition], Args)).


user_with_email_exists(Email) -> db_bool(q("SELECT id FROM users WHERE email = $1", [Email])).

user_with_uname_exists(Uname) -> db_bool(q("SELECT id FROM users WHERE uname = $1", [Uname])).

update_user_locale(UId, NewLocale) ->
    db_bool(q("UPDATE users SET locale = $2 WHERE id = $1", [UId, NewLocale])).

create_user(Uname, Fname, Lname, Pass, Email, Locale) ->
    db_val(q("INSERT INTO users (uname, fname, lname, password, email, locale)
              VALUES ($1, $2, $3, $4, $5, $6)
              RETURNING *",
             [Uname, Fname, Lname, Pass, Email, Locale])).

create_user_from_social_info(Provider, SocialId, SocialToken, Uname, Fname, Lname, Locale) ->
    db_val(q("INSERT INTO users (social_provider, social_id, social_token, uname, fname, lname, locale)
              VALUES ($1, $2, $3, $4, $5, $6, $7)
              RETURNING *",
             [Provider, SocialId, SocialToken, Uname, Fname, Lname, Locale])).

update_user_password(UId, NewPassword) ->
    db_bool(q("UPDATE users SET password = $1 WHERE id = $2", [NewPassword, UId])).

update_user_email(UId, NewEmail) ->
    db_bool(q("UPDATE users SET email = $1 WHERE id = $2", [NewEmail, UId])).

update_user(UId, Uname, Fname, Lname, Bio, PhotoName) ->
    db_val(q("UPDATE users
              SET uname  = COALESCE($2, uname),
                  fname  = COALESCE($3, fname),
                  lname  = COALESCE($4, lname),
                  bio    = COALESCE($5, bio),
                  avatar = COALESCE($6, avatar)
              WHERE id = $1
              RETURNING uname, fname, lname, bio, email, locale, avatar, social_provider",
             [UId, Uname, Fname, Lname, Bio, PhotoName])).

get_comments(ImdbId) ->
    db_vals(q("SELECT u.uname, u.fname, u.lname, u.avatar,
                      c.text, TO_CHAR(c.dt, 'YYYY-MM-DD HH24:MI:SS') AS dt
               FROM comments AS c
               JOIN users AS u ON c.user_id = u.id
               WHERE c.imdb_id = $1
               ORDER BY dt DESC", [ImdbId])).

create_comment(UId, ImdbId, Text) ->
    db_val(q("WITH i AS (INSERT INTO comments (user_id, imdb_id, text, dt)
                         VALUES ($1, $2, $3, NOW())
                         RETURNING text, dt)
              SELECT u.uname, u.fname, u.lname, u.avatar,
                     i.text, TO_CHAR(i.dt, 'YYYY-MM-DD HH24:MI:SS') AS dt
              FROM users AS u, i
              WHERE u.id = $1",
             [UId, ImdbId, Text])).

update_social_token(UId, Token) -> db_bool(q("UPDATE users SET social_token = $2 WHERE id = $1", [UId, Token])).

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
db_val({ok, _, _, []}, _)        -> null;
db_val({ok, _, []}, _)           -> null;
db_val(E, _)                     -> db_error(E).

db_vals(Ret) -> db_vals(Ret, map).
db_vals({ok, _, Cols, Rows}, map) -> {ok, serialize(Cols, Rows)};
db_vals({ok, Cols, Rows}, map)    -> {ok, serialize(Cols, Rows)};
db_vals(E, _)                     -> db_error(E).

db_error({error, _} = E) -> E;
db_error(E) -> {error, E}.

