-module(hyper_db).


-export([get_user_by_id/1,
         get_user_by_uname/1,
         get_user_by_email/1,
         get_user_by_social/2]).

%% API
-export([
         user_with_email_exists/1,
         user_with_uname_exists/1,
         create_user/5,
         create_user_from_social_info/5,
         update_user_password/2,
         update_user/6]).



get_user_by_id(Id) -> get_user("id = $1", [Id]).

get_user_by_uname(Uname) -> get_user("uname = $1", [Uname]).

get_user_by_email(Email) -> get_user("email = $1", [Email]).

get_user_by_social(Provider, Id) -> get_user("social_provider = $1 AND social_id = $2", [Provider, Id]).

get_user(SearchCondition, Args) ->
    db_val(q(["SELECT id, uname, fname, lname, bio, email, password, locale, avatar, social_provider, social_id
               FROM users WHERE ", SearchCondition], Args)).


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

update_user(UId, Uname, Fname, Lname, Bio, PhotoName) ->
    db_val(q("UPDATE users
              SET uname  = COALESCE($2, uname),
                  fname  = COALESCE($3, fname),
                  lname  = COALESCE($4, lname),
                  bio    = COALESCE($5, bio),
                  avatar = COALESCE($6, avatar),
              WHERE id = $1
              RETURNING uname, fname, lname, bio, email, locale, avatar",
             [UId, Uname, Fname, Lname, Bio, PhotoName])).

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

