-define(MIN_UNAME_LENGTH, 6).
-define(MAX_UNAME_LENGTH, 16).
-define(MIN_FNAME_LENGTH, 1).
-define(MAX_FNAME_LENGTH, 50).
-define(MIN_LNAME_LENGTH, 1).
-define(MAX_LNAME_LENGTH, 50).
-define(MIN_PASS_LENGTH, 8).
-define(MAX_PASS_LENGTH, 20).
-define(MAX_BIO_LENGTH, 200).

-define(HTTP_PORT, 8080).

-define(FRONTEND_ORIGIN, "http://localhost:3000").
-define(LINK_EXPIRED_REDIRECT_PATH, <<?FRONTEND_ORIGIN, "/link_expired">>).

-define(NOW_SEC, calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

-record(temp_account, {token::binary() | '_',
                       uname::binary() | '_',
                       fname::binary() | '_',
                       lname::binary() | '_',
                       email::binary() | '_',
                       password::binary() | '_',
                       created_at = ?NOW_SEC ::non_neg_integer() | '_'}).

-record(password_recovering_state, {id::non_neg_integer() | '_',
                                    token::binary() | '_',
                                    stage = confirmation::confirmation | new_password | '_',
                                    created_at = ?NOW_SEC ::non_neg_integer() | '_'}).

-record(email_updating_state, {token::binary() | '_',
                               email::binary() | '_',
                               uid::non_neg_integer() | '_',
                               created_at = ?NOW_SEC ::non_neg_integer() | '_'}).

