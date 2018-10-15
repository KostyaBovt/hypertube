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

-define(LINK_EXPIRED_REDIRECT_PATH, <<"/link_expired">>).

-record(temp_account, {token::binary() | '_',
                       uname::binary() | '_',
                       fname::binary() | '_',
                       lname::binary() | '_',
                       email::binary() | '_',
                       password::binary() | '_'}).

-record(password_recovering_state, {id::non_neg_integer() | '_',
                                    token::binary() | '_',
                                    stage = confirmation::confirmation | new_password | '_'}).

-record(email_updating_state, {token::binary() | '_',
                               email::binary() | '_',
                               uid::non_neg_integer() | '_'}).

