-define(MIN_UNAME_LENGTH, 6).
-define(MAX_UNAME_LENGTH, 16).
-define(MIN_FNAME_LENGTH, 1).
-define(MAX_FNAME_LENGTH, 50).
-define(MIN_LNAME_LENGTH, 1).
-define(MAX_LNAME_LENGTH, 50).
-define(MIN_PASS_LENGTH, 8).
-define(MAX_PASS_LENGTH, 20).

-define(LINK_EXPIRED_REDIRECT_PATH, <<"/link_expired">>).

-record(temp_account, {token::binary(),
                       uname::binary(),
                       fname::binary(),
                       lname::binary(),
                       email::binary(),
                       password::binary()}).

-record(session, {id::binary(),
                  uid::binary(),
                  uname::binary(),
                  interests = []::[binary()],
                  location = {0, 0}::{number(), number()} | undefined,
                  is_complete::boolean()}).

-record(password_recovering_state, {id::non_neg_integer(),
                                    token::binary(),
                                    stage = confirmation::confirmation | new_password}).

-record(email_updating_state, {token::non_neg_integer(), email::binary(), uid::non_neg_integer()}).

