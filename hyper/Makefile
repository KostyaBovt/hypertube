PROJECT = hyper
PROJECT_DESCRIPTION = Hypertube user account manager
PROJECT_VERSION = 0.1.0


DEPS = cowboy epgsql pgapp eimp jsone gen_smtp erlydtl erlpass
dep_cowboy_commit = master
dep_epgsql_commit = 4.1.0
dep_pgapp = git https://github.com/epgsql/pgapp.git master
dep_eimp = git https://github.com/processone/eimp master
dep_erlpass = git https://github.com/AndreyKlimchuk/erlpass master


BUILD_DEPS = liver jwt
dep_liver = git https://github.com/AndreyKlimchuk/liver.git master
dep_jwt = git https://github.com/artemeff/jwt.git master

LOCAL_DEPS = mnesia inets crypto

include erlang.mk

DIALYZER_OPTS = -Wno_match

