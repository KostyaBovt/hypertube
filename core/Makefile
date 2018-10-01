PROJECT = matcha
PROJECT_DESCRIPTION = Dating site
PROJECT_VERSION = 0.1.0


DEPS = cowboy lager epgsql pgapp eimp jsone gen_smtp erlydtl mimetypes erlpass
dep_cowboy_commit = master
dep_epgsql_commit = master
dep_pgapp = git https://github.com/epgsql/pgapp.git master
dep_eimp = git https://github.com/processone/eimp master
dep_erlpass = git https://github.com/AndreyKlimchuk/erlpass master

BUILD_DEPS = liver
dep_liver = git https://github.com/AndreyKlimchuk/liver.git master

LOCAL_DEPS = mnesia inets crypto

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'

rerun: all
	erl -pa ebin/ -s matcha

