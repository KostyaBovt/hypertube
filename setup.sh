if ! brew ls --versions postgresql > /dev/null; then
	echo "postgresql is not installed, installing ...";
	brew install postgresql;
fi

#if ! brew ls --version erlang > /dev/null; then
#	echo "erlang is not installed, installing ..."
#	brew install erlang
#fi

createdb
psql < Hypertube.sql

cd hyper
make
./start.sh


