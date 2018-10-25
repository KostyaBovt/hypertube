SHOST=`hostname -s`
run_erl -daemon /tmp/ logs/ "exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname 'hyper'@$SHOST -boot start_sasl -s 'hyper'"
