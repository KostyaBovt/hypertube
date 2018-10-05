SHOST=`hostname -s`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin \
 -sname 'core'@$SHOST \
 -boot start_sasl \
 -s 'hyper' \
