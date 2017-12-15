#!/bin/sh
NODE=b1@$(hostname)
APP=bidder_app
cd `dirname $0`
exec erl -smp auto +P 134217727 +K true +A 64 +stbt ts +zdbbl 512MB -pa $PWD/_build/default/lib/*/ebin -boot start_sasl -s lager -s $APP $@ -config app.config -sname $NODE -setcookie chat_cookie
