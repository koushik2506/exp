#!/bin/sh

rm -rf /tmp/erlang.*
rm -rf /tmp/log/*

run_erl -daemon /tmp/ /tmp/log/ "exec erl -noshell -s samp"

until [ -p /tmp/erlang.pipe.1.w ]
	do
		echo "sleeping"
		sleep 1
	done


echo "into read"
(sleep 1;echo -e \022 > /tmp/erlang.pipe.1.w)&

while read line < /tmp/erlang.pipe.1.r
	do
		echo $line 
	done
