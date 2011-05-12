#!/bin/sh

i=0;


while [ $i -lt 4 ]
	do
		sleep 1 
		echo "Stage $i"
		i=$(expr $i + 1)
	done
