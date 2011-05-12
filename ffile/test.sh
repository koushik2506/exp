#!/bin/sh

i=0

while [ $i -le 100 ]
	do
		echo $i
		i=$(expr $i + 1)
		sleep 1
	done
