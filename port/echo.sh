#!/bin/sh

while [ 1 ]
do
	read a
	echo "got $a" >>/tmp/a
	echo $a
done
