#!/bin/bash

while true; do
	clear
	make
	echo "" | ./mysplinterpreter input.spl
	sleep 5
done
