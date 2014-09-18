#! /bin/bash

PROC=$1
echo $PROC

if [[ $(readlink /proc/$PROC/fd/1) =~ ^pipe: ]]
then
	# Assuming first process in chain...
	NEXT_FD=1
elif [[ $(readlink /proc/$PROC/fd/0) =~ ^pipe: ]]
then
	# Last process in chain...
	NEXT_FD=0
else
	# Doesn't look like a pipe.
	exit
fi
	
NEXT_PROC_PIPE=$(readlink /proc/$PROC/fd/$NEXT_FD)

while [[ $NEXT_PROC_PIPE =~ ^pipe: ]] 
do
	PROC=$(find /proc/*/fd -type l -printf "%p/%l\n" 2>/dev/null | awk -F'/' '($6 == "'"$NEXT_PROC_PIPE"'") && ($3 != "'$PROC'" ) {print $3}')
	NEXT_PROC_PIPE=$(readlink /proc/$PROC/fd/$NEXT_FD)
	echo $PROC
done
