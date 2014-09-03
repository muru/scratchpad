#! /bin/bash

COUNT=1
MAX=$((5*1024*1024*1024))
i=1
TIMEFMT='%U %S %E %P'

mkdir -p /opt/test/cmp-diff
cd /opt/test/cmp-diff

touch file1 file2
SIZE=$(ls -s file1 | awk '{print $1}')

while [[ $SIZE -le $MAX ]]
do
	dd if=/dev/urandom bs=4096 count=$((COUNT*256)) status=none | base64 | tee >(sed '$ s/[+0-9]/a/' > file1) | sed '$ s/[+0-9]/b/' > file2
	dd if=/dev/urandom bs=4096 count=$COUNT status=none | base64 >> file1 
	dd if=/dev/urandom bs=4096 count=$COUNT status=none | base64 >> file2
	/usr/bin/time --quiet -f " D %D E %E F %F I %I K %K M %M O %O P %P R %R S %S U %U W %W X %X Z %Z c %c e %e k %k p %p r %r s %s t %t w %w x %x" -o ~/devel/time-diff-log -a diff -q file1 file2 > /dev/null
	/usr/bin/time --quiet -f " D %D E %E F %F I %I K %K M %M O %O P %P R %R S %S U %U W %W X %X Z %Z c %c e %e k %k p %p r %r s %s t %t w %w x %x" -o ~/devel/time-cmp-log -a cmp -s file1 file2
	SIZE=$(ls -s file1 | awk '{print $1}')
	sed '$ s/$/ '$SIZE'/' -i ~/devel/time-diff-log
	sed '$ s/$/ '$SIZE'/' -i ~/devel/time-cmp-log
	echo $COUNT $SIZE
	i=$((i + 1))
	COUNT=$((COUNT + i))
done
