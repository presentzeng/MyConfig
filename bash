#!/bin/sh
frm()
{
	for i in .*
	do 
		if [ $i != ".git" ]
		then
			rm -rf $i	
		fi
	done
}
if test -f test
then 
	clear
	./test
	echo "\n"
	echo "enter for rm & a for again"
	read var
	if  [ $var="a" ] # on both sizes of = there shouldn.t has space
	then
		clear
		./test
		read choice # any key to return
		frm
		vim Glist.cc
		clear
	else
		rm test
		vim Glist.cc
		frm
		clear
	fi
else
	echo "compile error"	
	read var #it means press any key to return
	frm
	vim Glist.cc
	clear
fi
