.IGNORE :test
test:Glist.cc
	g++ -o $@ $<
	sudo ./bash
.PHONY git:
	git add *
	git commit -m "auto commint by makefile"
	git push origin master
