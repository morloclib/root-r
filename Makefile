test:
	morloc make test.loc
	./nexus test

clean:
	rm -rf nexus nexus.c pool*
