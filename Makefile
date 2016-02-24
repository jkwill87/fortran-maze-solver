fms:
	@$(MAKE) -C src
	mkdir -p bin
	cp src/fms bin/fms

nocolour:
	@$(MAKE) -C src 'FCFLAGS=-D_NO_COLOUR -cpp -Wall -O2 -std=f2003'
	mkdir -p bin
	cp src/fms bin/fms

debug: clean
	@$(MAKE) -C src 'FCFLAGS=-cpp -Wall -std=f2003 -g -fbounds-check'
	mkdir -p bin
	cp src/fms bin/fms	

clean:
	@$(MAKE) -C src clean
	rm -f -r bin
