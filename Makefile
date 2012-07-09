
all: compile


deps:
	rebar get-deps


compile:
	rebar compile


clean:
	rebar clean


distclean: clean
	rm -rf deps log


.PHONY: deps

