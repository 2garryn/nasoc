all: clean get-deps compile 

clean:
	./rebar clean

get-deps:
	./rebar get-deps

compile: 
	./rebar compile
