all: clean compile test

clean:
	./rebar clean

compile: 
	./rebar compile

test:
	./rebar eunit