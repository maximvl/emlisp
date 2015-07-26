all: compile peg

deps:
	./rebar get-deps

compile: deps
	./rebar compile

peg:
	erl -pa deps/neotoma/ebin/ -eval "neotoma:file(\"src/em_grammar.peg\", [{transform_module, em_ast}, {output, \"src/\"}]), erlang:halt(0)."
	./rebar compile skip_deps=true

run:
	erl -noshell -pa deps/*/ebin -pa ebin -s emlisp -eval "emlisp:repl()."

clean:
	rm -vrf deps ebin erl_crash.dump
