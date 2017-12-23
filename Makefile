game:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

check:
	bash checkenv.sh && bash checktypes.sh

test:
	ocamlbuild -use-ocamlfind test_state.byte && ./test_state.byte

clean:
	ocamlbuild -clean

install-dep:
	opam install lwt yojson camomile lambda-term oUnit
