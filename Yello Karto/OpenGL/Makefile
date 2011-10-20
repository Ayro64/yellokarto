OCAML = ocamlopt
OCAMLFLAGS = -I +lablGL
OCAMLLD = unix.cmxa lablgl.cmxa lablglut.cmxa

cube: cube.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o cube cube.ml

clean::
	rm -f *~ *.o *.cm? cube