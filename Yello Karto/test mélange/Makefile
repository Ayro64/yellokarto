OCAMLFLAGS = -I +lablgtk2 -I +lablGL
OCAMLLD = unix.cmxa lablgtk.cmxa lablgtkgl.cmxa lablgl.cmxa #lablglut.cmxa
OCAMLOPT=ocamlopt

cube: melange.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -o melange melange.ml

clean::
	rm -f *~ *.o *.cm? melange