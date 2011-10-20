OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
traitement_image: traitement_image.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o traitement_image traitement_image.ml
 
clean::
	rm -f *~ *.o *.cm? traitement_image
