OCAMLFLAGS =  -I +sdl -I +lablgtk2 -I +lablGL
OCAMLLD = sdl.cmxa sdlloader.cmxa bigarray.cmxa unix.cmxa lablgl.cmxa lablgtk.cmxa lablgtkgl.cmxa -ccopt -L.
OCAMLOPT = ocamlopt

cube: libgtkgl-2.0.so moteur3D.ml traitement_image.ml modelisation.ml yellokarto.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -o yellokarto moteur3D.ml traitement_image.ml modelisation.ml yellokarto.ml
	rm -f *~ *.o *.cm?

libgtkgl-2.0.so:
	-ln -s /usr/lib64/libgtkgl-2.0.so.1 libgtkgl-2.0.so

clean:
	rm -f *~ *.o *.cm? 
