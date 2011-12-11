OCAMLFLAGS = -I +sdl -I +lablgtk2 -I +lablGL
OCAMLLD = sdl.cmxa sdlloader.cmxa bigarray.cmxa unix.cmxa lablgl.cmxa lablgtk.cmxa lablgtkgl.cmxa -ccopt -L.
OCAMLOPT = ocamlopt

program: libgtkgl-2.0.so skybox.ml  traitement_image.ml modelisation.ml moteur3D.ml yellokarto.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -o yellokarto skybox.ml traitement_image.ml modelisation.ml moteur3D.ml yellokarto.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i modelisation.ml > modelisation.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i yellokarto.ml > yellokarto.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i traitement_image.ml > traitement_image.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i moteur3D.ml > moteur3D.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i skybox.ml > skybox.mli
	rm -f *~ *.o *.cm? 


libgtkgl-2.0.so:
	-ln -s /usr/lib64/libgtkgl-2.0.so.1 libgtkgl-2.0.so

clean:
	rm -f *~ *.o *.cm? *mli yellokarto libgtkgl-2.0.so *.yk

