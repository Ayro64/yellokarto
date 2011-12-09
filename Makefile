OCAMLFLAGS = -I +sdl -I +lablgtk2 -I +lablGL
OCAMLLD = sdl.cmxa sdlloader.cmxa bigarray.cmxa unix.cmxa lablgl.cmxa lablglut.cmxa lablgtk.cmxa lablgtkgl.cmxa -ccopt -L.
OCAMLOPT = ocamlopt

program: libgtkgl-2.0.so skybox.ml moteur3D.ml traitement_image.ml modelisation.ml yellokarto.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -o yellokarto skybox.ml moteur3D.ml traitement_image.ml modelisation.ml yellokarto.ml
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i modelisation.ml > modelisation.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i yellokarto.ml > yellokarto.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i traitement_image.ml > traitement_image.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i moteur3D.ml > moteur3D.mli
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -i skybox.ml > skybox.mli



libgtkgl-2.0.so:
	-ln -s /usr/lib64/libgtkgl-2.0.so.1 libgtkgl-2.0.so

clean:
	rm -f *~ *.o *.cm? *mli 
