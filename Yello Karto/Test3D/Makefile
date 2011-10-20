# Makefile for fearofthemap project.

# Add your files here :
ML= traitement_image.ml yellokarto.ml
#MLI=types.mli image.mli io.mli point.mli trianle.mli parser.mli image_functions.mli grapheng.mli process_types.mli process.mli args_manager.mli gui.mli

# Please change this with careful !
# Do not touch if you don't know what you're doing.
OCAMLFLAGS= -I +sdl -I +lablgtk2
OCAMLLD=lablgtk.cmxa sdl.cmxa sdlloader.cmxa bigarray.cmxa # GL.cmxa Glu.cmxa Glut.cmxa

# You Can't Touch This - MC Hammer
# Now please do not modify what is following.
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
EXEC=yellokarto

CMX=$(ML:%.ml=%.cmx)
CMI=$(MLI:%.mli=%.cmi)

all: $(EXEC)
	rm -rf *.cm[xio] *.o
	rm -f .depend

.SUFFIXES: .ml .mli .cmi .cmx

$(EXEC): $(CMX)
	$(OCAMLOPT) $(OCAMLLD) $(OCAMLFLAGS) $^ -o $(EXEC)

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $< 

.mli.cmi:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

# Dependencies

include .depend

.depend:
	rm -f .depend
	$(OCAMLDEP) $(OCAMLLD) $(MLI) $(ML) > .depend

# Clean me all that shit !

clean:
	rm -rf *.cm[xio] *.o
	rm -f .depend

clear:
	rm -rf *.cm[xio] *.o
	rm -f .depend

distclean: clean
	find . -name "*~" -exec rm '{}' \;
	rm -f $(EXEC) interface

# EOF
