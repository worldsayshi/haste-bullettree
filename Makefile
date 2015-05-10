
all: editor editor.js

editor.js: editor.hs
	hastec editor.hs

editor:
	ghc --make editor.hs

clean:
	-rm -r main
	-rm *~
	-rm editor.hi
	-rm editor.o

distclean: clean
	-rm editor
	-rm editor.js
