
all: editor editor.js

sandbox: editor-sandox editor.js

editor.js: editor.hs
	hastec editor.hs

editor-sandbox:
	ghc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.8.3-packages.conf.d --make editor.hs

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
