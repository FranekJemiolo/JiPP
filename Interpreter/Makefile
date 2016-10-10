COLL=ParJam.hs TestJam.hs ErrM.hs AbsJam.hs LexJam.hs SkelJam.hs DocJam.txt
OBJS=Jam.cf Interpreter.hs Interpret.hs
all: interpret
interpret: Jam.cf Interpreter.hs Interpret.hs
	mkdir Interpreter
	cp $(OBJS) Interpreter/
	cd Interpreter && bnfc -m -haskell Jam.cf
	cd Interpreter && happy -gca ParJam.y
	cd Interpreter && alex -g LexJam.x
	cp Makefile Interpreter/
	cd Interpreter && ghc --make Interpreter.hs
clean:
	rm -rf Interpreter
