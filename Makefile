COMMON_SOURCES = Game.hs Piece.hs

connectfour:
	mkdir -p bin
	ghc $(COMMON_SOURCES) ConnectFour.hs -o bin/connectfour
	
foxhounds:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FoxAndHounds.hs -o bin/foxhounds

fivefieldkono:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FiveFieldKono.hs -o bin/fivefieldkono

