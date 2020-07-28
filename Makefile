COMMON_SOURCES = Game.hs Piece.hs

foxhounds:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FoxAndHounds.hs -o bin/foxhounds

fivefieldkono:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FiveFieldKono.hs -o bin/fivefieldkono

connect4:
	mkdir -p bin
	ghc $(COMMON_SOURCES) Connect4.hs -o bin/connect4
