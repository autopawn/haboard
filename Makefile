COMMON_SOURCES = Game.hs Piece.hs

foxhounds:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FoxAndHounds.hs -o bin/foxhounds

fivefieldkono:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FiveFieldKono.hs -o bin/fivefieldkono

damas:
	mkdir -p bin
	ghc $(COMMON_SOURCES) Damas.hs -o bin/damas
