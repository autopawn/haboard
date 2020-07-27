COMMON_SOURCES = Game.hs Piece.hs

pegsolitaire:
	mkdir -p bin
	ghc $(COMMON_SOURCES) PegSolitaire.hs -o bin/pegsolitaire

foxhounds:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FoxAndHounds.hs -o bin/foxhounds

fivefieldkono:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FiveFieldKono.hs -o bin/fivefieldkono
