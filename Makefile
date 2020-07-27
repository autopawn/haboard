COMMON_SOURCES = Game.hs Piece.hs

foxhounds:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FoxAndHounds.hs -o bin/foxhounds

fivefieldkono:
	mkdir -p bin
	ghc $(COMMON_SOURCES) FiveFieldKono.hs -o bin/fivefieldkono

harehounds:
	mkdir -p bin
	ghc $(COMMON_SOURCES) HareAndHounds2.hs -o bin/HareAndHounds
