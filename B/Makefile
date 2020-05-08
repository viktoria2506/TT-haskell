.PHONY: pack all run clean

all:
	alex Lexer.x
	happy Parser.y
	ghc Main.hs

run:
	./Main

clean:
	-rm -rf *.o *.hi Main

pack:
	zip vika.zip -r Makefile Lexer.x Parser.y Expression.hs Main.hs
