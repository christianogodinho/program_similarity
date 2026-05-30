all:
	ghc main.hs -o programa

run: all
	./programa res.txt sep.txt c1.txt c2.txt

clean:
	rm -f *.o *.hi programa
