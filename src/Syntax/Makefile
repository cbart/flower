all:
	happy -gca ParFlower.y
	alex -g LexFlower.x
	(latex DocFlower.tex; dvips DocFlower.dvi -o DocFlower.ps)
	cp TestFlower.hs ../
	(cd .. && ghc --make TestFlower.hs -o TestFlower)
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocFlower.ps
distclean: clean
	-rm -f DocFlower.* LexFlower.* ParFlower.* LayoutFlower.* SkelFlower.* PrintFlower.* TestFlower.* AbsFlower.* ../TestFlower ErrM.* SharedString.* Flower.dtd XMLFlower.* Makefile*
