HC=ghc
HC_OPTS=-cpp $(EXTRA_HC_OPTS)

all:
	$(HC) --make -i. -o fl Main.hs

clean:
	(cd Syntax; make clean)
	(cd Semantics; make clean)
	rm -f *.hi *.o */*.hi */*.o */*/*.hi */*/*.o TestFlower TestFlower.hs

.SUFFIXES: .o .hs .hi .lhs .hc .s
.PHONY: all syntax clean

# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.o-boot.hi-boot:
	@:

.lhs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)

.hs-boot.o-boot:
	$(HC) -c $< $(HC_OPTS)
