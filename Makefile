.PHONY: default
default: test

vpath %.hs src/gcj-lib

%.zip %.out: YEAR=$(subst Y,,$(word 1,$(subst ., ,$@)))
%.zip %.out: ROUND=$(word 2,$(subst ., ,$@))
%.zip %.out: PROBLEM=$(word 3,$(subst ., ,$@))
%.zip %.out: SIZE=$(word 4,$(subst ., ,$@))
%.verify: VERIFY=--verify


.SECONDEXPANSION:
%.out: Y$$(YEAR)/$$(ROUND)/$$(PROBLEM).hs
	stack build --exec "gcj $(VERIFY) $(YEAR)-$(ROUND)-$(PROBLEM)" < Y$(YEAR).$(ROUND).$(PROBLEM)-$(SIZE).in > $@
	grep ^! $@ && exit 1 || exit 0

%.zip: Y$$(YEAR)/$$(ROUND)/$$(PROBLEM).hs src/gcj-app/Main.hs src/gcj-lib/GCJ.hs src/gcj-lib/Lib.hs gcj.cabal stack.yaml
	zip $@ $^

.SECONDARY:

.PHONY: %.run
%-sample.run: Y2017.R1B.%.zip Y2017.R1B.%.sample.out ;
%-small.run: Y2017.R1B.%.zip Y2017.R1B.%.small.out ;
%-large.run: Y2017.R1B.%.zip Y2017.R1B.%.large.out ;
%-small2.run: Y2017.R1B.%.zip Y2017.R1B.%.small2.out ;
%.verify: %.run ;
