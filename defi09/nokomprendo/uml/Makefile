
UML = $(shell find . -name "*.uml")
SVG = $(UML:.uml=.svg)

.PHONY: all 

all: $(SVG)

%.svg: %.uml
	plantuml -Tsvg $<

clean:
	rm -f $(SVG)

