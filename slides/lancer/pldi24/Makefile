FILE = pldi24

DIFF_COMMIT ?= 6a4e39fb40c05d45599fcca35337088389ff5d54

THIS_REPO ?= git@github.com:GaloisInc/5stars.git

PAPER_DIR ?= writeups/pldi24
UNDO_PAPER_DIR ?= ../..

build:
	latexmk -pdf -auxdir=aux $(FILE)

clean:
	latexmk -c -auxdir=aux

.PHONY: build clean

diff:
	rm -rf tmp/diff
	rm -rf tmp/old-src
	mkdir -p tmp/diff/old
	mkdir -p tmp/diff/new
	mkdir -p tmp/diff/delta
	
	make build
	latexpand $(FILE).tex > tmp/diff/new/$(FILE).tex
	cp *.csv tmp/diff/new/
	cp *.bib tmp/diff/new/
	cp *.sty tmp/diff/new/
	
	git clone --depth=1 $(THIS_REPO) tmp/old-src
	cd tmp/old-src && ( \
	  git fetch origin $(DIFF_COMMIT) && \
	  git checkout $(DIFF_COMMIT) && \
	  cd $(PAPER_DIR) && ( \
	    latexmk -pdf -auxdir=aux $(FILE) && \
	    latexpand $(FILE).tex > $(UNDO_PAPER_DIR)/../diff/old/$(FILE).tex && \
	    cp *.csv $(UNDO_PAPER_DIR)/../diff/old/ && \
	    cp *.bib $(UNDO_PAPER_DIR)/../diff/old/ && \
	    cp *.sty $(UNDO_PAPER_DIR)/../diff/old/    \
	))
	
	cd tmp/diff/new && ( \
	  latexdiff ../old/$(FILE).tex $(FILE).tex > ../delta/$(FILE).tex && \
	  cp *.csv ../delta/ && \
	  cp *.bib ../delta/ && \
	  cp *.sty ../delta/ \
	)
	
	cd tmp/diff/delta && ( \
	  (latexmk -pdf -auxdir=aux -interaction=nonstopmode $(FILE).tex || true) \
	)
	
	cp tmp/diff/delta/$(FILE).pdf $(FILE)-diff.pdf
