MKSITE_ML_SRCS = src/lexer.ml src/parser.ml src/mksite.ml
MKSITE_MLI_SRCS = src/lexer.mli src/parser.mli
MKSITE_CMX_OBJS = $(patsubst src/%.ml,build/%.cmx,$(MKSITE_ML_SRCS))
MKSITE_CMI_OBJS = $(patsubst src/%.mli,build/%.cmi,$(MKSITE_MLI_SRCS))

OCAMLOPT = ocamlopt
OCAMLC = ocamlc

.PHONY: cleanbuild clean all

all: build/mksite

clean:
	rm -rf build
	mkdir build

cleanbuild: clean all

build/%.cmi: src/%.mli
	$(OCAMLC) -I build/ -c $< -o $@

build/%.cmx: src/%.ml
	$(OCAMLOPT) -I build/ -c $< -o $@

build/mksite: $(MKSITE_CMI_OBJS) $(MKSITE_CMX_OBJS)
	$(OCAMLOPT) unix.cmxa $(MKSITE_CMX_OBJS) -o $@
