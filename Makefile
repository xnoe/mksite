MKSITE_ML_SRCS = $(shell find src -name '*.ml')
MKSITE_CMX_OBJS = $(patsubst src/%.ml,build/%.cmx,$(MKSITE_ML_SRCS))

MKSITE_OBJS = $(MKSITE_CMX_OBJS)

OCAMLOPT = ocamlopt

.PHONY: cleanbuild all

all: build/mksite

clean:
	rm -rf build
	mkdir build

cleanbuild: clean all

build/%.cmx: src/%.ml
	$(OCAMLOPT) -c $< -o $@

build/mksite: $(MKSITE_CMX_OBJS)
	$(OCAMLOPT) unix.cmxa $(MKSITE_CMX_OBJS) -o $@
