OCAMLMAKEFILE = ../OCamlMakefile

INCDIRS = +camlp4

SOURCES = bio_string_ext.ml bio_string_ext.mli \
	bio_fasta.ml bio_fasta.mli \
	bio_genbank.ml bio_genbank.mli \
	bio_csv.ml bio_csv.mli \
	bio_newick.ml bio_newick.mli \
	bio.ml bio.mli

RESULT  = ocaml_bio

THREADS = yes

LIBS = str
PACKS = seq core_extended

LIBINSTALL_FILES = bio_string_ext.mli bio_string_ext.cmi \
	bio_fasta.mli bio_fasta.cmi \
	bio_genbank.mli bio_genbank.cmi \
	bio_csv.mli bio_csv.cmi \
	bio_newick.mli bio_newick.cmi \
	bio.mli bio.cmi \
	ocaml_bio.cma ocaml_bio.cmxa ocaml_bio.a

all:	byte-code-library native-code-library

install:	libinstall
uninstall:	libuninstall

clean::  clean-doc

-include $(OCAMLMAKEFILE)
