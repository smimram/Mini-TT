SOURCES = minitt.ml parser.mly lexer.mll main.ml
RESULT = minitt
ANNOTATE = true

all: dnc
	./minitt test.tt

conflicts:
	menhir --explain parser.mly && less parser.conflicts && rm parser.conflicts

include OCamlMakefile
