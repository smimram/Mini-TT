test:
	@dune exec ./minitt.exe test.tt

conflicts:
	menhir --explain parser.mly && less parser.conflicts && rm parser.conflicts
