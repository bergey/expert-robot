build:
	alex src/Lex.x
	stack build

test: build
	stack exec expert-robot
# TODO include these tests from Main
	# stack exec expert-robot <<< "(+ 1 2 3)"
	# stack exec expert-robot <<< "(+ 1 2 (* 4 5))"
	# stack exec expert-robot <<< "(+ 1 2 (foo 4 5))"
	# stack exec expert-robot <<< "(+ 1 2 (foo1 4 5))"
	# stack exec expert-robot <<< "(0foo 1 2 (foo' 4 5))"
