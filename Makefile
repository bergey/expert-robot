build: configure Setup
	./Setup build

test: build
	./dist/build/expert-robot/expert-robot
# TODO include these tests from Main
	# stack exec expert-robot <<< "(+ 1 2 3)"
	# stack exec expert-robot <<< "(+ 1 2 (* 4 5))"
	# stack exec expert-robot <<< "(+ 1 2 (foo 4 5))"
	# stack exec expert-robot <<< "(+ 1 2 (foo1 4 5))"
	# stack exec expert-robot <<< "(0foo 1 2 (foo' 4 5))"

configure: in-shell Setup
	./Setup configure

in-shell:
	@test ${IN_NIX_SHELL} || (echo "Run nix-shell first" && exit 1)

Setup: Setup.hs
	ghc Setup.hs

src/Lex.hs: src/Lex.x
	alex src/Lex.x
