build:
	alex src/Lex.x -o src/Main.hs
	stack build

test: build
	stack exec tiger <<< "/* A program to solve the 8-queens problem */"
	stack exec tiger <<< "             (for i := 0 to N-1"
	stack exec tiger <<< "do (for j := 0 to N-1"
	stack exec tiger <<< "(+ 1 2 3)"
	stack exec tiger <<< "(+ 1 2 (* 4 5))"
	stack exec tiger <<< "let var x := 5 in x end"
	stack exec tiger <<< "if x & y then 1 else 2"
	stack exec tiger <<< "function try(c:int) = if c > n then print(c) else try(2*c)"
	stack exec tiger <<< "type intArray = array of int"
	stack exec tiger < queens.tig

queens: build
	stack exec tiger < queens.tig
