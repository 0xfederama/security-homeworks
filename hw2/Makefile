.DEFAULT_GOAL := test
BUILD_DIR=_build/default/

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: test_hw1
test_hw1:  ## Run all the tests for the typechecker                                    
	opam exec -- dune exec test/test_hw1.exe

.PHONY: test_hw2
test_hw2:  ## Run all the tests for the typechecker                                    
	opam exec -- dune exec test/test_hw2.exe

.PHONY: test
test: build test_hw1 test_hw2 ## Run all the tests
