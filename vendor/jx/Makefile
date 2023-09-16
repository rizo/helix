
.PHONY: build
build:
	dune build

.PHONY: watch
watch:
	dune build -w

.PHONY: test
test:
	dune runtest

.PHONY: test-watch
test-watch:
	dune runtest -w

.PHONY: doc
doc:
	dune build @doc
	@ls _build/default/_doc/_html/index.html

.PHONY: clean
clean:
	dune clean

.PHONY: lock
lock:
	nix develop -f default.nix lock

.PHONY: shell
shell:
	nix develop -f default.nix -j auto -i -k TERM -k PATH -k HOME -v shell
