
.PHONY: build
build:
	dune build

.PHONY: watch
watch:
	dune build -w

.PHONY: jsoo-lock
jsoo-lock:
	nix develop -f default.nix -j8 -v jsoo.lock

.PHONY: jsoo-shell
jsoo-shell:
	nix develop -f default.nix -j8 -i -k TERM -k PATH -k HOME -v jsoo.shell
