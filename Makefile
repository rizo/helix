
.PHONY: lock
lock:
	nix develop -f default.nix -j8 -v lock

.PHONY: shell
shell:
	nix develop -f default.nix -j8 -i -k TERM -k PATH -k HOME -v shell

.PHONY: build
build:
	dune build

.PHONY: watch
watch:
	dune build -w

bundle:
	esbuild _build/default/examples/basic.bc.js --bundle --outdir=examples

bundle-demo-rescript:
	esbuild lib/es6/examples/demo/Demo.bs.js --bundle --outdir=examples/demo

rescript-serve-tests:
	npm exec -- \
		esbuild lib/es6/examples/demo-rescript/Demo.bs.js --bundle --servedir=examples/demo-rescript --outfile=examples/demo-rescript/Demo.bs.js
