{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "00720d8a87daef3bbf66eb89e2b7d8efcaf577aa";
  }) {
    inherit pkgs;
    verbosity = "info";
  };

in onix.env {
  path = ./.;
  deps = {
    "ocaml-base-compiler" = "5.2.0";
  };
  roots = [ ./helix.opam ];
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };
}
