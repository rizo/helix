{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "07c37d51af30158a0ef4a49eb0ecc22f9b6fdf41";
  }) {
    inherit pkgs;
    verbosity = "info";
  };

in onix.env {
  path = ./.;
  deps = {
    "ocaml-base-compiler" = "5.3.0";
  };
  roots = [ ./helix.opam ];
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };
}
