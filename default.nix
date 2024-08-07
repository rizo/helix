{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "41bf9e887fa8f1399ac328f1868d6d2ba27aab9f";
  }) {
    inherit pkgs ocamlPackages;
    verbosity = "info";
  };

in onix.env {
  path = ./.;
  deps = {
    "ocaml-system" = "5.1.1";
  };
  roots = [ ./helix.opam ];
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };
}
