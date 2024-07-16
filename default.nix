{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "2af576afb5ee0f5485deb534697d805b22d8100c";
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
