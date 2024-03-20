{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "8d3ed79ae8875587a2bdcb9d3cbb445fcfbbf5ce";
  }) { inherit pkgs; verbosity = "warning"; };  

in onix.env {
  path = ./.;
  deps = { "ocaml-base-compiler" = "5.1.0"; };
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };
}
