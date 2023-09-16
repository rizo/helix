{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "b6226a50cac0c107f139d26c73f9738f7d9b5b12";
  }) {
    inherit pkgs;
    verbosity = "info";
  };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };
}
