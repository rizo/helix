{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "b5f194dfb7c52cfd317d81679116478d2a959792";
  }) { inherit pkgs; };  

in onix.env {
  path = ./.;
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };
}