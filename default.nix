{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "a5de90d3437848d048ed73b7e9aa18fb57702ae7";
  }) { inherit pkgs; verbosity = "warning"; };  

in onix.env {
  path = ./.;
  env-file = ./.onix.env;
  deps = {
    "ocaml-system" = "*";
    # "stdweb" = ./vendor/stdweb/stdweb.opam;
    # "jx" = ./vendor/jx/jx.opam;
  };
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };
}
