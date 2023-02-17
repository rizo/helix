{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;

  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "083611626d1aaae530f40e53603a30d9d48677a7";
  }) {
    inherit pkgs ocamlPackages;
    verbosity = "debug";
  };

  repo = {
    url = "https://github.com/ocaml/opam-repository.git";
    rev = "0fd96b90e04599bcce3b6ae8ba54febdafeddb11";
  };
  path = ./.;
  gitignore = ./.gitignore;
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };

  jsoo = onix.env {
    inherit repo path gitignore vars;
    lock = ./onix-lock-jsoo.json;
    deps = {
      "ocaml-system" = "*";
      "reason" = "*";
      "js_of_ocaml-compiler" = "*";
    };
  };

  melange = onix.env {
    inherit repo path gitignore vars;
    lock = ./onix-lock-melange.json;
    deps = {
      "ocaml-system" = "*";
      "reason" = "*";
      "melange" = "*";
    };
    overlay = self: super: {
      "melange" = super.melange.overrideAttrs (superAttrs: {
        postInstall = ''
          mkdir -p $out/lib/melange
          mv $out/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib/melange/melange $out/lib/melange/melange
          cp -r $out/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib/melange/runtime $out/lib/melange/runtime
        '';
      });
    };
  };
in { inherit jsoo melange; }
