{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "b5f194dfb7c52cfd317d81679116478d2a959792";
  }) { inherit pkgs; };

  path = ./.;
  vars = {
    with-dev-setup = true;
    with-test = true;
    with-doc = true;
  };

  jsoo = onix.env {
    inherit path vars;
    lock = ./onix-lock-jsoo.json;
    deps = {
      "ocaml-system" = "*";
      "reason" = "*";
      "js_of_ocaml-compiler" = "*";
    };
  };

  melange = onix.env {
    inherit path vars;
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
          mv $out/lib/ocaml/${self.ocaml.version}/site-lib/melange/melange $out/lib/melange/melange
          cp -r $out/lib/ocaml/${self.ocaml.version}/site-lib/melange/runtime $out/lib/melange/runtime
        '';
      });
    };
  };
in { inherit jsoo melange; }
