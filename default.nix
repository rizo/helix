{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;

  onix = import
    (builtins.fetchGit {
      url = "https://github.com/odis-labs/onix.git";
      rev = "93f010fc3ca3613790a30f8f1919b601b1583361";
    })
    {
      inherit pkgs ocamlPackages;
      verbosity = "debug";
    };

  repo = {
    url = "https://github.com/ocaml/opam-repository.git";
    rev = "ff615534bda0fbb06447f8cbb6ba2d3f3343c57e";
  };

  env = onix.env {
    path = ./.;
    gitignore = ./.gitignore;
    deps = { "ocaml-system" = "*"; };
    vars = {
      with-dev-setup = true;
      with-test = true;
      with-doc = true;
    };
  };
in
{
  lock = env.lock;

  shell = pkgs.mkShell {
    inputsFrom = [ env.pkgs.helix ];
    buildInputs = [ pkgs.esbuild ];
    shellHook = ''
      export PS1="[\[\033[1;34m\]nix\[\033[0m\]]\$ "
    '';
  };
}
