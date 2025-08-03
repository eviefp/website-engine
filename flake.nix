{
  description = "website-engine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05-small";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pico-css = {
      url = "github:picocss/pico?ref=main";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix, pico-css }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          treefmt-config = {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              cabal-fmt.enable = true;
              fourmolu.enable = true;
              fourmolu.package = pkgs.haskell.packages.ghc984.fourmolu;
            };
          };
          treefmt = (treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;
        in
        {
          formatter = treefmt.wrapper;

          checks = {
            fmt = treefmt.check self;
            hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; } ''
              cd ${./.}
              hlint src spec app
              touch $out
            '';
          };

          packages.default = pkgs.haskell.packages.ghc984.callCabal2nix "website-engine" ./. { };

          devShells.default = pkgs.mkShell {
            name = "website-engine-shell";
            buildInputs = [
              pkgs.zlib.dev
              pkgs.haskell.compiler.ghc984
              pkgs.haskell.packages.ghc984.cabal-install
              pkgs.haskell.packages.ghc984.cabal2nix
              pkgs.haskell.packages.ghc984.haskell-language-server
              pkgs.http-server
            ];
            shellHook = ''
              ln -sf ${pico-css}/css/pico.min.css site/css/pico.min.css
            '';
          };
        }
      );
}
