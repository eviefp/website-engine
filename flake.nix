{
  description = "website-engine";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
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
              fourmolu.package = pkgs.haskell.packages.ghc9102.fourmolu;
            };
          };
          treefmt = (treefmt-nix.lib.evalModule pkgs treefmt-config).config.build;

          haskellPackages = pkgs.haskell.packages.ghc9102.override {
            overrides = final: prev: {
              # needed for the pandoc update; once nixpkgs will have pandoc >= 3.7.0.2
              # then we can remove this whole section up until pandoc
              commonmark-pandoc = final.callHackageDirect
                {
                  pkg = "commonmark-pandoc";
                  ver = "0.2.3";
                  sha256 = "sha256-qMfPEBq7kjgFZAxdc84LgoIEzuAS/wAb2CH3LGN3wiE=";
                }
                { };
              texmath = final.callHackageDirect
                {
                  pkg = "texmath";
                  ver = "0.12.10.3";
                  sha256 = "sha256-kSUGL4nLhroyIntE9vUfec6n6qefgfsZA7FxVM52NQg=";
                }
                { };
              typst = final.callHackageDirect
                {
                  pkg = "typst";
                  ver = "0.8.0.1";
                  sha256 = "sha256-sv6WF5vfSz+YOvnCBuZxM9pEpqbFGs6B3fsVDZKvFGY=";
                }
                { };
              typst-symbols = final.callHackageDirect
                {
                  pkg = "typst-symbols";
                  ver = "0.1.8.1";
                  sha256 = "sha256-y4OlWfqMRapDItRBQs19RBXzT3ezsATGi1is1Fdgfl0=";
                }
                { };
              Diff = final.callHackageDirect
                {
                  pkg = "Diff";
                  ver = "1.0.2";
                  sha256 = "sha256-fRxDSt8/CSGyUrmGNwF22ASjEzIRGifNk3M9j9HrC2g=";
                }
                { };
              citeproc = final.callHackageDirect
                {
                  pkg = "citeproc";
                  ver = "0.9.0.1";
                  sha256 = "sha256-sRCT3+BWhmgJWcc7pJzLhdGifgqmmXz+4kdEPDracXM=";
                }
                { };
              pandoc = final.callHackageDirect
                {
                  pkg = "pandoc";
                  ver = "3.7.0.2";
                  sha256 = "sha256-vqmg8sgObF+XTwFtcq2hrmjPczarf8v6TC0FIAtD3ao=";
                }
                { };

              website-engine = final.callCabal2nix "website-engine" ./. { };
            };
          };
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

          packages.default = haskellPackages.callCabal2nix "website-engine" ./. { };

          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.website-engine ];
            name = "website-engine-shell";
            buildInputs = [
              pkgs.haskell.compiler.ghc9102

              # use the default executables in order to hopefully hit the nixos cache
              pkgs.haskell.packages.ghc9102.cabal-install
              pkgs.haskell.packages.ghc9102.haskell-language-server

              pkgs.http-server
              pkgs.zlib.dev
            ];
            shellHook = ''
              ln -sf ${pico-css}/css/pico.min.css site/css/pico.min.css
            '';
          };
        }
      );
}
