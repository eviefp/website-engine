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
    let
      # overlays are a way to basically edit packages in a `nixpkgs` instance
      # we use it to update a few Haskell packages and to add the engine to the
      # list of available Haskell packages
      overlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ghc9102 = prev.haskell.packages.ghc9102.override (prevArgs: {
              overrides = prev.lib.composeExtensions (prevArgs.overrides or (_: _: { })) (
                finalHaskell: prevHaskell: {
                  # needed for the pandoc update; once nixpkgs will have pandoc >= 3.7.0.2
                  # then we can remove this whole section up until pandoc
                  commonmark-pandoc = prevHaskell.callHackageDirect
                    {
                      pkg = "commonmark-pandoc";
                      ver = "0.2.3";
                      sha256 = "sha256-qMfPEBq7kjgFZAxdc84LgoIEzuAS/wAb2CH3LGN3wiE=";
                    }
                    { };
                  texmath = prevHaskell.callHackageDirect
                    {
                      pkg = "texmath";
                      ver = "0.12.10.3";
                      sha256 = "sha256-kSUGL4nLhroyIntE9vUfec6n6qefgfsZA7FxVM52NQg=";
                    }
                    { };
                  typst = prevHaskell.callHackageDirect
                    {
                      pkg = "typst";
                      ver = "0.8.0.1";
                      sha256 = "sha256-sv6WF5vfSz+YOvnCBuZxM9pEpqbFGs6B3fsVDZKvFGY=";
                    }
                    { };
                  typst-symbols = prevHaskell.callHackageDirect
                    {
                      pkg = "typst-symbols";
                      ver = "0.1.8.1";
                      sha256 = "sha256-y4OlWfqMRapDItRBQs19RBXzT3ezsATGi1is1Fdgfl0=";
                    }
                    { };
                  Diff = prevHaskell.callHackageDirect
                    {
                      pkg = "Diff";
                      ver = "1.0.2";
                      sha256 = "sha256-fRxDSt8/CSGyUrmGNwF22ASjEzIRGifNk3M9j9HrC2g=";
                    }
                    { };
                  citeproc = prevHaskell.callHackageDirect
                    {
                      pkg = "citeproc";
                      ver = "0.9.0.1";
                      sha256 = "sha256-sRCT3+BWhmgJWcc7pJzLhdGifgqmmXz+4kdEPDracXM=";
                    }
                    { };
                  pandoc = prevHaskell.callHackageDirect
                    {
                      pkg = "pandoc";
                      ver = "3.7.0.2";
                      sha256 = "sha256-vqmg8sgObF+XTwFtcq2hrmjPczarf8v6TC0FIAtD3ao=";
                    }
                    { };

                  # add the engine to the list of available packages
                  website-engine = prevHaskell.callCabal2nix "website-engine" self { };
                }
              );
            });
          };
        };
      };
    in
    (flake-utils.lib.eachDefaultSystem
      (system:
        let

          # use this for tooling, higher chances of nixos cache hits
          pkgs = import nixpkgs {
            inherit system;
          };
          # use this for haskellPackages
          pkgs-pandoc-upgrade = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };

          # used for formatting
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

          # used for brevity :)
          haskellPackages = pkgs-pandoc-upgrade.haskell.packages.ghc9102;

          # our package
          website-engine = haskellPackages.callCabal2nix "website-engine" ./. { };
        in
        {
          # run with `nix fmt`
          formatter = treefmt.wrapper;

          # run with `nix flake check`
          checks = {
            # run the formatter in check mode (errors instead of fixing)
            fmt = treefmt.check self;

            # haskell lints
            hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; } ''
              cd ${./.}
              hlint src spec app
              touch $out
            '';

            # by building the project, nix also runs the tests automatically
            hstests = pkgs.runCommand "hstests" { buildInputs = [ website-engine ]; } ''
            '';
          };

          # expose the default executable; run with `nix run .#default`
          packages.default = website-engine;

          # default shell; run with `nix develop` or use `direnv`
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
      )
    // {
      overlays.default = overlay;
    });
}
