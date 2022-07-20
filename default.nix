########################################################################
# default.nix -- The top-level nix build file for quadraticvoting.
#
# This file defines various attributes that are used for building and
# developing quadraticvoting.
#
########################################################################
{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, haskellNix ? import sources.haskell-nix {
    pkgs = import sources.nixpkgs { inherit system; };
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  }
, packages ? import ./nix { inherit system sources crossSystem config sourcesOverride haskellNix enableHaskellProfiling; }
  # Whether to build our Haskell packages (and their dependencies) with profiling enabled.
, enableHaskellProfiling ? false
}:
let
  inherit (packages) pkgs plutus-apps;
  inherit (plutus-apps) haskell;
in
rec {
  inherit pkgs plutus-apps;

  inherit (plutus-apps) web-ghc;

  inherit (haskell.packages.plutus-pab-executables.components.exes)
    plutus-pab-examples
    plutus-uniswap;

  webCommon = pkgs.callPackage sources.web-common { inherit (plutus-apps.lib) gitignore-nix; };

  plutus-use-cases = pkgs.recurseIntoAttrs (pkgs.callPackage ./plutus-use-cases {
    inherit haskell;
  });

  pab-cli = plutus-apps.haskell.packages.plutus-pab-executables.components.exes.pab-cli;

  plutus-chain-index = plutus-apps.haskell.packages.plutus-chain-index.components.exes.plutus-chain-index;

  marconi = plutus-apps.haskell.packages.plutus-chain-index.components.exes.marconi;

  docs = import ./nix/docs.nix { inherit pkgs plutus-apps; };

  # This builds a vscode devcontainer that can be used with the plutus-starter project (or probably the plutus project itself).
  devcontainer = import ./nix/devcontainer/plutus-devcontainer.nix { inherit pkgs plutus-apps; };

  build-and-push-devcontainer-script = import ./nix/devcontainer/deploy/default.nix { inherit pkgs plutus-apps; };
}
