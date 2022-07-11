{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for plutus-starter.
#
# This file defines various attributes that are used for building and
# developing plutus-starter.
#
########################################################################

let
  
  hackageSrc = builtins.fetchTarball https://github.com/input-output-hk/hackage.nix/archive/master.tar.gz;
  stackageSrc = builtins.fetchTarball https://github.com/input-output-hk/stackage.nix/archive/master.tar.gz;
  haskellSrc = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;

  haskellNix = import haskellSrc {
    # This allows you to override the pins used by `haskell.nix` internally
    sourcesOverride = {
      hackage = hackageSrc;
      stackage = stackageSrc;
    };
  };
  
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs plutus-starter;
  project = plutus-starter.haskell.project;
in
{
  inherit pkgs plutus-starter;

  inherit project;
}
