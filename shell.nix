let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
in
{ withHoogle ? defaultCustomConfig.withHoogle
, useCabalRun ? true
, customConfig ? {
    inherit withHoogle;
  }
, pkgs ? import ./nix customConfig
}:
with pkgs;
let
  inherit (pkgs) customConfig;
  inherit (customConfig) withHoogle;
  inherit (pkgs.haskell-nix) haskellLib;
  project = qvf-cli-project;

  ## The default shell is defined by flake.nix: (qvf-cli-project = flake.project.${final.system})
  inherit (project) shell;

  ## XXX: remove this once people retrain their muscle memory:
  dev = project.shell;

  commandHelp =
    ''
      echo "
        Commands:
          * nix flake lock --update-input <iohkNix|haskellNix> - update nix build input
          * qvf-cli - run to generate tx components
      "
    '';
  haveGlibcLocales = false;

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
in
 shell // { inherit dev; }
