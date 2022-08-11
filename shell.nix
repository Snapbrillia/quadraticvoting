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

  commandHelp =
    ''
      echo "
        Commands:
          * nix flake lock --update-input <iohkNix|haskellNix> - update nix build input
          * qvf-cli - run to generate tx components
      "
    '';
  haveGlibcLocales = false;

  shell = project.shellFor {
    name = "dev-shell";

    inherit withHoogle;

    packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);

    tools = {
      haskell-language-server = {
        # version = "latest";
        inherit (project) index-state;
      };
    };

    # These programs will be available inside the nix-shell.
    nativeBuildInputs = with haskellPackages; with qvf-cli-packages; [
      cabalWrapped
      haskell-language-server
      ghcid
      haskellBuildUtils
      pkgs.graphviz
      nixWrapped
      pkgconfig
      pkgs.git
      pkgs.hlint
      pkgs.moreutils
      pkgs.pstree
      pkgs.time
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  };

  ## XXX: remove this once people retrain their muscle memory:
  dev = project.shell;


  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
in
 shell // { inherit dev; }
