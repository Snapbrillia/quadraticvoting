# our packages overlay
final: prev: with final; {

  inherit (quadraticvoting-project.args) compiler-nix-name;

  # The is used by nix/regenerate.sh to pre-compute package list to avoid double evaluation.
  genProjectPackages = lib.genAttrs
    (lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
      quadraticvoting-project.hsPkgs))
    (name: lib.attrNames quadraticvoting-project.pkg-set.options.packages.value.${name}.components.exes);

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (quadraticvoting-project) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (quadraticvoting-project) index-state;
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    inherit (quadraticvoting-project) index-state;
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "latest";
    inherit (quadraticvoting-project) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (quadraticvoting-project) index-state;
  };
}
