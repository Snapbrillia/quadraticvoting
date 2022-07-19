# our packages overlay
final: prev: with final; {

  inherit (quadraticvotingProject.args) compiler-nix-name;

  # The is used by nix/regenerate.sh to pre-compute package list to avoid double evaluation.
  genProjectPackages = lib.genAttrs
    (lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
      quadraticvotingProject.hsPkgs))
    (name: lib.attrNames quadraticvotingProject.pkg-set.options.packages.value.${name}.components.exes);

  cabal = haskell-nix.tool compiler-nix-name "cabal" {
    version = "latest";
    inherit (quadraticvotingProject) index-state;
  };

  hlint = haskell-nix.tool compiler-nix-name "hlint" {
    version = "3.2.7";
    inherit (quadraticvotingProject) index-state;
  };

  ghcid = haskell-nix.tool compiler-nix-name "ghcid" {
    version = "0.8.7";
    inherit (quadraticvotingProject) index-state;
  };

  haskell-language-server = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "latest";
    inherit (quadraticvotingProject) index-state;
  };

  haskellBuildUtils = prev.haskellBuildUtils.override {
    inherit compiler-nix-name;
    inherit (quadraticvotingProject) index-state;
  };
}
