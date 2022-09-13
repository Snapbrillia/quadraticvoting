############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitrev # Version info (git revision)
, projectPackagesExes
}:
let

  inherit (haskell-nix) haskellLib;

  projectPackageNames = builtins.attrNames projectPackagesExes;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/

in
haskell-nix.cabalProject' ({ pkgs
                           , lib
                           , config
                           , buildProject
                           , ...
                           }: 
  let
    haskell-language-server = pkgs.buildPackages.buildPackages.haskell-language-server;
    haskell-language-server-wrapper = 
      pkgs.writeShellScriptBin "haskell-language-server-wrapper" 
        ''${haskell-language-server}/bin/haskell-language-server "$@"'';
  in
  {
  name = "quadraticvoting";
  src = haskellLib.cleanSourceWith {
    src = ../.;
    name = "quadraticvoting-src";
    filter = name: type: (lib.cleanSourceFilter name type)
      && (haskell-nix.haskellSourceFilter name type)
      # removes socket files
      && lib.elem type [ "regular" "directory" "symlink" ];
  };
  compiler-nix-name = "ghc8107";
  cabalProjectLocal = ''
    allow-newer: terminfo:base
  '';
  shell = {
    name = "cabal-dev-shell";

    packages = lib.attrVals projectPackageNames;

    # These programs will be available inside the nix-shell.
    nativeBuildInputs = with pkgs.buildPackages.buildPackages; [
        nix-prefetch-git
        pkg-config
        hlint
        ghcid
        haskell-language-server-wrapper
        haskell-language-server
        cabalWrapped
        # we also add cabal (even if cabalWrapped will be used by default) for shell completion:
        cabal
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    withHoogle = false;
  };
  modules =
    let
      inherit (config) src;
      # setGitRev is a postInstall script to stamp executables with
      # version info. It uses the "gitrev" argument, if set. Otherwise,
      # the revision is sourced from the local git work tree.
      setGitRev = ''${pkgs.buildPackages.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*'';
    in
    [
      ({ pkgs, ... }: {
        # Use the VRF fork of libsodium
        packages = lib.genAttrs [ "cardano-crypto-praos" "cardano-crypto-class" ] (_: {
          components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        });
      })
      ({ pkgs, ... }: {
        # stamp executables with the git revision, strip/rewrite:
        packages = lib.mapAttrs
          (name: exes: {
            components.exes = lib.genAttrs exes (exe: {
              postInstall = ''
                ${setGitRev}
                ${lib.optionalString (pkgs.stdenv.hostPlatform.isMusl) ''
                  ${pkgs.buildPackages.binutils-unwrapped}/bin/*strip $out/bin/*
                ''}
              '';
            });
          })
          projectPackagesExes;
      })
      {
        packages = lib.genAttrs projectPackageNames
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      # Musl libc fully static build
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl (
        let
          # Module options which adds GHC flags and libraries for a fully static build
          fullyStaticOptions = {
            enableShared = false;
            enableStatic = true;
          };
        in
        {
          packages = lib.genAttrs projectPackageNames (name: fullyStaticOptions);
          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }
      ))
    ];
})
