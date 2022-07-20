{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, packages ? import ./. { inherit system enableHaskellProfiling sources sourcesOverride; }
}:
let
  #inherit (packages) pkgs plutus-apps plutus-playground pab-nami-demo docs webCommon;
  inherit (packages) pkgs plutus-apps docs webCommon;
  inherit (pkgs) stdenv lib utillinux nixpkgs-fmt;
  inherit (plutus-apps) haskell nix-pre-commit-hooks;

  # Feed cardano-wallet, cardano-cli & cardano-node to our shell.
  # This is stable as it doesn't mix dependencies with this code-base;
  # the fetched binaries are the "standard" builds that people test.
  # This should be fast as it mostly fetches Hydra caches without building much.
  cardano-wallet = (import sources.flake-compat {
    inherit pkgs;
    src = builtins.fetchTree
      {
        type = "github";
        owner = "input-output-hk";
        repo = "cardano-wallet";
        rev = "f6d4db733c4e47ee11683c343b440552f59beff7";
        narHash = "sha256-3oeHsrAhDSSKBSzpGIAqmOcFmBdAJ5FR02UXPLb/Yz0=";
      };
  }).defaultNix;
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "1.34.1";
      sha256 = "1hh53whcj5y9kw4qpkiza7rmkniz18r493vv4dzl1a8r5fy3b2bv";
    })
    { };

  nixFlakesAlias = pkgs.runCommand "nix-flakes-alias" { } ''
    mkdir -p $out/bin
    ln -sv ${pkgs.nixFlakes}/bin/nix $out/bin/nix-flakes
  '';

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = with pkgs; [
    cacert
    editorconfig-core-c
    ghcid
    jq
    nixFlakesAlias
    nixpkgs-fmt
    nodejs
    plantuml
    shellcheck
    sqlite-interactive
    stack
    yq
    z3
    zlib
  ];

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus-apps; [
    cabal-install
    cardano-node.cardano-cli
    cardano-node.cardano-node
    cardano-wallet.packages.${pkgs.system}.cardano-wallet
    cardano-repo-tool
    docs.build-and-serve-docs
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    updateMaterialized
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs ++ localInputs;
  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;
}
