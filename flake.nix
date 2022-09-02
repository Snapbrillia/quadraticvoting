{
  description = "Snapbrillia Quadratic Voting";

  inputs = {
    # IMPORTANT: report any change to nixpkgs channel in nix/default.nix:
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hostNixpkgs.follows = "nixpkgs";
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    nixTools = {
        url = "github:input-output-hk/nix-tools";
        flake = false;
      };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackageNix";
    };
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    plutus-apps = {
      url = "github:input-output-hk/plutus-apps";
      flake = false;
    };
    customConfig.url = "github:input-output-hk/empty-flake";

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror/nix";

  };

  outputs =
    { self
    , nixpkgs
    , hostNixpkgs
    , utils
    , haskellNix
    , iohkNix
    , plutus-apps
    , cardano-mainnet-mirror
    , ...
    }@input:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith;
      removeRecurse = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations");
      flatten = attrs: lib.foldl' (acc: a: if (lib.isAttrs a) then acc // (removeAttrs a [ "recurseForDerivations" ]) else acc) { } (lib.attrValues attrs);

      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = head supportedSystems;
      customConfig = recursiveUpdate
        (import ./nix/custom-config.nix customConfig)
        input.customConfig;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          inherit customConfig;
          gitrev = final.customConfig.gitrev or self.rev or "0000000000000000000000000000000000000000";
          # TODO !@! remove?
          commonLib = lib
            // final.cardanoLib
            // iohkNix.lib;
        })
        (import ./nix/pkgs.nix)
        self.overlay
      ];

      projectPackagesExes = import ./nix/project-packages-exes.nix;

      mkPackages = project:
        let
          inherit (project.pkgs.stdenv) hostPlatform;
          inherit (project.pkgs.haskell-nix) haskellLib;
          projectPackages = project.hsPkgs;
        in
        {
          inherit projectPackages;
          projectExes = flatten (haskellLib.collectComponents' "exes" projectPackages);
        };

      mkQvfPackages = project: (mkPackages project).projectExes // {
        inherit (project.pkgs) cardanoLib;
      };

      flake = eachSystem supportedSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          inherit (pkgs.haskell-nix) haskellLib;
          inherit (haskellLib) collectComponents';
          inherit (project.pkgs.stdenv) hostPlatform;

          project = (import ./nix/haskell.nix {
            inherit (pkgs) haskell-nix gitrev;
            inherit projectPackagesExes;
          }).appendModule customConfig.haskellNix;

          inherit (mkPackages project) projectPackages projectExes;

          shell = import ./shell.nix { inherit pkgs customConfig cardano-mainnet-mirror; };
          devShells = {
            devops = shell;
            workbench-shell = shell;
            cluster = shell;
            profiled = shell;
          };
          devShell = shell.dev;

          exes = projectExes;
          packages = projectPackages;
          apps = lib.mapAttrs 
                  (n: p: 
                    { type = "app"; 
                      program = p.exePath or 
                                (if (p.executable or false) 
                                then "${p}" 
                                else "${p}/bin/${p.name or n}"); }) 
                  exes;

        in
        {
          inherit packages apps project;

          legacyPackages = pkgs;

          # Built by `nix build .`
          defaultPackage = packages.qvf-genesis.components.exes.qvf-genesis;

          # Run by `nix run .`
          defaultApp = apps.qvf-genesis;

          # Built by `nix build .#qvf-genesis-static.x86_64-linux`
          # Bundle  to a docker image:
          # nix bundle --bundler github:NixOS/bundlers#toDockerImage .#qvf-genesis-static.x86_64-linux
          # docker load < qvf-genesis-exe-qvf-genesis-x86_64-unknown-linux-musl-0.1.0.0.tar.gz
          # docker run qvf-genesis-x86_64-unknown-linux-musl-0.1.0.0:latest xxx
          qvf-genesis-static = jobs.x86_64-linux.linux.musl.qvf-genesis;

          # Built by `nix build .#qvf-genesis.x86_64-linux`
          # Bundle to a docker image:
          # nix bundle --bundler github:NixOS/bundlers#toDockerImage .#qvf-genesis.x86_64-linux
          # docker load < qvf-genesis-exe-qvf-genesis-0.1.0.0.tar.gz
          # docker run qvf-genesis-0.1.0.0:latest xxx          
          qvf-genesis = packages.qvf-genesis.components.exes.qvf-genesis;

          # This is used by `nix develop .` to open a devShell
          inherit devShell devShells;

          jobs = optionalAttrs (system == "x86_64-linux")
            {
              linux = {
                native = packages // {
                  shells = devShells // {
                    default = devShell;
                  };
                  internal = {
                    roots.project = project.roots;
                    plan-nix.project = project.plan-nix;
                  };
                };
                musl =
                  let
                    muslProject = project.projectCross.musl64;
                    inherit (mkPackages muslProject) projectPackages projectExes;
                  in
                  projectExes // { internal.roots.project = muslProject.roots; };
              };
            };
        }
      );
      jobs = flake.jobs;
      qvf-genesis-static = flake.qvf-genesis-static;
      qvf-genesis = flake.qvf-genesis;
    in
    flake // {

      inherit jobs qvf-genesis-static qvf-genesis ;

      overlay = final: prev: {
        quadraticvoting-project = flake.project.${final.system};
        quadraticvoting-packages = mkQvfPackages final.quadraticvoting-project;
        inherit (final.quadraticvoting-packages) qvf-cli quadraticVoting;
      };
    };
}
