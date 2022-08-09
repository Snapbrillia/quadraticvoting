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
    customConfig.url = "github:input-output-hk/empty-flake";
  };

  outputs =
    { self
    , nixpkgs
    , hostNixpkgs
    , utils
    , haskellNix
    , iohkNix
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

      mkQuadraticVotingPackages = project: (mkPackages project).projectExes // {
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
          # TODO !@! remove?
          inherit (project.pkgs.stdenv) hostPlatform;

          project = (import ./nix/haskell.nix {
            inherit (pkgs) haskell-nix gitrev;
            inherit projectPackagesExes;
          }).appendModule customConfig.haskellNix;

          # project = stdProject.projectCross.musl64;

          inherit (mkPackages project) projectPackages projectExes;

          # TODO !@! - need to do something here
          #shell = import ./shell.nix { inherit pkgs customConfig; };
          #devShells = {
          #  inherit (shell) devops;
          #  cluster = shell;
          #  profiled = project.profiled.shell;
          #};

          #devShell = shell.dev;

          exes = projectExes;
          packages = projectPackages;
          apps = lib.mapAttrs (n: p: { type = "app"; program = p.exePath or (if (p.executable or false) then "${p}" else "${p}/bin/${p.name or n}"); }) exes;

        in
        {
          inherit packages apps project;

          legacyPackages = pkgs;

          # This is used by `nix develop .` to open a devShell
          #inherit devShell devShells;

          jobs = optionalAttrs (system == "x86_64-linux")
            {
              linux = {
                native = packages // {
                  # shells = devShells // {
                  #   default = devShell;
                  # };
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
                  projectExes // {
                    qvf-cli-linux = import ./nix/binary-release.nix {
                      inherit pkgs;
                      inherit (exes.qvf-cli.identifier) version;
                      platform = "linux";
                      exes = lib.collect lib.isDerivation projectExes;
                    };
                    internal.roots.project = muslProject.roots;
                  };
              };
            };
                    # Built by `nix build .`
          defaultPackage = jobs.x86_64-linux.linux.musl.qvf-cli;

          # Run by `nix run .`
          defaultApp = apps.qvf-cli;


        }
      );
      jobs = flake.jobs;
    in
    flake // {

      inherit jobs;

      overlay = final: prev: {
        quadraticVotingProject = flake.project.${final.system};
        quadraticVotingPackages = mkQuadraticVotingPackages final.quadraticVotingProject;
        inherit (final.quadraticVotingPackages) qvf-cli;
      };
    };
}
