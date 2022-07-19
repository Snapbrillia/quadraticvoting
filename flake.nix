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
    # TODO - remove or replace with cardano-node to get cardano-cli
    # plutus-apps = {
    #   url = "github:input-output-hk/plutus-apps";
    #   flake = false;
    # };

    # Custom user config (default: empty), eg.:
    # { outputs = {...}: {
    #   # Cutomize listeming port of node scripts:
    #   nixosModules.cardano-node = {
    #     services.cardano-node.port = 3002;
    #   };
    # };
    customConfig.url = "github:input-output-hk/empty-flake";

    # TODO - review -do we want the equivalent?
    # qvf-measured = {
    #   url = "github:input-output-hk/cardano-node";
    # };
    # TODO - not yet!
    # qvf-snapshot = {
    #   url = "github:input-output-hk/cardano-node/7f00e3ea5a61609e19eeeee4af35241571efdf5c";
    # };
    qvf-process = {
      url = "github:snapbrillia/quadraticvoting";
      flake = false;
    };
    
    # TODO !@! - remove?
    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror/nix";
  };

  outputs =
    { self
    , nixpkgs
    , hostNixpkgs
    , utils
    , haskellNix
    , iohkNix
    # TODO maybe cardano node (to get cardano-cli and maybe other stuff)
    # , plutus-apps 
    , cardano-mainnet-mirror
    # , qvf-snapshot
    # , qvf-measured
    , qvf-process
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
        # TODO - we need something here?
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          inherit customConfig;
          gitrev = final.customConfig.gitrev or self.rev or "0000000000000000000000000000000000000000";
          commonLib = lib
            // iohkNix.lib
            // final.cardanoLib
            // import ./nix/svclib.nix { inherit (final) pkgs; };
        })
        (import ./nix/pkgs.nix)
        self.overlay
      ];

      projectPackagesExes = import ./nix/project-packages-exes.nix;

      mkPackages = project:
        let
          inherit (project.pkgs.stdenv) hostPlatform;
          inherit (project.pkgs.haskell-nix) haskellLib;
          # TODO - reinstate later but for qvf-cli?
          # profiledProject = project.appendModule {
          #   modules = [{
          #     enableLibraryProfiling = true;
          #     packages.cardano-node.components.exes.cardano-node.enableProfiling = true;
          #     packages.tx-generator.components.exes.tx-generator.enableProfiling = true;
          #     packages.locli.components.exes.locli.enableProfiling = true;
          #   }];
          # };
          # assertedProject = project.appendModule {
          #   modules = [{
          #     packages = lib.genAttrs [
          #       "ouroboros-consensus"
          #       "ouroboros-consensus-cardano"
          #       "ouroboros-consensus-byron"
          #       "ouroboros-consensus-shelley"
          #       "ouroboros-network"
          #       "network-mux"
          #     ]
          #       (name: { flags.asserts = true; });
          #   }];
          # };
          # eventloggedProject = project.appendModule
          #   {
          #     modules = [{
          #       packages = lib.genAttrs [ "cardano-node" ]
          #         (name: { configureFlags = [ "--ghc-option=-eventlog" ]; });
          #     }];
          #   };
          # TODO - remove or replace with cardano-no t get cardano-cli
          # inherit ((import plutus-apps {
          #   inherit (project.pkgs) system;
          # }).plutus-apps.haskell.packages.plutus-example.components.exes) plutus-example;

          # TODO - remove
          # pinned-workbench =
          #   cardano-node-workbench.workbench.${project.pkgs.system};
          hsPkgsWithPassthru = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
            (path: value:
              # TODO - just return value until/unless we reinstate profiledProject etc
              # if (lib.isAttrs value) then
              #   lib.recursiveUpdate value
              #     {
              #       passthru = {
              #         profiled = lib.getAttrFromPath path profiledProject.hsPkgs;
              #         asserted = lib.getAttrFromPath path assertedProject.hsPkgs;
              #         eventlogged = lib.getAttrFromPath path eventloggedProject.hsPkgs;
              #       };
              #     } else value)
              value)
            project.hsPkgs;
          projectPackages = lib.mapAttrs (n: _: hsPkgsWithPassthru.${n}) projectPackagesExes;
        in
        {
          # TODO - remove
          # inherit projectPackages profiledProject assertedProject eventloggedProject;
          inherit projectPackages;
          # TODO - remove
          # inherit pinned-workbench;
          # TODO -remove
          # projectExes = flatten (haskellLib.collectComponents' "exes" projectPackages) // (with hsPkgsWithPassthru; {
          #   inherit (ouroboros-consensus-byron.components.exes) db-converter;
          #   inherit (ouroboros-consensus-cardano.components.exes) db-analyser;
          #   inherit (bech32.components.exes) bech32;
          # } // lib.optionalAttrs hostPlatform.isUnix {
          #   inherit (network-mux.components.exes) cardano-ping;
          #   inherit plutus-example;
          # });

          projectExes = flatten (haskellLib.collectComponents' "exes" projectPackages);
        };

      mkQuadraticvotingPackages = project: (mkPackages project).projectExes // {
        inherit (project.pkgs) quadraticvoting;
      };

      flake = eachSystem supportedSystems (system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          inherit (pkgs.haskell-nix) haskellLib;
          inherit (haskellLib) collectChecks' collectComponents';
          # TODO !@!- not doing this yry
          # inherit (pkgs.commonLib) eachEnv environments mkSupervisordCluster;
          inherit (pkgs.commonLib) eachEnv environments;
          inherit (project.pkgs.stdenv) hostPlatform;

          project = (import ./nix/haskell.nix {
            inherit (pkgs) haskell-nix gitrev;
            inherit projectPackagesExes;
          }).appendModule customConfig.haskellNix;
          # TODO - reinstate when/if these are reinstated
          # // {
          #   profiled = profiledProject;
          #   asserted = assertedProject;
          #   eventlogged = eventloggedProject;
          # };

          # TODO - remove
          # inherit (mkPackages project) projectPackages projectExes profiledProject assertedProject eventloggedProject pinned-workbench;
          inherit (mkPackages project) projectPackages projectExes;

          # TODO !@! - need to do something here
          shell = import ./shell.nix { inherit pkgs customConfig cardano-mainnet-mirror; };
          devShells = {
            inherit (shell) devops workbench-shell;
            cluster = shell;
            profiled = project.profiled.shell;
          };

          devShell = shell.dev;

          # NixOS tests run a node and submit-api and validate it listens
          # TODO !@! - what do we wnat to do here
          # nixosTests = import ./nix/nixos/tests {
          #   inherit pkgs;
          # };

          # checks = flattenTree (collectChecks' projectPackages) //
          #   # Linux only checks:
          #   (optionalAttrs hostPlatform.isLinux (
          #     prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) nixosTests)
          #   ))
          #   # checks run on default system only;
          #   // (optionalAttrs (system == defaultSystem) {
          #   hlint = pkgs.callPackage pkgs.hlintCheck {
          #     inherit (project.args) src;
          #   };
          # });

          exes = projectExes;
            # TODO !@! - not doing docker (yet)
          #  // {
          #   inherit (pkgs) cabalProjectRegenerate checkCabalProject;
          #   "dockerImages/push" = import ./.buildkite/docker-build-push.nix {
          #     hostPkgs = import hostNixpkgs { inherit system; };
          #     inherit (pkgs) dockerImage submitApiDockerImage;
          #   };
          #   "dockerImage/node/load" = pkgs.writeShellScript "load-docker-image" ''
          #     docker load -i ${pkgs.dockerImage} $@
          #   '';
          #   "dockerImage/submit-api/load" = pkgs.writeShellScript "load-submit-docker-image" ''
          #     docker load -i ${pkgs.submitApiDockerImage} $@
          #   '';
          # } // flattenTree (pkgs.scripts // {
          #   # `tests` are the test suites which have been built.
          #   tests = collectComponents' "tests" projectPackages;
          #   # `benchmarks` (only built, not run).
          #   benchmarks = collectComponents' "benchmarks" projectPackages;
          # });

          # TODO !@! - not doing this - yet
          # inherit (pkgs) workbench all-profiles-json supervisord-workbench-nix supervisord-workbench-for-profile;

          packages = exes;
            # TODO !@! - not doing this yey
            # let
            #   supervisord-workbench =
            #     pkgs.callPackage supervisord-workbench-nix { workbench = pinned-workbench; };
            # in
            # exes
            # # Linux only packages:
            # // optionalAttrs (system == "x86_64-linux") rec {
            #   "dockerImage/node" = pkgs.dockerImage;
            #   "dockerImage/submit-api" = pkgs.submitApiDockerImage;
            #   ## TODO: drop external membench, once we bump 'node-snapshot'
            #   # snapshot = membench.outputs.packages.x86_64-linux.snapshot;
            #   # membenches = pkgs.membench-node-this-5.batch-report;

            #   ## This is a very light profile, no caching&pinning needed.
            #   workbench-ci-test =
            #     (pkgs.supervisord-workbench-for-profile
            #       {
            #         # inherit supervisord-workbench; ## Not required, as long as it's fast.
            #         profileName = "ci-test-bage";
            #         cardano-node-rev =
            #           if __hasAttr "rev" self
            #           then self.rev
            #           else throw "Cannot get git revision of 'cardano-node', unclean checkout?";
            #       }).profile-run {};

            #   all-profiles-json = pkgs.all-profiles-json;
            # }
            # # Add checks to be able to build them individually
            # // (prefixNamesWith "checks/" checks);

          apps = lib.mapAttrs (n: p: { type = "app"; program = p.exePath or (if (p.executable or false) then "${p}" else "${p}/bin/${p.name or n}"); }) exes;

        in
        {

          # TODO !@! - not doing checks yet
          # inherit environments packages checks apps project;
          inherit environments packages apps project;

          legacyPackages = pkgs;

          # Built by `nix build .`
          defaultPackage = packages.qvf-cli;

          # Run by `nix run .`
          defaultApp = apps.qvf-cli;

          # This is used by `nix develop .` to open a devShell
          inherit devShell devShells;

          # The parametrisable workbench.
          # TODO !@! - not doing this yet
          # inherit workbench;

          systemHydraJobs = optionalAttrs (system == "x86_64-linux")
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
                  # TODO !@! not doing this yet
                  # profiled = lib.genAttrs [ "cardano-node" "tx-generator" "locli" ] (n:
                  #   packages.${n}.passthru.profiled
                  # );
                  # asserted = lib.genAttrs [ "cardano-node" ] (n:
                  #   packages.${n}.passthru.asserted
                  # );
                };
                musl =
                  let
                    muslProject = project.projectCross.musl64;
                    inherit (mkPackages muslProject) projectPackages projectExes;
                  in
                  projectExes // {
                    cardano-node-linux = import ./nix/binary-release.nix {
                      inherit pkgs;
                      inherit (exes.cardano-node.identifier) version;
                      platform = "linux";
                      exes = lib.collect lib.isDerivation projectExes;
                    };
                    internal.roots.project = muslProject.roots;
                  };
                # TODO - remove - we're not building for windows
                # windows =
                #   let
                #     windowsProject = project.projectCross.mingwW64;
                #     inherit (mkPackages windowsProject) projectPackages projectExes;
                #   in
                #   projectExes
                #     // (removeRecurse {
                #     checks = collectChecks' projectPackages;
                #     tests = collectComponents' "tests" projectPackages;
                #     benchmarks = collectComponents' "benchmarks" projectPackages;
                #     cardano-node-win64 = import ./nix/binary-release.nix {
                #       inherit pkgs;
                #       inherit (exes.cardano-node.identifier) version;
                #       platform = "win64";
                #       exes = lib.collect lib.isDerivation projectExes;
                #     };
                #     internal.roots.project = windowsProject.roots;
                #   });
              };
            }; 
            # TODO - remove - we're not building for Macs
            # // optionalAttrs (system == "x86_64-darwin") {
            # macos = lib.filterAttrs
            #   (n: _:
            #     # only build docker images once on linux:
            #     !(lib.hasPrefix "dockerImage" n))
            #   packages // {
            #   cardano-node-macos = import ./nix/binary-release.nix {
            #     inherit pkgs;
            #     inherit (exes.cardano-node.identifier) version;
            #     platform = "macos";
            #     exes = lib.collect lib.isDerivation projectExes;
            #   };
            #   shells = removeAttrs devShells [ "profiled" ] // {
            #     default = devShell;
            #   };
            #   internal = {
            #     roots.project = project.roots;
            #     plan-nix.project = project.plan-nix;
            #   };
            # };
        }
      );

      makeRequired = isPr: extra:
      let
        jobs = lib.foldl' lib.mergeAttrs { } (lib.attrValues flake.systemHydraJobs);
        # TODO !@! - not doing macos or membenches yet 
        # nonRequiredPaths = map lib.hasPrefix ([ "macos." ] ++ lib.optional isPr "linux.native.membenches");
      in with self.legacyPackages.${defaultSystem};
        releaseTools.aggregate {
          name = "github-required";
          meta.description = "All jobs required to pass CI";
          constituents = lib.collect lib.isDerivation
            (lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
              (path: value:
                # TODO !@! - no nonRequiredPaths yet
                # let stringPath = lib.concatStringsSep "." path; in if lib.isAttrs value && (lib.any (p: p stringPath) nonRequiredPaths) then { } else value)
                value )
              jobs) ++ extra;
        };


      hydraJobs =
        let
          jobs = lib.foldl' lib.mergeAttrs { } (lib.attrValues flake.systemHydraJobs);
        in
        jobs // (with self.legacyPackages.${defaultSystem}; rec {
          quadraticvoting-deployment = quadraticvoting.mkConfigHtml { inherit (quadraticvoting.environments) mainnet testnet; };
          build-version = writeText "version.json" (builtins.toJSON {
            inherit (self) lastModified lastModifiedDate narHash outPath shortRev rev;
          });
          required = makeRequired false [ quadraticvoting-deployment build-version ];
        });

      # TODO !@! - not doing this yet
      # hydraJobsPr =
      #   let
      #     nonPrJobs = map lib.hasPrefix [
      #       "linux.native.membenches"
      #     ];
      #   in
      #   (lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
      #     (path: value:
      #       let stringPath = lib.concatStringsSep "." path; in if lib.isAttrs value && (lib.any (p: p stringPath) nonPrJobs) then { } else value)
      #     hydraJobs) // {
      #       required = makeRequired true [ hydraJobs.cardano-deployment hydraJobs.build-version ];
      #     };

    in
    builtins.removeAttrs flake [ "systemHydraJobs" ] // {

      # TODO !@! - not doing hydraJobsPr yet
      # inherit hydraJobs hydraJobsPr;
      inherit hydraJobs;

      overlay = final: prev: {
        quadraticvotingProject = flake.project.${final.system};
        quadraticvotingPackages = mkQuadraticvotingPackages final.quadraticvotingProject;
        inherit (final.quadraticvotingPackages) qvf-cli;

        # TODO, fix this
        #db-analyser = ouroboros-network-snapshot.haskellPackages.ouroboros-consensus-cardano.components.exes.db-analyser;
      };
      # TODO !@! - not doing this yet
      # nixosModules = {
      #   cardano-node = { pkgs, lib, ... }: {
      #     imports = [ ./nix/nixos/cardano-node-service.nix ];
      #     services.cardano-node.cardanoNodePackages = lib.mkDefault (mkQuadraticvotingPackages flake.project.${pkgs.system});
      #   };
      #   cardano-submit-api = { pkgs, lib, ... }: {
      #     imports = [ ./nix/nixos/cardano-submit-api-service.nix ];
      #     services.cardano-submit-api.cardanoNodePackages = lib.mkDefault (mkQuadraticvotingPackages flake.project.${pkgs.system});
      #   };
      # };
    };
}
