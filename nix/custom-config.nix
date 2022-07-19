self: {
  # TODO - do we want soem thing similar
  # withHoogle = true;
  # localCluster = {
  #   cacheDir    = "${self.localCluster.stateDir}/.cache";
  #   stateDir    = "run/current";
  #   batchName   = "plain";
  #   profileName = "default-bage";
  #   basePort    = 30000;
  #   enableEKG        = true;
  #   workbenchDevMode = true;
  #   extraSupervisorConfig = {};
  # };
  # optional extra haskell.nix module
  haskellNix = {};
}
