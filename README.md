# Snapbrillia Quadratic Voting Project

This branch introduces a Nix build system for the project. 'So what?' I hear you say...

This means: 
- No more faffing around with cloning plutus-apps and/or setting up links to its dist-newstyle directory etc.
- No need to explicitly clone the plutonomy repo and set it up
- Support for Vscode (i.e. code completion and all that stuff). (This does *NOT* use the devcontainer. I haven't got that working yet. Not sure it's a priority given we can use Vscode without it).
- *Most importantly* it provides a 'reproduceable' build. i.e. the build should be the 'same' on everyone's machine.
- It provides the basis for setting up pipeline builds on github

## Setting up

# Vscode
   If you want to use vscode 
   a. Install it from xxx
   b. Start vscode and add the following extensions:
      i.   Haskell (id: haskell.haskell)
      ii.  Haskell Syntax Highlighting (id: justusadam.language-haskell)
      iii. Nix Environment Selector (id: arrterian.nix-env-selector)
      iv.  direnv (id: mkhl.direnv) -- optional (see 2. below)
   c. Exit vscode.

# Direnv
   '[direnv](https://direnv.net) is an extension for your shell. It augments existing shells with a new feature that can load and unload environment variables depending on the current directory.'
   The project supports direnv for those who want to use it. If you have direnv installed when you checkout this branch you should be prompted to allow direnv for the project's root directory. If that doesn't happen you can execute the following in the projects root directory
   ```bash
   $ direnv allow .
   ```
   N.B. If you do this, every time you (or your shell) cd's into the root directory it will run 'shell.nix'. This will cause the same activity as if you had typed 'nix-shell' in the root directory. Initially this will be relatively expensive and it can still be a pain once things have settled down (i.e. been cached). So you may prefer to invoke 'nix-shell' explicitly.

# Clone this repo and/or checkout this branch.
   You may prefer to play with the support this branch provides with out interfering (too much) with your current development environment. If so clone [quadraticvoting](https://github.com/Snapbrillia/quadraticvoting) into a new location (recommended). 
   ```bash
   $ mkdir -p ~/tmp/repos
   $ cd ~/tmp/repos
   $ git clone https://github.com/Snapbrillia/quadraticvoting.git
   ```
   Or you could just checkout this branch in your existing directory.
   ```bash
   $ git checkout feature/vscode-nix-plutonomy
   ```
   If you've installed [direnv](https://direnv.net) you should get prompted at this point (see 2 above). If you decide to allow 'direnv' on prompting or explicitly, go and get a coffee at this point. The project's 'shell.nix' will get invoked to setup your nix (build) environment.

# Build
   If you're not using [direnv](https://direnv.net) invoke 'nix-shell' in the project's root directory.
   ```bash
   $ cd ~/tmp/repos
   $ nix-shell
   ```
   On first invocation this will take some time as it populates your nix cache. On my machine, in Vietnam, it took 10 minutes. And then
   ```bash
   $ cabal build
   ```
   And that's it. You should be able to use the repl at this point. i.e. The following should work:
   ```bash
   $ cabal repl
   ```

# Vscode - again
   Start vscode from your projects root dir. Wait for things to settle down :-). Then use the 'Nix Environment Selector'
   extension to make the correct versions of the tools are used (in particular the haskell-language-server) and that they run in the right environment. Follow the instructions in 'Nix Environment Selector' documentation. You will be presented with a list of nix files to select from. Choose 'life'. Err, actually choose 'shell.nix'. Then exit and restart. Give it a few seconds to warm up, then open e.g. OnChain.hs. You should see activity in the status bar - hls getting to work. 

And you're all set....

