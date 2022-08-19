# Snapbrillia Quadratic Voting Project

This branch introduces a Nix build system for the project. 'So what?' I hear you say...

This means: 
- No more faffing around with cloning plutus-apps and/or setting up links to its dist-newstyle directory etc.
- No need to explicitly clone the plutonomy repo and set it up
- Support for VSCode (i.e. code completion and all that stuff). (This does *NOT* use the devcontainer. I haven't got that working yet. Not sure it's a priority given we can use Vscode without it).
- *Most importantly* it provides a 'reproduceable' build. i.e. the build should be the 'same' on everyone's machine.
- It provides the basis for setting up pipeline builds on github

The build system implements a [Nix flake](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html).

> Flakes are the unit for packaging Nix code in a reproducible and discoverable way. They can
> have dependencies on other flakes, making it possible to have multi-repository Nix projects.


# Setting up

## Nix

The multi-user Nix installation is recommended. This can be performed as follows:
```
$ sh <(curl -L https://nixos.org/nix/install) --daemon
```
This will install the Nix cache under /nix. You will need approximately 100Gb free storage on the partition on which /nix is mounted.

After installation copy the following to /etc/nix/nix.conf

```
build-users-group   = nixbld
max-jobs            = auto # maybe only for personal machines
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
experimental-features = nix-command flakes
allow-import-from-derivation = true
```
Then restart the nix daemon:

```
sudo systemctl restart nix-daemon
```

## Clone this repo and/or checkout this branch.

You may prefer to play with the support this branch provides with out interfering (too much) with your current development environment. If so clone [quadraticvoting](https://github.com/Snapbrillia/quadraticvoting) into a new location (recommended). 
```bash
$ mkdir -p ~/tmp/repos
$ cd ~/tmp/repos
$ git clone https://github.com/Snapbrillia/quadraticvoting.git
$ cd quadraticvoting
$ git checkout feature/chained-stateful-scripts-nix
```
Or you could just checkout this branch in your current quadraticvoting directory.
```bash
$ git pull
$ git checkout feature/chained-stateful-scripts-nix
```

## Building

When this build system hits 'main' the repo will become a flake. It supports multiple ways of building the code and setting up a development environment. E.G.
1. __nix build .__

The default flake build:

```bash
$ cd ~/tmp/repos/quadraticvoting
$ nix build . -o qvf-cli-dynamic
```
This is the default 'flake' build. It builds 'qvf-cli' as a dynamic executable. It creates a symlink 'qvf-cli-dynamic' to the nix store directory that contains the executable. i.e.
```bash
$ ls -l
...
lrwxrwxrwx 1 andy andy    71 Aug 18 22:20 qvf-cli-dynamic -> /nix/store/w84h3k9iiq461bw35j2hfg1r9vvs1r9m-qvf-cli-exe-qvf-cli-0.1.0.0
...
```
We can see the executable is dynamic
```bash
$ ldd qvf-cli-dynamic/bin/qvf-cli 
	linux-vdso.so.1 (0x00007ffc96cee000)
	liblzma.so.5 => /nix/store/p6vws9zzv997asjmrnyc33v9xiw5pjyr-xz-5.2.5/lib/liblzma.so.5 (0x00007f049faeb000)
	libpthread.so.0 => /nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib/libpthread.so.0 (0x00007f049fae6000)
	libz.so.1 => /nix/store/hcdak2r7n3g850iw1hmhiasi0nzchdw8-zlib-1.2.12/lib/libz.so.1 (0x00007f049fac8000)
	libncursesw.so.6 => /nix/store/p9nmjgz8kzx87qjxka7g29j6qya752np-ncurses-6.3-p20220507/lib/libncursesw.so.6 (0x00007f049fa51000)
	libsodium.so.23 => /usr/local/lib/libsodium.so.23 (0x00007f049f9f1000)
	libgmp.so.10 => /nix/store/z2kzn2kj4wkz3rl1207r4rqyi4ar936j-gmp-with-cxx-6.2.1/lib/libgmp.so.10 (0x00007f049f950000)
	libc.so.6 => /nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib/libc.so.6 (0x00007f049f752000)
	libm.so.6 => /nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib/libm.so.6 (0x00007f049f679000)
	librt.so.1 => /nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib/librt.so.1 (0x00007f049f674000)
	libdl.so.2 => /nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib/libdl.so.2 (0x00007f049f66d000)
	libffi.so.8 => /nix/store/a6n90jvgz1sbr6982f6pzqs7y95x32b2-libffi-3.4.2/lib/libffi.so.8 (0x00007f049f660000)
	libnuma.so.1 => /nix/store/8pc3bcwhgz4aldkmpysqjblkcl4arq0w-numactl-2.0.14/lib/libnuma.so.1 (0x00007f049f651000)
	/nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib/ld-linux-x86-64.so.2 => /nix/store/k56d9sk88pvrqhvwpa6msdf8gpwnimf6-glibc-2.34-210/lib64/ld-linux-x86-64.so.2 (0x00007f049fb17000)
```
2. __nix-build__

```bash
$ cd ~/tmp/repos/quadraticvoting
$ nix-build
...
/nix/store/w84h3k9iiq461bw35j2hfg1r9vvs1r9m-qvf-cli-exe-qvf-cli-0.1.0.0
```
This is equivalent to the previous command, except it creates the default symlink 'result' to the store.

3. __cabal build all__

If you have already established the Nix environment for the project then 'qvf-cli' can be built using 'raw' cabal. This will be quickest.

```bash
$ cd ~/src/quadraticvoting
$ nix-shell
$ cabal build all
nix/store/ql8kpz74mxgcml09qpyaa607y5phwnvx-cabal-install-exe-cabal-3.6.2.0/bin/cabal --project-file=/home/andy/src/quadraticvoting/.nix-shell-cabal.project build all
Warning: No remote package servers have been specified. Usually you would have
one specified in the config file.
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - quadraticVoting-0.1.0.0 (lib) (configuration changed)
 - qvf-cli-0.1.0.0 (exe:qvf-cli) (configuration changed)
Configuring library for quadraticVoting-0.1.0.0..
Preprocessing library for quadraticVoting-0.1.0.0..
Building library for quadraticVoting-0.1.0.0..
[1 of 3] Compiling Token            ( src/Token.hs, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/quadraticVoting-0.1.0.0/build/Token.o, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/quadraticVoting-0.1.0.0/build/Token.dyn_o )
[2 of 3] Compiling Utils            ( src/Utils.hs, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/quadraticVoting-0.1.0.0/build/Utils.o, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/quadraticVoting-0.1.0.0/build/Utils.dyn_o )
[3 of 3] Compiling OnChain          ( src/OnChain.hs, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/quadraticVoting-0.1.0.0/build/OnChain.o, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/quadraticVoting-0.1.0.0/build/OnChain.dyn_o )
Configuring executable 'qvf-cli' for qvf-cli-0.1.0.0..
Preprocessing executable 'qvf-cli' for qvf-cli-0.1.0.0..
Building executable 'qvf-cli' for qvf-cli-0.1.0.0..
[1 of 1] Compiling Main             ( app/CLI.hs, /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/qvf-cli-0.1.0.0/x/qvf-cli/build/qvf-cli/qvf-cli-tmp/Main.o )
Linking /home/andy/src/quadraticvoting/dist-newstyle/build/x86_64-linux/ghc-8.10.7/qvf-cli-0.1.0.0/x/qvf-cli/build/qvf-cli/qvf-cli ...
```
4. __nix develop .__

The build system supports 'nix develop'. This can be used instead of 'nix-shell' in the example above. Currently it provides no additional functionality. But in future we may want to extend its functionality to provide support for testing.

5. __Building a static executable.__

We can produce a fully static executable as follows:
```bash
$ cd ~/src/quadraticvoting
$ nix build .#qvf-cli-static.x86_64-linux -o qvf-cli-static
$ ldd qvf-cli-static/bin/qvf-cli 
$	not a dynamic executable
```
# Direnv

> [direnv](https://direnv.net) is an extension for your shell. It augments existing shells with a new feature that 
> can load and unload environment variables depending on the current directory.

The project has a default .envrc file and therefore supports [direnv](https://direnv.net) for those who want to use it. If you have [direnv](https://direnv.net) installed then you should be prompted to *allow* [direnv](https://direnv.net) for the project's root directory when you checkout this branch . If that doesn't happen (for some reason) then after the checkout completes you can execute the following in the quadraticvoting's 'root' directory
```bash
$ direnv allow .
```
N.B. If you do this, every time you (or your shell) cd's into the root directory it will run 'nix-shell'. Initially this will be relatively expensive and it can still be a pain once things have settled down (i.e. been cached). So you may prefer not to enable [direnv](https://direnv.net) but to invoke 'nix-shell' explicitly.

# Editor Support

Editor and IDE support for Haskell is provided by the [haskell-language-server](https://haskell-language-server.readthedocs.io/en/latest/index.html) project. The build system ensures that the Nix environment contains an appropriately compiled version of 'haskell-language-server' and a version of 'haskell-language-server-wrapper'. This means VSCode, Emacs, Vim etc. can be made Haskell-aware and thus increase productivity.

## VSCode

The project provides default support for VSCode via 'vscode/settings.json'. This uses the Nix Environment Selector plugin. Follow instuctions below to make VSCode haskell-aware.

1. Install VSCode from [here](https://code.visualstudio.com/download)
2. Start VSCode and add the following extensions:
- Haskell (id: haskell.haskell)
- Haskell Syntax Highlighting (id: justusadam.language-haskell)
- Nix Environment Selector (id: arrterian.nix-env-selector)
- direnv (id: mkhl.direnv) -- optional (see 2. below)
3. Exit VSCode.

When VSCode opens the project folder it will use the 'Nix Environment Selector' to set up the nix environment automatically. It may take a few seconds to settle down (it's running 'nix-shell'). When you open a haskell file you should see activity in the status bar (e.g. 'processing 2/5'). 

## Vim

Configuration instructions for vim are [here](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#vim-or-neovim). The following worked for me :-) :

1. __Install nodejs__
```bash
$ curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
$ sudo apt-get install -y nodejs
```
2. __Install vim-plug__
```bash
curl -fLo ~/.vim/autoload/plug.vim --create-dirs     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```
3. __Add coc.nvim plugin to .vimrc__

> call plug#begin()
> 
> " Use release branch (recommend)
> Plug 'neoclide/coc.nvim', {'branch': 'release'}
>
> call plug#end()

4. __Install coc.nvim plugin__

i.   invoke vim
ii.  type ':PlugInstall'
iii. type ':q'

5. __Add support for Haskell__

Add the following to ~/.vim/coc-settings.json

> {
>   "languageserver": {
>     "haskell": {
>       "command": "haskell-language-server-wrapper",
>       "args": ["--lsp"],
>       "rootPatterns": ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"],
>       "filetypes": ["haskell", "lhaskell"]
>     }
>   }
> }

Now when you edit a haskell file using vim inside the project's nix environment (set up via 'nix-shell', 'nix develop .' or direnv) you should see vim interacting with the 'haskell-language-server'. You will see a notification indicating that the 'haskell-language-server' is processing the project's Haskell files. 

However, I didn't see any context sensitive info. OTOH I am not a vim user, so it's possible there's something else required. Please update the README if you discover what it is :-)



