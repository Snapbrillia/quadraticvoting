# Bash Solutions to Bridge `qvf-cli` and `cardano-cli` for Transaction Construction

Our custom application, `qvf-cli`, is developed to provide off-chain
accessibility of the on-chain logics. To utilize this functionality, we have
developed this set of bash scripts and functions for a more convenient
developer experience.

These utility functions, along with all environment assignments are defined
in `env.sh`.

## Quickstart

1. Clone the repository, and navigate into its home directory:
```bash
$ git clone https://github.com/Snapbrillia/quadraticvoting
$ cd quadraticvoting
```

2. Enter the Nix shell:
```bash
$ nix-shell
# Make sure you are still in the repository's directory:
$ cd /path/to/quadraticvoting
```

3. Build the `qvf-cli` binary and optionally put it in your `PATH`. An easy
way to do that is by using Cabal's `install` command (the flag tells Cabal that
in case the binary is already installed, it's allowed to overwrite it):
```bash
$ cabal install qvf-cli --overwrite-policy=always
```

4. Modify the `scripts/local-env.sh` file to match your environment and then
source it to expose its variables:
```bash
$ cd /path/to/quadraticvoting
$ . scripts/local-env.sh
```

5. Source `scripts/env.sh`, and the logs should guide you through any further
requirements:
```bash
$ . scripts/env.sh
```
