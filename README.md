# Snapbrillia Quadratic Voting Project

## Setting up

1. Clone the [plutus-apps repo](https://github.com/input-output-hk/plutus-apps)
   in any location:
   ```bash
   $ cd ~
   $ git clone https://github.com/input-output-hk/plutus-apps
   ```
2. Enter the `nix-shell`:
   ```bash
   $ cd ~/plutus-apps
   $ nix-shell
   ```
3. Build all the dependencies here:
   ```bash
   $ cabal build all
   ```
   *NOTE:* This command generates the `dist-newstyle` folder which can take
   quite some time. If you already have run `cabal repl` or a similar command
   inside another Plutus project, you can move the `dist-newstyle` from there
   to here in order to reduce the build time.

4. Clone this repository:
   ```bash
   $ cd ~
   $ git clone https://github.com/snapbrillia/quadraticvoting
   ```

5. Create a symlink to the `dist-newstyle` from your `plutus-apps` directory
   here:
   ```bash
   $ cd ~/quadraticvoting
   $ rm -rf dist-newstyle
   $ ln -s ~/plutus-apps/dist-newstyle dist-newstyle
   ```

Now you are ready to either enter a `repl`, or use the generator application.

## Using the Datum/Redeemer Generator Application

After setting up your environment, you should be able to run:
```bash
$ cabal run qvf-cli
```
To have the help text of the application printed. Follow the guides to generate
your desired files.
