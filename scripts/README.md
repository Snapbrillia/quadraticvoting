# A Set of Helper Bash Scripts for Easy On-Chain Testing

To avoid using the resource intensive Plutus Application Backend (PAB), we have
developed a set of bash scripts to perform various test scenarios with a smart
contract that is deployed on the testnet.

One of the primary functionalities implemented here, is the ability of using a
single wallet that contains a large sum of Lovelaces, and distributing a total
amount among a number of freshly generated wallets.

This allows us to easily have access to numerous wallets for better testing.
And, to prevent loss of tADA tokens, we also have a function to drain all the
wallets into the initial wallet.

These utility functions, along with all environment assignmentsare defined
in `env.sh`.
