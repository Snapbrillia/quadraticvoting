# == CHANGE THESE VARIABLES ACCORDINGLY ==

# Running node's network magic:
export MAGIC='--testnet-magic 1'

# Path to the socket file used by the running node:
export CARDANO_NODE_SOCKET_PATH="$HOME/preprod-testnet/node.socket"

# Absolute path to the quadraticvoting home directory:
export REPO="$HOME/code/quadraticvoting"

# The `cardano-cli` binary:
export cli="cardano-cli"

# The `qvf-cli` binary:
export qvf="qvf-cli"

# Absolute path to the directory for storing wallets/artifacts:
export preDir="$REPO/testnet2"
mkdir -p $preDir

# Change this to "dev" for testing/development:
export ENV="prod"