# A Tokenized Approach to Limit the Growth of Datum Values

The idea is to have a "governing" script (`G`), a generic minting script (`M`),
and project-specific scripts (`X(i)`) for accumulation of vote tokens. Let's
consider the flow:

- The key holder will use an NFT minting script to mint an authentication
  token.

- At this point, the key holder has an NFT with a known currency symbol (`S`). 
  By parametrizing `M` with this authentication asset, it'll be "aware" of `S`.
  Subsequently, parametrizing `G` by both `S` and `M`, will make it capable of
  validating UTxO authenticity, and the tokens minted by `M`.

The UTxO at `G` should be redeemable in four different ways:
  1. Registering a new project,
  2. Contributing to the prize pool,
  3. Burning the vote tokens accumulated by `X(i)`,
  4. And distributing the funds.

We start with the project registration.

## 1. Registering a Project

- With the information of the project (name, wallet, and requested fund), 
  `X(0)` is generated, and its validator hash is used to construct this 
  consuming & minting transaction.

- `M` will mint 2 identical tokens in this transaction: one for `G`, and one
  for `X(0)`. The token name of these tokens will be the validator hash of the
  project's script. And because of `G`'s awareness of `M`, it can validate the
  consumption.
  
  However, there might be a risk here. Since `X(0)`'s validator hash is coming
  from off-chain logic, one can argue that any script can be injected here
  (there is no way for `G` to know `X(0)` beforehand). But since the minting
  for each donation is going to be performed by `M`, this might not be an
  issue. _This part of the architecture should be scrutinized further_.

  `M` is also aware of `S`, and therefore can check for its presence in both
  sides of the transaction.
