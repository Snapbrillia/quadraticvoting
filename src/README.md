# A Tokenized Approach to Limit the Growth of Datum Values

The idea is to have a "governing" script (`G`), a project registration minting
script (`P`), and a vote token minting script (`V`).


## Initiation

Let's consider the flow:

- The key holder will use an NFT minting script to mint an authentication
  token (`S`).

  This NFT minting script can be done two different ways:

    - Using a known script, and storing the spent UTxO in the token name: This
      approach has the benefit of more trust by the users. The
      "recognizability" of the policy ID means that users can rest assured that
      this token is truly an NFT. The downside of this approach is that it
      leads to larger scripts. With Vasil, this may not be a credible downside.

    - Using a custom script, and specifying a minimal token name for the NFT:
      This leads to smaller script sizes, but may lead to less trust.

- At this point, the key holder has an NFT with a known currency symbol (`S`). 
  By parametrizing `P` with this authentication asset, project registration
  minter will be "aware" of `S`, and therefore capable of validating its
  presence in a registration transaction.

  On the other hand, parametrizing `V` by `P` will make the donation minter
  capable of validating authenticity of a project UTxO, and may lead to
  alleviating the datum from carrying all the information.

  Subsequently, parametrizing `G` by both `S` and `P`, will make it capable of
  validating UTxO authenticity, and check that a transaction that's attempting
  to spend its singular UTxO for the purpose of registering a new project is
  in fact minting an asset with a symbol of `P`.

In short, key holder will mint `S`. `P` will be parametrized by `S`. `V` will
be parametrized by `P`. And finally, `G` will be parametrized by both `S` and 
`P` (should it also be parametrized by `V`?).


## Project Registration

`P` will be responsible of generating UTxOs at `G` that each carry an asset
minted by it. The token names of these assets are going to be the public key
hashes of the wallets that each particular project will have its prize sent to
(i.e. the project owner's public key hash).  Each UTxO is also going to carry a
datum that stores additional information about its corresponding project:
label, requested funds, etc. The datum is also going to keep track of the total
amount of Lovelaces received by that project. This will be the only field
that'll be subject to updates.

A project registration transaction therefore, will consist of these inputs:

  - As many UTxOs necessary to cover the registration fee by the project owner,

  - The singular UTxO from the governing script carrying its authenticity asset
  (`S`). Note that `P` can not be aware of `G` and can only check the
  _presence_ of `S`. This UTxO should also have a datum attached that tracks
  the number of projects registered so far (does that suffice?).

And these outputs:

  - The "governing" UTxO sent back to `G` with an updated datum (i.e.
  incremented by 1),

  - A UTxO sent to the same address, carrying a project asset (currency symbol
  of `P` and token name of project owner's public key hash), and a datum with
  additional information about the project,

  - A change UTxO sent back to project owner's wallet.


## Donation to a Project

`V` on the other hand, will require the presence of a token minted by `P`
(which identifies the target project) and only then allows minting of vote
assets. The token names of these assets will be the public key hashes of the
donors.

To prevent noticeable growth of the datum attached to a project UTxO, `V` will
require the production of a new UTxO along with reproducing the project's
authenticity UTxO. But it will also require the datum of that UTxO to be
updated such that it'll only increase the total received funds (rather than
appending donors' information).

So a donation transaction will comprise of these inputs:

  - As many UTxOs necessary to cover the donation by the donor,

  - The singular UTxO of the project that carries its authenticity asset
  (minted by `P`), and a datum carrying other project info, and total
  Lovelace received so far.

And these outputs:

  - The project UTxO with an updated datum. The only field that should be
  updated is the total Lovelaces received. This leads to a negligable growth.

  - A UTxO carrying the donated Lovelaces, an equal amount of `V` (where its
  token name is used to store the donor's public key hash), and a datum that
  holds the target project's information,

  - A change output going back to the donor's wallet.



