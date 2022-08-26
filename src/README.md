# A Tokenized Approach to Limit the Growth of Datum Values

We faced two primary issues with the first version of our Quadratic Voting and
Funding (QVF) contract.

First, the transaction fees kept rising after each interaction with the
contract. The reason for this was the fact that we stored every data inside the
datum (i.e. state) of the contract's singular UTxO (authenticated by an NFT).

The second (not so unrelated) problem was the limited number of
projects/donations the contract could store. Since the datum needed to be
provided in each transaction, this meant that at some point, this growing datum
would reach the size limit of a transaction, and would not be able to accept
any further registrations/donations.

The purpose of this document is to first present a design framework for
addressing possible solutions for the afformentioned problems. And after
proposing a solution, some of the technical details are covered.


## Design Framework

This contract has a "growing state" in its nature. Therefore, to allow a large
number of interactions, this state should somehow be sharded. However, "naive"
sharding (i.e. simply multiplying the number of states accounted for by the
contract) doesn't address the issue of rising transaction fees. Because each
shard still keeps growing.

In order to keep the transaction fees as constant as possible, the transactions
need to be as identical as possible—meaning a prior interaction should not
have a significant impact on its succeeding transaction.

One other constraint to have in mind for presenting solutions to the problems
is that a smart contract needs to be "kept in the loop." The main reason for
that is the fact that a contract has no control over _incoming_ UTxOs.
Therefore the contract should remain conscious of the overal state.


## The Solution

To address the sharding, and also keeping the transactions as identical as
possible, this solution aims to dedicate singular UTxOs for each donation or
project. This way, the *outputs* of transactions are going to be almost
identical (only differing because of the interactions themselves).

To keep the contract involved as much as is necessary (but not any less), and
also having the *inputs* of transactions as similar as possible, this solution
defines a datum structure that is meant to keep track of "total counts" of
various values (e.g. only keeping track of the _number_ of registered projects
rather than a datatype that stores all the information).  This results in a
state that doesn't change much between two interactions (increase/decrease of 
an integer value).

This can be possible by "delegating" the task of storing parts of the
information inside native tokens (their token names, to be precise). These
tokens are also going to act as "secondary authentication tokens" that allow
the contract to distinguish between authentic and rogue UTxOs.


## Technical Details

The idea is to have a "governing" script (`G`), a project registration minting
script (`P`), and a vote token minting script (`V`).


## Initiation

Let's consider the flow:

- The key holder will use an NFT minting script to mint an authentication
  token (`S`).

  This NFT minting can be done in two different ways:

  - Using a known script, and storing the spent UTxO in the token name: This
  approach has the benefit of more trust by the users. The "recognizability" of
  the policy ID means that users can rest assured that this token is truly an
  NFT. The downside of this approach is that it leads to larger scripts. With
  Vasil, this may not be a credible downside.

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



