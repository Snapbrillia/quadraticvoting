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

To keep the contract involved as much as is necessary (but not any more), and
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

We'll call the QVF "governing" script `G`. This script is going to be
parametrized by an NFT, which we'll call `S`. To distinguish UTxOs, we will 
also need two other minting scripts: one for registering projects, which we'll
use `P` to address its currency symbol; and one for donating to projects (with
a currency symbol of `V`).


### Initiation

Let's consider the flow:

- The key holder mints `S`, which can be done in two different ways:

  - Using a known script, and storing the spent UTxO in the token name: This
  approach has the benefit of more trust by the users. The "recognizability" of
  the policy ID means that users can rest assured that this token is truly an
  NFT. The downside of this approach is that it leads to larger scripts. With
  Vasil, this may not be a credible downside.

  - Using a custom script, and specifying a minimal token name for the NFT:
  This leads to smaller script sizes, but may lead to less trust.

- At this point, the key holder can generate `P`, which leads to project
  registration minter's "awareness" of `S`, and therefore capable of validating
  its presence in a registration transaction.

- With `P`'s currency symbol found, `V` can now be generated. Since donation
  transactions don't require the presence of `S`, it suffices to make `V` aware
  of `P` tokens.

- Subsequently, parametrizing `G` by both `S` and `P`, will make it capable of
  validating UTxO authenticity, and check that a transaction that's attempting
  to spend its singular UTxO for the purpose of registering a new project is
  in fact minting an asset with a symbol of `P`.

In short, key holder will mint `S`. `P` will be parametrized by `S`. `V` will
be parametrized by `P`. And finally, `G` will be parametrized by both `S` and 
`P` (should it also be parametrized by `V`?).


### Project Registration

Project registration transactions (`Tx(P)` for short) will be responsible of
generating UTxOs at `G`, each carrying a `P` asset.

To ensure uniqueness of a project UTxO, the token names of their `P` assetes
are going to be one of the UTxOs that the project owner will be consuming
during the registration. This allows the off-chain platform to robustly
distinguish between available projects for donors, and relieves the `G` UTxO
from keeping a record of registered projects.

The datum attached to these UTxOs will store additional information
about its corresponding project (public key hash for receiveing the prize,
label, etc.), along with a record of amount of Lovelaces received by that
project. This will be the only field in the datum that will be subject to
updates.

Therefore, a `Tx(P)` will consist of these inputs:

  - As many UTxOs necessary to cover the registration fee by the project owner,

  - The singular UTxO from the governing script carrying its authenticity asset
  (`S`). Note that `P` can not be aware of `G` and can only check the
  _presence_ of `S`. This UTxO should also have a datum attached that tracks
  the number of projects registered so far (does that suffice?).

And these outputs:

  - The "governing" UTxO sent back to `G` with an updated datum (i.e.
  incremented by 1),

  - A fresh UTxO sent to `G`, carrying a project asset (`P` with an
  identification token name, i.e. one of the UTxOs being spent in this
  transaction), and a datum with additional information about the project,

  - A change UTxO sent back to project owner's wallet.


### Donation to a Project

A donation transaction (`Tx(V)`) on the other hand, will require the presence
of a token minted by `P` (which identifies the target project) and only then
its `V` allows minting of vote assets. The token names of these assets will be
the public key hashes of the donors.

To prevent noticeable growth of the datum attached to a project UTxO, `Tx(V)` will
require the production of a new UTxO along with reproducing the project's
authenticity UTxO. But it will also require the datum of that UTxO to be
updated such that it'll only increase the total received funds (rather than
appending donors' information).

So a `Tx(V)` will comprise of these inputs:

  - As many UTxOs necessary to cover the donation by the donor,

  - The singular UTxO of the project that carries its authenticity asset
  (minted by `P`), and a datum carrying other project info, and total
  Lovelace received so far.

And these outputs:

  - The project UTxO with an updated datum. The only field that should be
  updated is the total Lovelaces received. This leads to a negligable growth
  for subsequent transactions,

  - A UTxO carrying the donated Lovelaces, an equal amount of `V` (where its
  token name is used to store the donor's public key hash), and a datum that
  holds the target project's information (probably only its identification
  UTxO),

  - A change output going back to the donor's wallet.


### Contribution to the Match Pool

Since this endpoint is not meant to store any information about the donor,
there is also no need to mint any intermediate tokens. The contributor can
simply consume the singular UTxO at `G` and increase its Lovelaces.

Transaction inputs:

  - As many UTxOs necessary to cover the donation by the donor,

  - The singular UTxO from the governing script (`S`).

Transaction outputs:

  - The governing UTxO sent back to `G` with an unchanged datum.

  - A change output going back to the donor's wallet.


### Prize Distribution

At the end of a funding round, this architecture results in a potentially large
number of scattered UTxOs (all marked with either `S`, `P`, or `V`). Since a
single transaction is likely unable to handle them all, there should be steps
to help reduce the UTxO count.

As each project UTxO accounts for their total received Lovelaces, these UTxOs
can start burning the vote tokens accumulated within multiple UTxOs.

Disregarding the size limitation of transactions, this would mean there should
be a transaction for each project, where the donation UTxOs are "folded" into a
singular UTxO, carrying the project's portion of the total funding pool.


