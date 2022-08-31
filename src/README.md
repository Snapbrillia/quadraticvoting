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


## Table of Contents
- [Design Framework](#design-framework)
- [The Solution](#the-solution)
- [Technical Details](#technical-details)
  - [Initiation](#initiation)
  - [Project Registration](#project-registration)
  - [Donation to a Project](#donation-to-a-project)
  - [Contribution to the Match Pool](#contribution-to-the-match-pool)
  - [Prize Distribution](#prize-distribution)
    - [Mathematical Detour to Find the Practical Limits](#mathematical-detour-to-find-the-practical-limits)
    - [Folding the Project UTxOs](#folding-the-project-utxos)
- [Logical Layout for Minting Scripts](#logical-layout-for-minting-scripts)
  - [NFT Minter](#nft-minter)
  - [Project Registration Minter](#project-registration-minter)
    - [`RegisterProject`](#projectregistration)
    - [`DistributePrize`](#distributeprize)
  - [Donation Minter](#donation-minter)
    - [`DonateToProject`](#donatetoproject)
    - [`FoldDonations`](#folddonations)


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

- The key holder mints `S`, an NFT.

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

Project registration transactions (`Tx_p` for short) will be responsible of
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

Therefore, a `Tx_p` will consist of these inputs:

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

A donation transaction (`Tx_v`) on the other hand, will require the presence
of a token minted by `P` (which identifies the target project) and only then
its `V` allows minting of vote assets. The token names of these assets will be
the target project's identifier.

To prevent noticeable growth of the datum attached to a project
UTxO, `Tx_v` will require the production of a new UTxO along with reproducing
the project's authenticity UTxO. But it will also require the datum of that
UTxO to be updated such that it'll only increase the total received funds
(rather than appending donors' information).

So a `Tx_v` will comprise of these inputs:

  - As many UTxOs necessary to cover the donation by the donor,

  - The singular UTxO of the project that carries its authenticity asset
  (minted by `P`), and a datum carrying other project info, and total
  Lovelace received so far.

And these outputs:

  - The project UTxO with an updated datum. The only field that should be
  updated is the total Lovelaces received. This leads to a negligable growth
  for subsequent transactions,

  - A UTxO carrying the donated Lovelaces, a single `V` asset (where its token
  name is the target project's identifier), and a minimal datum that holds the
  donor's public key hash.

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
number of scattered UTxOs (all marked with either `S`, `P`, or `V`).

This would mean there should be a transaction for each project, where the
donation UTxOs are folded into a singular UTxO, carrying the project's portion
of the total funding pool (`w_p`).

Due to the size limit of transactions however, a single transaction may not be
able to handle all the vote UTxOs associated to a project.


#### Mathematical Detour to Find the Practical Limits

By observing the CBOR of a signed transaction, we can arrive at these values:

| Element                                        | Size      |
|------------------------------------------------|-----------|
| Each input                                     | 36 bytes  |
| Each wallet output                             | 67 bytes  |
| Each key value pair (public key hash, amount)  | 35 bytes  |
| One signature                                  | 102 bytes |
| One redeemer without data                      | 15 bytes  |

We'll go over why these particular elements are of interest.

Another important number we can find is a transaction with:
- An empty array for inputs,
- A single wallet output,
- A single script output with:
    - A script hash,
    - Some Lovelaces,
    - An asset with a 32-byte long token name and a relatively large amount,
    - And an array containing a single empty object as the inline datum,
- An empty array for collaterals,
- Fee,
- Both sides of the TTL interval set,
- Integrity hash,
- One signature,
- A redeemer with an empty object as its data,
- An empty array for datum values,
- And an empty array for scripts.

Such a transaction has a size of 368 bytes. Although, some of those elements
have integer fields, and this number has been achieved with some _typical_
values. Therefore, to simplify our calculations, we'll round it up to
_**384 bytes**_, which leads to _**16000 bytes**_ left for us to populate.

To find the final size of a folding transaction, there are a number of variable
elements needed to add:
- Inputs (count of `x`),
- Script reference inputs (count of `x`),
- Collaterals (count of `c`),
- Script output (i.e. list of public key hashes and amounts, size of `y`),
- And a redeemer data (`r`).

We can arrive at an equation for finding the number of inputs and outputs with
given `c`, `r`, and `s` (described below):
```
s           # number of bytes to slice off of public key hashes
t = 36      # byte size increase per input
u = 35 - s  # byte size increase per additional public key hash
H = 16000   # available byte count

H = ct + r + 2tx + uy

2tx + uy = H - r - ct = C(c, r)

By setting: C(c,r) = H - r - ct, we'll get:
    
                         ┌──────────────┐ 
                         │ 2tx + uy = C │
                         └──────────────┘
    
Which is always true.

-------------------------------------------------------------------------------

For the final folding transaction, we can be conservative and set y = 1, which
gives us the number of inputs that transaction can accept (x_f):

                         ┌───────────────┐ 
                         │        C - u  │
2tx_f + u = C     =>     │ x_f = ─────── │
                         │         2t    │
                         └───────────────┘

-------------------------------------------------------------------------------

Another boundry condition, is the first reduction, in which the number of
inputs and outputs are equal, i.e. x = y:

                      ┌──────────────────────┐ 
                      │                C     │
2tx_0 + ux_0 = C  =>  │ x_0 = y_0 = ──────── │
                      │              2t + u  │
                      └──────────────────────┘

-------------------------------------------------------------------------------

Assuming the whole reduction of the donations into the weight portion (w_p)
occurs in two phases, we can find the upper limit of donation UTxOs by
multiplying x_0 and x_f:

                  ┌───────────────────────────────┐ 
                  │               C        C - u  │
                  │ T(c,r,s) = ──────── * ─────── │
                  │             2t + u      2t    │
                  └───────────────────────────────┘

To find a ballpark value, we can set C = 12000 and u = 35 to find: T = 18637
```

The last transaction should either burn the vote assets, or simply leave them
at the script address. However, to ensure that they are not carried along with
the project UTxOs (and consequently inflate the upcoming transaction fees), it
might be preferred to enforce their burning.


#### Folding the Project UTxOs

After the final phase of folding donations into `w_p` values, we are left with
all the project UTxOs flagged as "consolidated."

The governing script (`G`) should consume all these UTxOs, grabbing all their
Lovelaces, but still reproducing them such that they carry their `w_p`, along
with their registration fee, and adding their `w_p` to an accumulator carried
inside its own datum.

If we were to keep a record of `w_p` values, for example, within the datum of
the governing UTxO (which carries `S`), the contract would support a limited
number of projects. This way, however, it'll only be limited by the total 
transaction fees required for distribution.

Going over all the project UTxOs, `G` will arrive at the total sum
of `w_p` values. We'll call this `W`.

Disregarding the key holder fees and refund of project registration fees, the
prize each project receives is essentially `w_p / W` times the prize pool.
Therefore, after `G` finds `W`, it can start distributing the prizes by
consuming each project's UTxO.


## Logical Layout for Minting Scripts

In this section we're going to dive a bit deeper and provide a description of
scripts' behaviors.


### NFT Minter

There are two ways to mint an NFT:

- Using a known script, and storing the spent UTxO in the token name: This
  approach has the benefit of more trust by the users. The "recognizability" of
  the policy ID means that users can rest assured that this token is truly an
  NFT. The downside of this approach is that it leads to larger scripts. With
  Vasil, this may not be a credible downside.

  Since the concatenation of transaction ID and its output index exceed 32
  bytes, the token name can not store the UTxO in raw form. Hashing the 
  concatenation is one option to get a properly sized unique byte string.

- Using a custom script, and specifying a minimal token name for the NFT:
  This leads to smaller script sizes, but may lead to less trust.


### Project Registration Minter

First and foremost, this script should be parametrized by the NFT minted
earlier. This leads to script's capability of verifying the presence of this
NFT and ensure it's being used in the intended context.

A possible redeemer datatype can be as follows:

```hs
data RegistrationRedeemer
  = RegisterProject RegistrationInfo
  | DistributePrize

data RegistrationInfo = RegistrationInfo
  { riTxOutRef   :: TxOutRef
  , riPubKeyHash :: BuiltinByteString
  , riLabel      :: BuiltinByteString
  , riRequested  :: Integer
  }
```

The second constructor of the redeemer is meant for burning the tokens (is it
necessary?).


#### `RegisterProject`

These are the conditions under which this minter should allow project
registration (i.e. minting an authentication asset for the project):

- Deadline has not passed,

- Registration fees are paid,

- Signed by the public key hash stated in `RegistrationInfo`,

- There is exactly 1 `S` present in inputs, and 1 in outputs (should it also
validate the two share the same address?). From this point forward, we'll call
this "`S` is present,"

- Datum attached to the output UTxO carrying `S` is proper,

- Exactly 1 token is being minted, such that its token name is the hash of the
specified UTxO (`riTxOutRef` in `RegistrationInfo`),

- One other output goes to the same address as the one `S` is coming from, with
a proper inline datum attached to it (we'll go over the structure of this datum
later).


#### `DistributePrize`

This endpoint is meant to be invoked for burning the project assets. Still
unsure whether this is truly needed, but let's consider the conditions under
which this minter allows burning:

- Deadline has passed,

- `S` is present,

- `P` is present,

- Project UTxO input has a datum that signals its conclusion of `w_p`,

- Exactly 1 token is being burnt, such that its token name matches the
identifier specified in the datum,

- An ouput exists which goes to the project's public key hash.


### Donation Minter

Similar to the project minter, this script should be parametrized by
the `P` currency symbol.

A possible redeemer datatype can be as follows:

```hs
data DonationRedeemer
  = DonateToProject DonationInfo
  | FoldDonations

data DonationInfo = DonationInfo
  { diProjectId :: BuiltinByteString
  , diDonor     :: BuiltinByteString
  , diAmount    :: Integer
  }
```

Let's consider the conditions for each endpoint.


#### `DonateToProject`

- Deadline has not passed,

- Donation is not less than minimum (2 ADA),

- Signed by the donor's public key hash (`diDonor` in `DonationInfo`),

- `P` is present,

- Datum attached to above is properly updated (i.e. donation count is
incremented by 1),

- Exactly 1 token is being minted, such that its token name is the target
project's identifier (`diProjectId` in `DonationInfo`).

- One other output goes to the same address as the one `P` is coming from, with
donor's public key hash as its datum,

- The above has enough Lovelaces.


#### `FoldDonations`

TODO.

