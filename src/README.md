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
- [Validation Layout for Transactions](#validation-layout-for-transactions)
  - [NFT Minting Transaction](#nft-minting-transaction)
  - [Project Registration Transaction](#project-registration-transaction)
  - [Donation Transaction](#donation-transaction)
  - [First Phase of Folding Donations](#first-phase-of-folding-donations)
  - [Second Phase of Folding Donations](#second-phase-of-folding-donations)
  - [Accumulation of Donations](#accumulation-of-donations)
  - [Key Holder Fee Collection](#key-holder-fee-collection)
  - [Prize Distribution Transaction](#prize-distribution-transaction)


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

To ensure uniqueness of a project UTxO, the token names of their `P` assets
are going to be one of the UTxOs that the project owner will be consuming
during the registration. This allows the off-chain platform to robustly
distinguish between available projects for donors, and relieves the `G` UTxO
from keeping a record of registered projects.

The datum attached to these UTxOs will store additional information
about its corresponding project (public key hash for receiveing the prize,
label, etc.), along with a record of the number of donations received by that
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
UTxO to be updated such that it'll only increment the total received donations
(rather than appending donors' information).

So a `Tx_v` will comprise of these inputs:

  - As many UTxOs necessary to cover the donation by the donor,

  - The singular UTxO of the project that carries its authenticity asset
  (minted by `P`), and a datum carrying other project info, and total count of
  donations received so far.

And these outputs:

  - The project UTxO with an updated datum. The only field that should be
  updated is the total donation count received. This leads to a negligable
  growth for subsequent transactions,

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
| Each input                                     | 38 bytes  |
| Each wallet output                             | 67 bytes  |
| Each key value pair (public key hash, amount)  | 68 bytes  |
| One signature                                  | 102 bytes |
| Each redeemer without data                     | 14 bytes  |

We'll go over why these particular elements are of interest.

Another important number we can find is a transaction with:
- A single input,
- A single wallet output,
- A single script output with:
    - A script hash,
    - Some Lovelaces,
    - An asset with a 32-byte long token name and a relatively large amount,
    - And an array containing a single empty object as the inline datum,
- An empty array for collaterals,
- A single script reference UTxO,
- Fee,
- Both sides of the TTL interval set,
- Integrity hash,
- One signature,
- An empty array for redeemers,
- No datums and scripts in witness set (not even empty arrays).

The decoded CBOR of such a transaction looks like this:
```
[ { 0:  [ [h'30AB6FF0B6137E32836B6F51C5B1411C65030BF2E64F605B8409D51E552FF802', 256]
        ]
  , 1:  [ { 0: h'003A56F24E6E474ECC440555F0F4948101148694C4727AD8AC05AFF0B9F809DFED2456621369A039E767A9438D8FD8BAD8AE32FD0F7041B1C0'
          , 1: 7311569000000
          }
        , { 0: h'70311E7CBDA49831695C2D51DB0F2E92816BD4AF8CBFA453DC5AF77277'
          , 1: [ 7600000000000
               , { h'499D7F6DCDE3B31912A742497A3A294488D416E59216A9AA47776238':
                     { h'5FAE3E5B2722EEF3A82A85775052EFB4F76901312AC200F4E28059DF51F7306C': 18000000000
                     }
                 }
               ]
          , 2: [122([])]
          }
        ]
  , 2:  71879200000
  , 3:  67439744000
  , 4:  67441447000
  , 11: h'5F0690B995666E8DE6B7BBA8A68341F268C6578E2EBD5A310C915DE0F94B425E'
  , 13: []
  , 18: [ [h'5FAE3E5B2722EEF3A82A85775052EFF4E28059DF51F7306CB4F76901312AC200', 256]
        ]
  }
, { 0:  [ [ h'B7A9392B498B97FF41D01EAC21A22634AF9E841593549B6F2AF9E1DC67DFF78B'
          , h'148C6E21009F92FE9D364385E4208DFBE61F16AA73438D90C87564F944EAD7FAD3BF996788725215E652DEF2AFFB1978F19A7C602D000B775969E05804704208'
          ]
        ]
  , 5:  []
  }
, true
, null
]
```

Such a transaction has a size of _**454 bytes**_. Since, where applicable, the
integer values used are bigger than necessary, we can consider this the upper
bound of the overhead in each transaction. Therefore, we can round a bit more
conservatively, and end up with _**15900 bytes**_ free space to populate.

To find the final size of a folding transaction, there are a number of variable
elements needed to add:
- Inputs (count of `x`),
- A redeemer for each input (count of `x`, data size of `r`),
- Collaterals (count of `c`),
- Output datum (i.e. map from public key hashes to amounts, size of `y`).

We can arrive at an equation for finding the number of inputs and outputs with
given `c`, `r`, and `s` (described below):
```
s           # number of bytes to slice off of public key hashes
t = 38      # byte size increase per input
u = 68 - s  # byte size increase per additional public key hash
R = 14 + r  # byte size increase per redeemer
H = 15900   # available byte count

H = ct + Rx + tx + uy

Rx + tx + uy = H - ct = C(c)

By setting: C(c) = H - ct, we'll get:

                              ┌───────────────────┐
                              │ (R + t)x + uy = C │
                              └───────────────────┘

Which is always true.

-------------------------------------------------------------------------------

For the final folding transaction, we can set y = 1, which gives us the number
of inputs that this transaction can accept (x_f):

                               ┌───────────────┐
                               │        C - u  │
(R + t)x_f + u = C     =>      │ x_f = ─────── │
                               │        R + t  │
                               └───────────────┘

-------------------------------------------------------------------------------

Another boundry condition, is the first reduction, in which the number of
inputs and outputs are equal, i.e. x = y:

                           ┌─────────────────────────┐
                           │                  C      │
(R + t)x_0 + ux_0 = C  =>  │ x_0 = y_0 = ─────────── │
                           │              R + t + u  │
                           └─────────────────────────┘

-------------------------------------------------------------------------------

Assuming the whole reduction of the donations into the weight portion (w_p)
occurs in two phases, we can find the upper limit of donation UTxOs by
multiplying x_0 and x_f:

                      ┌──────────────────────────────────┐
                      │                 C         C - u  │
                      │ T(c,r,s) = ─────────── * ─────── │
                      │             R + t + u     R + t  │
                      └──────────────────────────────────┘

To find a ballpark value, we can set:
  c = 5 => C = 15710
  r = 6 => R = 20
  s = 0 => u = 68
  and      t = 38

  therefore: T = 124.68 * 269.69 = 33624.95
```

The last transaction should either burn the vote assets, or simply leave them
at the script address. However, to ensure that they are not carried along with
the project UTxOs (and consequently inflating the upcoming transaction fees),
it might be preferred to enforce their burning.


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


## Validation Layout for Transactions

This section is meant to be accompanied by
[the flow diagram of the contract](https://figma.com/file/rbmPW7o6ol8aICbB4l318a/Quadratic-Voting-and-Funding).
Each subsection below will elaborate on its corresponding dashed square in the
diagram.

The color purple for a transaction indicates that there are no minting/burning
meant to occur. While red or green transactions refer to their corresponding
minting policies (red for `P`, and green for `V`).

### NFT Minting Transaction

At this point the funding round is not initiated yet. Which means the only
script executed here would be the NFT minter (i.e. `S`). The color yellow is
used to refer to this NFT, hence the color of the transaction, and the UTxO
that carries `S` inside.

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


### Project Registration Transaction

Since this transaction is meant to update the main datum, it has to spend the
singular authenticated UTxO of the contract (i.e. the yellow UTxO), which means
the QVF validator has to be executed.

This transaction also produces a new UTxO carrying a freshly minted `P` token.
Therefore the project minter will also be executed.

These are all the conditions required for this transaction to be valid:

- The deadline has not passed,

- The transaction is signed by the project's owner (i.e. prize recepient),

- Exactly 1 `S` is being spent from the script, and 1 is being sent back to the
the script address (from this point forward, we'll refer to this condition
as "`X` is present"),

- Datum attached to the UTxO carrying `S` is proper, and increments the number
of registered projects by 1,

- Exactly 1 token is being minted (`P`), such that its token name is the unique
identifier of the project (i.e hash of the specified UTxO),

- The output UTxO carrying this fresh `P` token, also carries the registration
fee's Lovelaces.

To avoid redundancy, most of these checks can probably be done only by the
validator. And to prevent arbitrary minting, presence of `S` might suffice to
lock the two scripts together.


### Donation Transaction

As this task is delegated to the target project's UTxO (i.e. the red arrow in
the diagram), this transaction does not require the involvement of the main
UTxO.

This transaction also executes two scripts: the validator, and the donation
minter.

The required conditions are:

- The deadline has not passed,

- The transaction is signed by the donor,

- `P` is present. The token name of this asset needs to match that of target
project's identifier,

- Datum attached to the UTxO carrying `P` is proper, and increments the number
of donations by 1,

- Exactly 1 token is being minted (`V`), such that its token name is the
project's identifier,

- The output UTxO carrying this fresh `V` token, also carries the intended
donation in Lovelaces, while its datum stores the donor's public key hash.


### First Phase of Folding Donations

Since there is no minting or burning happening in this transaction, only the
validator is executed.

The required conditions are:

- The deadline has passed,

- The transaction is signed by the key holder,

- `P` is present,

- A set number of donation UTxOs are being spent. This number is the lesser
value between a maximum (this maximum will be achieved at a later point in
development, current estimate is ~100), and the total number of unfolded
donations,

- All the donations are stored in the datum of the singular output UTxO, which
carries all the input `V` tokens.


### Second Phase of Folding Donations

To prevent the `P` UTxO from carrying all its `V` tokens (which leads to higher
fees), this transaction will burn the donation tokens. Therefore the donation
minter will also be executed here.

The required conditions are:

- The deadline has passed,

- The transaction is signed by the key holder,

- `P` is present,

- All the `V` tokens (i.e. the number of donations) are being burnt,

- The output `P` UTxO carries all the donated Lovelaces, and its datum carries
the computed "prize weight" for this project.


### Accumulation of Donations

At this point the contract expects only numerous `P` UTxOs available at the
script address, such that all of them have their "prize weight" stored in their
datum.

Here, accumulation means that the main UTxO needs to traverse all of
these `P` UTxOs, collect their Lovelaces, and accumulate their weight values in
order to find the final denominator in the QVF equation. No tokens should be
burnt.

The required conditions are:

- The deadline has passed,

- The transaction is signed by the key holder,

- The same number of spent `P` UTxOs are being produced, each having a properly
updated datum, and having all their received donations depleted (except for the
registration fees),

- `S` is present,

- Output `S` carries all the consumed donations, and has a properly updated
datum to keep track of the number of projects traversed so far.


### Key Holder Fee Collection

After the final donation accumulation transaction, the contract is expected to
have the project UTxOs (all of which are marked as "depleted"), and also
the `S` UTxO (i.e. the yellow one in the diagram), such that its datum signals
the conclusion of accumulation, and that the key holder fee is still
not collected.

This transaction should consume the main UTxO, collect the fee, and flag the
datum accordingly.

The required conditions are:

- The deadline has passed,

- The transaction is signed by the key holder,

- `S` is present, and properly updated.


### Prize Distribution Transaction

Now that the key holder fee is deduced from the pool, each project wallet
should receive its portion (i.e. their "prize weight" divided by the sum of all
the weights).

If a project has won more than they had requested, the excess should go into a
new UTxO (still carrying the `P` token) with an "escrow" datum.

On the other hand, if the won amount is equal or less than the requested
amount, there is no reason to preserve the `P` token and the registration fee.
Therefore we may (?) burn the token, and after deducing the transaction fee
from the registration fee, the rest can be sent to the project owner.

The required conditions are:

- The deadline has passed,

- `S` is present,

- `P` is present,

- A proper portion from the prize pool is sent to the associated project's
wallet,

- If there is excess money with respect to the requested amount, the `P` token
should be preserved to hold the excess as an escrow account,

- If the won prize is equal or less than the requested amount, the `P` token
should be burnt, and the prize (along with the registration) fee should be sent
to the project's owner.


