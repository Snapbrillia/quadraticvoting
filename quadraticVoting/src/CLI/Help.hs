module CLI.Help
  ( generic
  , forGenerating
  , forEndpoint
  ) where


import qualified Data.List as List


generic :: String
generic =
  -- {{{
     "\nQVF off-chain assistive CLI application."
  ++ "\n"
  ++ "\nYou can separately print the guide for each endpoint with"
  ++ "\n(-h|--help|man) following the desired action. Available"
  ++ "\nendpoints are:"
  ++ "\n"
  ++ "\nSmart Contract Initiation:"
  ++ "\n\tqvf-cli generate scripts --help"
  ++ "\n"
  ++ "\nSmart Contract Interaction:"
  ++ "\n\tqvf-cli register-project            --help"
  ++ "\n\tqvf-cli donate-to-project           --help"
  ++ "\n\tqvf-cli contribute                  --help"
  ++ "\n\tqvf-cli fold-donations              --help"
  ++ "\n\tqvf-cli consolidate-donations       --help"
  ++ "\n\tqvf-cli traverse-donations          --help"
  ++ "\n\tqvf-cli accumulate-prize-weights    --help"
  ++ "\n\tqvf-cli eliminate-one-project       --help"
  ++ "\n\tqvf-cli distribute-prize            --help"
  ++ "\n\tqvf-cli remove-donationless-project --help"
  ++ "\n"
  ++ "\nUtility:"
  ++ "\n\tqvf-cli pretty-datum       --help"
  ++ "\n\tqvf-cli datum-is           --help"
  ++ "\n\tqvf-cli get-constr-index   --help"
  ++ "\n\tqvf-cli data-to-cbor       --help"
  ++ "\n\tqvf-cli cbor-to-data       --help"
  ++ "\n\tqvf-cli string-to-hex      --help"
  ++ "\n\tqvf-cli get-deadline-slot  --help"
  ++ "\n\tqvf-cli current-state      --help"
  ++ "\n\tqvf-cli emulate-outcome    --help"
  ++ "\n\tqvf-cli pretty-leaderboard --help"
  ++ "\n"
  ++ "\nOr simply use (-h|--help|man) to print this help text."
  ++ "\n"
  ++ "\n"
  -- }}}


forGenerating :: String -> String
forGenerating genStr =
  -- {{{
  case genStr of
    "scripts" -> scriptGeneration
    _         -> generic
  -- }}}


forEndpoint :: String -> String
forEndpoint action =
  -- {{{
  case action of
    "update-deadline"             -> updateDeadline
    "register-project"            -> projectRegistration
    "donate-to-project"           -> donation
    "fold-donations"              -> folding
    "consolidate-donations"       -> consolidation
    "traverse-donations"          -> traversal
    "accumulate-prize-weights"    -> prizeWeightAccumulation
    "eliminate-one-project"       -> projectElimination
    "distribute-prize"            -> distribution
    "remove-donationless-project" -> donationlessRemoval
    "pretty-datum"                -> prettyDatum
    "datum-is"                    -> checkDatum
    "get-constr-index"            -> getConstr
    "data-to-cbor"                -> dataToCBOR
    "cbor-to-data"                -> cborToData
    "string-to-hex"               -> stringToHex
    "get-deadline-slot"           -> deadlineToSlot
    "current-state"               -> currentState
    "emulate-outcome"             -> emulateOutcome
    "pretty-leaderboard"          -> prettyLeaderboard
    _                             -> generic
  -- }}}


-- {{{ ENDPOINTS:
endpointDescriptionArgs :: String -> String -> String -> [String] -> String
endpointDescriptionArgs elem0 description elem1 restOfElems =
  -- {{{
  let
    elems           = elem0 : elem1 : restOfElems
    cmd             :: String
    cmd             = "qvf-cli "
    makeBlank x     = replicate x ' '
    preBlank        :: String
    preBlank        = "\t" ++ makeBlank (length cmd)
    longest         =
      -- {{{
      length $ List.maximumBy
        (\arg0 arg1 -> compare (length arg0) (length arg1))
        elems
      -- }}}
    withPostBlank e = e ++ makeBlank (longest - length e) ++ " \\\n"
    elemsWithBlanks = map ((preBlank ++) . withPostBlank) $ tail $ init elems
  in
     "\n\n\t" ++ description ++ "\n\n"
  ++ "\t" ++ cmd
  ++ withPostBlank elem0
  ++ concat elemsWithBlanks
  ++ preBlank ++ last elems ++ "\n"
  -- }}}


commonDescription :: String
commonDescription =
  -- {{{
     "Given properly formatted JSON representation of required UTxOs,"
  ++ "\n\tthis endpoint writes the proper redeemer to disk, and also"
  ++ "\n\treturns a JSON comprising of the needed UTxOs, formatted as:"
  ++ "\n\t\t{\"inputs\":  [{utxo-object}]"
  ++ "\n\t\t,\"refs\":    [{utxo-object}]"
  ++ "\n\t\t,\"outputs\": [{utxo-object}]"
  ++ "\n\t\t,\"extra\":   <extra-pieces-of-info-for-dev>"
  ++ "\n\t\t}\n"
  -- }}}


scriptGeneration :: String
scriptGeneration =
  -- {{{
  endpointDescriptionArgs
    "generate scripts"
    (    "Generate the compiled Plutus validation and minting scripts.\n\n"

      ++ "\tThe JSON for file names should have these fields:\n\n"
      ++ "\t\t{ ocfnPreDir             :: String\n"
      ++ "\t\t, ocfnProjectsPreDir     :: String\n"
      ++ "\t\t, ocfnGovernanceMinter   :: String\n"
      ++ "\t\t, ocfnRegistrationMinter :: String\n"
      ++ "\t\t, ocfnDonationMinter     :: String\n"
      ++ "\t\t, ocfnQVFMainValidator   :: String\n"
      ++ "\t\t, ocfnDeadlineSlot       :: String\n"
      ++ "\t\t, ocfnDeadlineDatum      :: String\n"
      ++ "\t\t, ocfnInitialGovDatum    :: String\n"
      ++ "\t\t, ocfnCurrentDatum       :: String\n"
      ++ "\t\t, ocfnUpdatedDatum       :: String\n"
      ++ "\t\t, ocfnNewDatum           :: String\n"
      ++ "\t\t, ocfnQVFRedeemer        :: String\n"
      ++ "\t\t, ocfnMinterRedeemer     :: String\n"
      ++ "\t\t, ocfnProjectTokenName   :: String\n"
      ++ "\t\t}"
    )
    "<key-holder-pub-key-hash>"
    [ "<txID>#<output-index>"
    , "<current-slot-number>"
    , "<deadline-posix-milliseconds>"
    , "{file-names-json}"
    ]
  -- }}}


updateDeadline :: String
updateDeadline =
  -- {{{
  endpointDescriptionArgs
    "update-deadline"
    (    "Given the current slot number and the new deadline in POSIX milliseconds,"
      ++ "\n\tthis endpoint writes the updated deadline datum to its corresponding"
      ++ "\n\tfile, along with the redeemer and the equivalent slot number for the"
      ++ "\n\tnew deadline."
    )
    "<current-slot-number>"
    [ "<deadline-posix-milliseconds>"
    , "{file-names-json}"
    ]
  -- }}}


projectRegistration :: String
projectRegistration =
  -- {{{
  endpointDescriptionArgs
    "register-project"
    (    "Read the current datum from disk, and write the corresponding files:\n"
      ++ "\t\t- Updated governance datum,\n"
      ++ "\t\t- New initial project datum,\n"
      ++ "\t\t- Static datum for project's info,\n"
      ++ "\t\t- Redeemer for the QVF validator,\n"
      ++ "\t\t- Redeemer for the registration policy."
    )
    "<project-owner-pub-key-hash>"
    [ "<project-name>"
    , "<project-requested-fund>"
    , "{file-names-json}"
    ]
  -- }}}


donation :: String
donation =
  -- {{{
  endpointDescriptionArgs
    "donate-to-project"
    (    "Read the current datum from disk, and write the corresponding files:\n"
      ++ "\t\t- Updated project datum,\n"
      ++ "\t\t- New donation datum,\n"
      ++ "\t\t- Redeemer for the QVF validator,\n"
      ++ "\t\t- Redeemer for the donation policy."
    )
    "<donors-pub-key-hash>"
    [ "<target-project-id>"
    , "<donation-amount>"
    , "{file-names-json}"
    ]
  -- }}}


folding :: String
folding =
  -- {{{
  endpointDescriptionArgs
    "fold-donations"
    (    "Read the current project datum from disk, and write the\n"
      ++ "\tupdated project datum to disk. Also prints a JSON\n"
      ++ "\twith two fields of \"lovelace\" and \"mint\" to help\n"
      ++ "\tthe bash script construct the `tx-out` argument.\n"
    )
    "<donation(s)-lovelace-count-0>"
    [ "<donation-count-------------0>"
    , "{donation-datum-json--------0}"
    , "<donation(s)-lovelace-count-1>"
    , "<donation-count-------------1>"
    , "{donation-datum-json--------1}"
    , "<...etc...>"
    , "{file-names-json}"
    ]
  -- }}}


consolidation :: String
consolidation =
  -- {{{
  endpointDescriptionArgs
    "consolidate-donations"
    (    "Read the current project datum from disk, and write the\n"
      ++ "\tupdated project datum to disk. Also prints a JSON\n"
      ++ "\twith two fields of \"lovelace\" and \"mint\" to help\n"
      ++ "\tthe bash script construct the `tx-out` argument.\n"
      ++ "\tThe value in the \"lovelace\" field is the amound that\n"
      ++ "\tshould be added to the input main UTxO.\n"
    )
    "<donation(s)-lovelace-count-0>"
    [ "<donation-count-------------0>"
    , "{folded-donation-datum-json-0}"
    , "<donation(s)-lovelace-count-1>"
    , "<donation-count-------------1>"
    , "{folded-donation-datum-json-1}"
    , "<...etc...>"
    , "{file-names-json}"
    ]
  -- }}}


traversal :: String
traversal =
  -- {{{
  endpointDescriptionArgs
    "traverse-donations"
    (    "Takes information of two input fully folded donation UTxOs,\n"
      ++ "\tcompares their donations, and if duplicates where found,\n"
      ++ "\tit'll return a JSON object with \"lovelace0\" and \"lovelace1\"\n"
      ++ "\tfields (as resolving the duplication requires exchange of\n"
      ++ "\tsome Lovelaces). Reallocation of donation assets doesn't\n"
      ++ "\tseem necessary at this point.\n"
      ++ "\tIf there are no overlaps, the \"Nothing\" string is returned.\n"
    )
    "<donations-lovelace-count-0>"
    [ "{donations-datum-json-----0}"
    , "<donations-lovelace-count-1>"
    , "{donations-datum-json-----1}"
    , "{file-names-json}"
    ]
  -- }}}


prizeWeightAccumulation :: String
prizeWeightAccumulation =
  -- {{{
  endpointDescriptionArgs
    -- inputCntStr : govInputStr : infoInputsStr : projInputsStr : fileNamesJSON
    "accumulate-prize-weights"
    commonDescription
    "<number-of-projects-to-process>"
    [ "{governance-input-utxo}"
    , "[{array-of-project-info-utxos}]"
    , "[{array-of-project-state-utxos}]"
    , "{file-names-json}"
    ]
  -- }}}


projectElimination :: String
projectElimination =
  -- {{{
  endpointDescriptionArgs
    -- govInputStr : infoInputsStr : projInputsStr : registeredProjsStr : fileNamesJSON
    "eliminate-one-project"
    (    commonDescription
      ++ "\n\tEach registered project's JSON should have 3 fields:"
      ++ "\n\t\t- \"pkh\": The public key hash (PKH) of the project owner,"
      ++ "\n\t\t- \"address\": The encoded (CIP19) address of said PKH,"
      ++ "\n\t\t- \"tn\": The token name (i.e. identifier) of the project."
      ++ "\n"
    )
    "{governance-input-utxo}"
    [ "[{array-of-project-info-utxos}]"
    , "[{array-of-project-state-utxos}]"
    , "[{array-of-registered-projects}]"
    ]
  -- }}}


distribution :: String
distribution =
  -- {{{
  endpointDescriptionArgs
    -- ownerAddrStr : govInputStr : infoInputStr : projInputStr : fileNamesJSON
    "distribute-prize"
    commonDescription
    "<project-owners-address>"
    [ "{governance-input-utxo}"
    , "{project-info-utxo}"
    , "{project-state-utxo}"
    , "{file-names-json}"
    ]
  -- }}}


donationlessRemoval :: String
donationlessRemoval =
  -- {{{
  endpointDescriptionArgs
    -- govInputStr : infoInputStr : projInputStr : fileNamesJSON
    "remove-donationless-project"
    (    "Given properly formatted JSON representation of required UTxOs,"
      ++ "\n\tthis endpoint writes the proper redeemers to disk, along with"
      ++ "\n\tthe updated datum."
    )
    "{governance-input-utxo}"
    [ "{project-info-utxo}"
    , "{project-state-utxo}"
    , "{file-names-json}"
    ]
  -- }}}


prettyDatum :: String
prettyDatum =
  -- {{{
  endpointDescriptionArgs
    "pretty-datum"
    "Print an easy-to-read parsing of a given datum JSON:"
    "{current-datum-json-value}"
    []
  -- }}}


checkDatum :: String
checkDatum =
  -- {{{
  endpointDescriptionArgs
    "datum-is"
    (    "Check whether a given data JSON decodes to a specific `QVFDatum`"
      ++ "\n\t(returns either \"True\" or \"False\"). Supported keywords are:"
      ++ "\n\t\tDeadlineDatum"
      ++ "\n\t\tProjectInfo"
      ++ "\n\t\tDonationAccumulationConcluded"
    )
    "<predicate-keyword>"
    ["{current-datum-json-value}"]
  -- }}}


getConstr :: String
getConstr =
  -- {{{
  endpointDescriptionArgs
    "get-constr-index"
    (    "Given a constructor from `QVFDatum`, this endpoints returns the\n"
      ++ "\tconstructor index of the `Data` equivalent.\n"
    )
    "<datum-constructor>"
    []
  -- }}}


dataToCBOR :: String
dataToCBOR =
  -- {{{
  endpointDescriptionArgs
    "data-to-cbor"
    "Return the CBOR encoding of a given JSON formatted `Data` value:"
    "{arbitrary-data-json-value}"
    []
  -- }}}


cborToData :: String
cborToData =
  -- {{{
  endpointDescriptionArgs
    "cbor-to-data"
    "Attempt decoding a CBOR to a JSON formatted `Data` value:"
    "<cbor-hex-string>"
    []
  -- }}}


stringToHex :: String
stringToHex =
  -- {{{
  endpointDescriptionArgs
    "string-to-hex"
    "Export the hex serialization of a token name:"
    "<token-name>"
    ["<output.hex>"]
  -- }}}


deadlineToSlot :: String
deadlineToSlot =
  -- {{{
  endpointDescriptionArgs
    "get-deadline-slot"
    "Convert the deadline in a given datum to the slot number:"
    "<current-slot-number>"
    ["<current.datum>"]
  -- }}}

currentState :: String
currentState =
  -- {{{
  endpointDescriptionArgs
    "current-state"
    (    "Given a list of all currency symbols that are considered authentic"
      ++ "\n\tby the validator, along with all the UTxOs sitting at the script"
      ++ "\n\taddress, this endpoint returns an object with 3 fields:"
      ++ "\n"
      ++ "\n\t1. \"deadline\": POSIX time in milliseconds for the deadline of"
      ++ "\n\t   the funding round."
      ++ "\n"
      ++ "\n\t2. \"matchPool\": Lovelace count available in the match pool."
      ++ "\n"
      ++ "\n\t3. \"donations\": An array of objects that map token names to"
      ++ "\n\t   an array of objects that map public key hashes of donors to"
      ++ "\n\t   their amount of donations in Lovelaces. In this object, next"
      ++ "\n\t   to the token name field, a \"total\" field carries the total"
      ++ "\n\t   raised Lovelaces for convenience."
      ++ "\n"
      ++ "\nIn short:"
      ++ "\n\t  { \"deadline\" : int"
      ++ "\n\t  , \"matchPool\": int"
      ++ "\n\t  , \"donations\":"
      ++ "\n\t      [ { \"$tn\"  : [{\"$pkh\": int}]"
      ++ "\n\t        , \"total\": int"
      ++ "\n\t        }"
      ++ "\n\t      ]"
      ++ "\n\t  }"
    )
    "[<currency-symbol>]"
    [ "[{script-utxo}]"
    ]
  -- }}}

emulateOutcome :: String
emulateOutcome =
  -- {{{
  endpointDescriptionArgs
    "emulate-outcome"
    (    "Given a list of all currency symbols that are considered authentic"
      ++ "\n\tby the validator, along with all the UTxOs sitting at the script"
      ++ "\n\taddress, this endpoint returns an object with 2 fields:"
      ++ "\n"
      ++ "\n\t1. \"infos\": An array of objecs that each carry distribution"
      ++ "\n\t   information of a project:"
      ++ "\n"
      ++ "\n\t\tDistributionInformation"
      ++ "\n\t\t  { \"tn\"                : Hex string of project's token name"
      ++ "\n\t\t  , \"requested\"         : Project's funding goal"
      ++ "\n\t\t  , \"raised\"            : Donations received"
      ++ "\n\t\t  , \"raisedAfterFee\"    : Donations after deduction of the platform fee"
      ++ "\n\t\t  , \"matchPool\"         : Rightful portion from the match pool"
      ++ "\n\t\t  , \"matchPoolAfterFee\" : Match pool portion after deduction of the fee"
      ++ "\n\t\t  , \"prizeWeight\"       : Sum of the square roots of all the received donations, squared"
      ++ "\n\t\t  , \"ratio\"             : Value showing how much of the funding goal had been reached"
      ++ "\n\t\t  }"
      ++ "\n"
      ++ "\n\t   Note that if `ratio` is less than 1, it means that the project"
      ++ "\n\t   had not been eligible to take any money from the match pool."
      ++ "\n\t   Therefore, the net Lovelace count they are going to get is the"
      ++ "\n\t   same as `raisedAfterFee`. Since `ratio` is found by dividing the"
      ++ "\n\t   sum of `raised` and `matchPool` by `requested` (where `matchPool`"
      ++ "\n\t   is found through dividing `prizeWeight` by the total sum of all"
      ++ "\n\t   the remaining projects' prize weights) at the time of elimination,"
      ++ "\n\t   this set of information allows a \"leaderboard\" to represent how"
      ++ "\n\t   close an eliminated project had been at the time of elimination."
      ++ "\n"
      ++ "\n\t2. \"inputs\": A sorted array of UTxOs which, essentially, is a"
      ++ "\n\t   shorter representation of the input UTxO objects that resulted"
      ++ "\n\t   in the accompanied list of distribution information objects."
    )
    "[<currency-symbol>]"
    [ "[{script-utxo}]"
    ]
  -- }}}

prettyLeaderboard :: String
prettyLeaderboard =
  -- {{{
  endpointDescriptionArgs
    "pretty-leaderboard"
    (    "Given a list of all currency symbols that are considered authentic"
      ++ "\n\tby the validator, along with all the UTxOs sitting at the script"
      ++ "\n\taddress, this endpoint prints a \"prettified\" representation of"
      ++ "\n\tthe current outcome of the funding round:"
    )
    "[<currency-symbol>]"
    [ "[{script-utxo}]"
    ]
  -- }}}
-- }}}
