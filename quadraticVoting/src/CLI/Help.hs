module CLI.Help
  ( generic
  , forGenerating
  , forEndpoint
  ) where


import qualified Data.List as List


generic :: String
generic =
  -- {{{
     "\nQVF off-chain assistive CLI application.\n\n"

  ++ "You can separately print the argument guide for each action\n"
  ++ "with (-h|--help|man) following the desired action. Available\n"
  ++ "endpoints are:\n"

  ++ "\nSmart Contract Interaction:\n"
  ++ "\tqvf-cli generate scripts         --help\n"
  ++ "\tqvf-cli register-project         --help\n"
  ++ "\tqvf-cli donate-to-project        --help\n"
  ++ "\tqvf-cli contribute               --help\n"
  ++ "\tqvf-cli fold-donations           --help\n"
  ++ "\tqvf-cli consolidate-donations    --help\n"
  ++ "\tqvf-cli traverse-donations       --help\n"
  ++ "\tqvf-cli accumulate-prize-weights --help\n"
  ++ "\tqvf-cli eliminate-one-project    --help\n"
  ++ "\tqvf-cli distribute-prize         --help\n"
  ++ "\nUtility:\n"
  ++ "\tqvf-cli pretty-datum      --help\n"
  ++ "\tqvf-cli datum-is          --help\n"
  ++ "\tqvf-cli get-constr-index  --help\n"
  ++ "\tqvf-cli data-to-cbor      --help\n"
  ++ "\tqvf-cli cbor-to-data      --help\n"
  ++ "\tqvf-cli string-to-hex     --help\n"
  ++ "\tqvf-cli get-deadline-slot --help\n\n"

  ++ "Or simply use (-h|--help|man) to print this help text.\n\n"
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
    "register-project"         -> projectRegistration
    "donate-to-project"        -> donation
    "fold-donations"           -> folding
    "consolidate-donations"    -> consolidation
    "traverse-donations"       -> traversal
    "accumulate-prize-weights" -> prizeWeightAccumulation
    "eliminate-one-project"    -> projectElimination
    "distribute-prize"         -> distribution
    "pretty-datum"             -> prettyDatum
    "datum-is"                 -> checkDatum
    "get-constr-index"         -> getConstr
    "data-to-cbor"             -> dataToCBOR
    "cbor-to-data"             -> cborToData
    "string-to-hex"            -> stringToHex
    "get-deadline-slot"        -> deadlineToSlot
    _                          -> generic
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
    -- govInputStr : infoInputStr : projInputStr : ownerAddrStr : fileNamesJSON
    "distribute-prize"
    commonDescription
    "{governance-input-utxo}"
    [ "{project-info-utxos}"
    , "{project-state-utxos}"
    , "<project-owners-address>"
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
    (    "Check whether a given data JSON decodes to a specific `QVFDatum`\n"
      ++ "\t(returns either \"True\" or \"False\"). Supported keywords are:\n"
      ++ "\t\tDeadlineDatum\n"
      ++ "\t\tProjectInfo"
      ++ "\t\tDonationAccumulationConcluded"
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
-- }}}
