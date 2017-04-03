module QRCode.ECLevel exposing (ECLevel(..))

{-|
@docs ECLevel
-}


{-| Error correction level. Provides the following error
correction capability:

- **L** (Low): 7% of codewords can be restored.
- **M** (Medium): 15% of codewords can be restored.
- **Q** (Quartile): 25% of codewords can be restored.
- **H** (High): 30% of codewords can be restored.

-}

type ECLevel
    = L
    | M
    | Q
    | H
