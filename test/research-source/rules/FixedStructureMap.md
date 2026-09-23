# Fixed Structure Map [CODE-QUALITY]

## Description

This pattern was identified in Plinth audit finding **2.2.0.5** (Cerra P2P Lending).

Map shouldn’t be used as pseudo records in datums where the map is expected to always contain a fixed set of keys and the validation logic has to manually check for those keys. Doing this allows silent omission or duplication, reducing type safety.

## Detection logic

```hs
member <string> (<datum-field> <datum>)
```

Where `<string>` could be any key in the map.

## Examples

### Valid case

```hs
data PoolDatum = PoolDatum
  { fee   :: Integer
  , owner :: PubKeyHash
  , nonce :: Integer
  }
```

### Invalid case

```hs
data PoolDatum = PoolDatum
  { extraInfo :: Map BuiltinByteString Integer
  }
-- Validation
checkDatum :: PoolDatum -> Bool
checkDatum d =
     member "fee" (extraInfo d)
  && member "owner" (extraInfo d)
  && member "nonce" (extraInfo d)
```
