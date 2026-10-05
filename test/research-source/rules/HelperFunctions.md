# Helper Functions [CODE-QUALITY] [PERFORMANCE]

## Description

Helper functions that only pattern match or call another function with fixed arguments could be inlined. Could help minimize unnecessary script size and execution overhead.

## Detection logic

**Pattern 1 - Function only pattern matches and forwards values:**

```hs
<fun> :: <typeA> -> <typeB>
<fun> <constructor1> = ...
<fun> <constructorN> = ...
```

Where `<typeA>` and `<typeB>` could be any type, and `<constructor>` refers to the constructors of `<typeA>`.
It could be preferable, in some cases, to inline the pattern matching instead of creating a helper function.

**Pattern 2 - Function immediatly calls another function:**

```hs
<fun1> :: <typeA> -> <typeB>
<fun1> <arg> = <fun2> <arg>
```

Where `<fun1>` simply calls `<fun2>`, causing unnecessary overhead.

## Examples

### Valid case

```hs
-- Simple validator that checks tx signature
mkValidator :: PubKeyHash -> ScriptContext -> Bool
mkValidator pkh ctx =
  txSignedBy (scriptContextTxInfo ctx) pkh
```

### Invalid case

```hs
{-# INLINABLE isAdmin #-}
isAdmin :: PubKeyHash -> TxInfo -> Bool
isAdmin pkh info =
  txSignedBy info pkh

-- Could simply call txSignedBy directly instead of defining a helper function
mkValidator :: PubKeyHash -> ScriptContext -> Bool
mkValidator pkh _ ctx =
  isAdmin pkh (scriptContextTxInfo ctx)
```
