module Target.PlutusTx where

import Data.Foldable (forM_, for_)
import System.FilePath ((</>))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Text as Text

import qualified PlutusTx as Tx
import qualified PlutusTx.Builtins as B
import qualified PlutusTx.Prelude as Tx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Maybe as Maybe
import PlutusTx (UnsafeFromData(unsafeFromBuiltinData))


-- Place for future imports
--
--
--
--
--
--
--
--
--
--
--
--

assocMap :: AssocMap.Map k v
assocMap = AssocMap.unsafeFromList mempty

unsafeFromBuiltinData :: Integer
unsafeFromBuiltinData =
  Tx.unsafeFromBuiltinData (error "we don't care")

usageOfPTxMaybe :: Integer
usageOfPTxMaybe = let 
  x = Maybe.fromMaybe 0 (Maybe.Just 1)
  in x