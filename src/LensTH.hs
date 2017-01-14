module LensTH
  ( elSuffixFields
  ) where

import Data.Char (toUpper)
import Language.Haskell.TH
import Lens.Micro.Platform

-- | Generate classy fields with an L suffix, e.g.
--
--     data Foo = Foo
--       { bar :: Int
--       , baz :: Bool
--       }
--     makeLensesWith elSuffixFields ''Foo
--
-- would generate
--
--     class HasBar s a | s -> a where
--       barL :: Lens' s a
--
--     class HasBaz s a | s -> a where
--       bazL :: Lens' s a
--
--     instance HasBar Foo Int where ...
--     instance HasBar Foo Bool where ...
--
elSuffixFields :: LensRules
elSuffixFields = camelCaseFields & lensField .~ namer

namer :: Name -> [Name] -> Name -> [DefName]
namer _ _ (nameBase -> field) = [MethodName cls method]
 where
  cls = mkName ("Has" ++ over _head toUpper field)
  method = mkName (field ++ "L")
