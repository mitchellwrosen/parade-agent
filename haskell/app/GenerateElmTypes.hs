{-# language ScopedTypeVariables  #-}
{-# options_ghc -fno-warn-orphans #-}

import Card (Card(..), Color(..))
import Game (GameState(..), Player(..))

import Data.Proxy
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Elm
import GHC.Generics (Generic)

main :: IO ()
main = specsToDir [spec] "elm"

spec :: Spec
spec = Spec
  { namespace = ["Parade", "Types"]
  , declarations =
      [ "import Dict exposing (Dict)"
      , "import Json.Decode exposing (..)"
      , "import Json.Decode.Pipeline exposing (..)"
      , toElmTypeSource (Proxy :: Proxy Card)
      , toElmTypeSource (Proxy :: Proxy Color)
      , toElmTypeSource (Proxy :: Proxy GameState)
      , toElmTypeSource (Proxy :: Proxy Player)
      , toElmDecoderSource (Proxy :: Proxy Card)
      , toElmDecoderSource (Proxy :: Proxy Color)
      , toElmDecoderSource (Proxy :: Proxy GameState)
      , toElmDecoderSource (Proxy :: Proxy Player)
      ]
  }

deriving instance ElmType Card
deriving instance ElmType Color
deriving instance ElmType GameState
deriving instance ElmType Player

deriving instance Generic Card
deriving instance Generic Color
deriving instance Generic GameState
deriving instance Generic Player

instance ElmType IntSet where
  toElmType :: IntSet -> ElmDatatype
  toElmType _ = ElmPrimitive (EDict EInt (ElmPrimitive EUnit))

instance forall a. ElmType a => ElmType (Seq a) where
  toElmType :: Seq a -> ElmDatatype
  toElmType _ = toElmType ([] :: [a])

instance HasElmComparable Color where
  toElmComparable :: Color -> ElmPrimitive
  toElmComparable _ = EInt
