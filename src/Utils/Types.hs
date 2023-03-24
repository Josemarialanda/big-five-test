{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils.Types where

import Data.Kind
  ( Constraint,
    Type,
  )
import Data.Type.Bool
  ( If,
    Not,
  )
import Data.Type.Equality (type (==))
import GHC.TypeLits
  ( ErrorMessage
      ( ShowType,
        Text,
        (:<>:)
      ),
    Symbol,
    TypeError,
  )

-- | Error thrown when a symbol is not a member of an empty list
type EmptyMemberList a =
  'Text "Could not match " ':<>: 'ShowType a ':<>: 'Text " with any of the members of the empty list"

-- | Error thrown when a symbol is not a member of a list of symbols
type MemberNotInList a as =
  'ShowType a ':<>: 'Text " is not a member of the list " ':<>: 'ShowType as

-- mmm. wonder if I can get rid of the repeated list argument

-- | This type family checks if a symbol is a member of a list of symbols
type family IsMember (a :: Symbol) (as :: [Symbol]) (xs :: [Symbol]) :: Constraint where
  IsMember a '[] xs = TypeError (EmptyMemberList a)
  IsMember a '[b] xs = If (a == b) (() :: Constraint) (TypeError (MemberNotInList a xs))
  IsMember a (b ': bs) xs = If (Not (a == b)) (IsMember a bs xs) (() :: Constraint)
