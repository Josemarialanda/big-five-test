{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import qualified Data.Aeson as A
import qualified Data.List.Split as S
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol)
import qualified Text.Regex.TDFA as R
import Utils.Types (IsMember)
import Utils.Utils (printTag)

-- | Type level list of facets.
type HasFacet = '["EXTRAVERSION", "AGREEABLENESS", "CONSCIENTIOUSNESS", "NEUROTICISM", "OPENNESS TO EXPERIENCE"]

-- | Type constraint that a symbol represents a facet.
type IsFacet facet = (KnownSymbol facet, IsMember facet HasFacet HasFacet)

-- | The basic parser type.
newtype Parser a = Parser {runParser :: String -> Either String a}

-- | The result type we want to parse into.
data Results = Results
  { results'name :: Text,
    results'email :: Text,
    results'extraversion :: Facet "EXTRAVERSION",
    results'agreeableness :: Facet "AGREEABLENESS",
    results'conscientiousness :: Facet "CONSCIENTIOUSNESS",
    results'neuroticism :: Facet "NEUROTICISM",
    results'opennesToExperience :: Facet "OPENNESS TO EXPERIENCE"
  } deriving Show

instance A.ToJSON Results where
  toJSON :: Results -> A.Value
  toJSON (Results a1 a2 a3 a4 a5 a6 a7) =
    A.object
      [ "NAME" A..= a1,
        "EMAIL" A..= a2,
        "EXTRAVERSION" A..= a3,
        "AGREEABLENESS" A..= a4,
        "CONSCIENTIOUSNESS" A..= a5,
        "NEUROTICISM" A..= a6,
        "OPENNESS TO EXPERIENCE" A..= a7
      ]

data ExistentialFacet = forall facet. IsFacet facet => ExistentialFacet {unFacet :: Facet facet}

data Facet facet = Facet
  { f'overallScore :: Int,
    f'scores :: Scores facet
  } deriving Show

instance KnownSymbol facet => A.ToJSON (Facet facet) where
  toJSON :: Facet facet -> A.Value
  toJSON Facet {..} =
    A.object
      [ "Overall Score" A..= f'overallScore,
        "Facets" A..= (f'scores :: Scores facet)
      ]

data Scores facet = Scores
  { f'score1 :: Int,
    f'score2 :: Int,
    f'score3 :: Int,
    f'score4 :: Int,
    f'score5 :: Int,
    f'score6 :: Int
  } deriving Show

instance KnownSymbol facet => A.ToJSON (Scores facet) where
  toJSON :: Scores facet -> A.Value
  toJSON Scores {..} = case printTag (Proxy @facet) of
    "EXTRAVERSION" -> extraversionFacet f'score1 f'score2 f'score3 f'score4 f'score5 f'score6
    "AGREEABLENESS" -> agreeablenessFacet f'score1 f'score2 f'score3 f'score4 f'score5 f'score6
    "CONSCIENTIOUSNESS" -> conscientiousnessFacet f'score1 f'score2 f'score3 f'score4 f'score5 f'score6
    "NEUROTICISM" -> neuroticismFacet f'score1 f'score2 f'score3 f'score4 f'score5 f'score6
    "OPENNESS TO EXPERIENCE" -> opennesToExperienceFacet f'score1 f'score2 f'score3 f'score4 f'score5 f'score6
    _ -> error $ "Cannot parse facet " <> printTag (Proxy @facet) <> "."

extraversionFacet :: Int -> Int -> Int -> Int -> Int -> Int -> A.Value
extraversionFacet a1 a2 a3 a4 a5 a6 =
  A.object
    [ "Friendliness" A..= a1,
      "Gregariousness" A..= a2,
      "Assertiveness" A..= a3,
      "Activity Level" A..= a4,
      "Excitement-Seeking" A..= a5,
      "Cheerfulness" A..= a6
    ]

agreeablenessFacet :: Int -> Int -> Int -> Int -> Int -> Int -> A.Value
agreeablenessFacet a1 a2 a3 a4 a5 a6 =
  A.object
    [ "Trust" A..= a1,
      "Morality" A..= a2,
      "Altruism" A..= a3,
      "Cooperation" A..= a4,
      "Modesty" A..= a5,
      "Sympathy" A..= a6
    ]

conscientiousnessFacet :: Int -> Int -> Int -> Int -> Int -> Int -> A.Value
conscientiousnessFacet a1 a2 a3 a4 a5 a6 =
  A.object
    [ "Self-Efficacy" A..= a1,
      "Orderliness" A..= a2,
      "Dutifulness" A..= a3,
      "Achievement-Striving" A..= a4,
      "Self-Discipline" A..= a5,
      "Cautiousness" A..= a6
    ]

neuroticismFacet :: Int -> Int -> Int -> Int -> Int -> Int -> A.Value
neuroticismFacet a1 a2 a3 a4 a5 a6 =
  A.object
    [ "Anxiety" A..= a1,
      "Anger" A..= a2,
      "Depression" A..= a3,
      "Self-Consciousness" A..= a4,
      "Immoderation" A..= a5,
      "Vulnerability" A..= a6
    ]

opennesToExperienceFacet :: Int -> Int -> Int -> Int -> Int -> Int -> A.Value
opennesToExperienceFacet a1 a2 a3 a4 a5 a6 =
  A.object
    [ "Imagination" A..= a1,
      "Artistic Interests" A..= a2,
      "Emotionality" A..= a3,
      "Adventurousness" A..= a4,
      "Intellect" A..= a5,
      "Liberalism" A..= a6
    ]
