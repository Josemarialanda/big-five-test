module Parser where

import GHC.Generics ( Generic )
import GHC.TypeLits               ( KnownSymbol )
import qualified Data.List.Split as S
import qualified Data.Aeson      as A
import qualified Text.Regex.TDFA as R
import Types
    ( Facet(Facet),
      Scores(Scores),
      IsFacet,
      ExistentialFacet(ExistentialFacet, unFacet),
      Parser(Parser),
      Results(..) )
import Utils.Utils ( removeRepeatingSeparator, matchToRegex, printTag, parseNumber ) 
import Data.Data (Proxy (Proxy))


extractResults :: String -> Maybe [String]
extractResults = matchToRegex testResultsRegex
  where
  testResultsRegex :: String
  testResultsRegex = "([a-zA-Z0-9-]|([a-zA-Z0-9-] [a-zA-Z0-9-]))+(\\.+)([0-9][0-9])"

-- | Parse a list of strings (raw results) into Results (parsed results).
parseResults :: [String] -> Results
parseResults = results . -- Convert list of existentially typed facets to a Results
               fmap existentialFacet . -- Convert list of facet text into a list of existentially typed facets
               S.chunksOf 7 . -- 7 because each facet has 6 scores and 1 overall score
               (fmap . removeRepeatingSeparator $ '.') -- Remove repeating separators from testResultsRegex
  where
  -- | Parse a list of scores into a Score
  facetScores :: forall facet. IsFacet facet => [(String, String)] -> Scores facet
  facetScores xs = case compare (length xs) 6 of
    LT -> error $ "Less scores than expected for " <> printTag (Proxy :: Proxy facet)
    GT -> error $ "More scores than expected for " <> printTag (Proxy :: Proxy facet)
    EQ -> let [(_,s1),(_,s2),(_,s3),(_,s4),(_,s5),(_,s6)] = xs 
          in Scores (parseNumber s1) (parseNumber s2) (parseNumber s3) (parseNumber s4) (parseNumber s5) (parseNumber s6)

  -- | Parse a list of facet text into a Facet.
  -- Cannot construct a Facet of the wrong type. (try to, and you'll get a type error)
  existentialFacet :: [(String, String)] -> ExistentialFacet
  existentialFacet (("EXTRAVERSION",s):fs)           = ExistentialFacet (Facet (parseNumber s) (facetScores fs) :: Facet "EXTRAVERSION")
  existentialFacet (("AGREEABLENESS",s):fs)          = ExistentialFacet (Facet (parseNumber s) (facetScores fs) :: Facet "AGREEABLENESS")
  existentialFacet (("CONSCIENTIOUSNESS",s):fs)      = ExistentialFacet (Facet (parseNumber s) (facetScores fs) :: Facet "CONSCIENTIOUSNESS")
  existentialFacet (("NEUROTICISM",s):fs)            = ExistentialFacet (Facet (parseNumber s) (facetScores fs) :: Facet "NEUROTICISM")
  existentialFacet (("OPENNESS TO EXPERIENCE",s):fs) = ExistentialFacet (Facet (parseNumber s) (facetScores fs) :: Facet "OPENNESS TO EXPERIENCE")
  existentialFacet ((facet,_):_)                     = error $ "Unknown facet: " <> show facet

  -- TODO: This is a bit of a hack, but it works for now...
  -- Although not really hehe, because chunksOf and fmap do not change the order of the elements, and we know 
  -- that our facets are of the correct type by construction, so we can just use typedFacet to convert them.
  -- i.e it may be kind of a hack, but it's a safe hack.
  -- Admittedly, it's not very elegant, I got a bit lazy here :P
  -- | Parse a list of existentially typed facets into a Results.
  results :: [ExistentialFacet] -> Results
  results [ ExistentialFacet f1
          , ExistentialFacet f2
          , ExistentialFacet f3
          , ExistentialFacet f4
          , ExistentialFacet f5] = Results 
          { results'name                = "-"
          , results'email               = "-"
          , results'extraversion        = typedFacet @"EXTRAVERSION" f1
          , results'agreeableness       = typedFacet @"AGREEABLENESS" f2
          , results'conscientiousness   = typedFacet @"CONSCIENTIOUSNESS" f3
          , results'neuroticism         = typedFacet @"NEUROTICISM" f4 
          , results'opennesToExperience = typedFacet @"OPENNESS TO EXPERIENCE" f5
          }
  results _ = error "Impossible error!"

-- | This parser is used to parse the results from the big5report.
big5reportParser :: Parser Results
big5reportParser = Parser $ \input -> case extractResults input of
  Nothing -> Left "Regex fail: Could not extract results from input"
  Just results -> Right $ parseResults results

-- | This function is used to convert an existential facet to a typed facet.
-- | Kinda like a typecast, but not really.
typedFacet :: forall facet t. IsFacet facet => Facet t -> Facet facet
typedFacet (Facet overallScore (Scores s1 s2 s3 s4 s5 s6)) = Facet overallScore (Scores s1 s2 s3 s4 s5 s6)