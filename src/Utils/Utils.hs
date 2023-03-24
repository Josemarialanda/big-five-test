module Utils.Utils where

import qualified Text.Regex.TDFA as R
import Data.Data (Proxy (Proxy),Typeable)
import GHC.TypeLits (symbolVal, KnownSymbol)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

removeRepeatingSeparator :: Char -> String -> (String, String)
removeRepeatingSeparator c s = (s11,s22)
  where 
  (s11,s12) = span (/=c) s
  s22       = dropWhile (==c) s12

matchToRegex :: String -> String -> Maybe [String]
matchToRegex regex text = case R.getAllTextMatches $ text R.=~ regex of
    [] -> Nothing
    xs -> return xs

printTag :: forall tag. KnownSymbol tag => Proxy tag -> String
printTag _ = symbolVal $ Proxy @tag

parseNumber :: String -> Int
parseNumber num = fromMaybe (error $ "Could not parse " <> show num) (readMaybe num)

parseListOfNumbers :: [String] -> [Int]
parseListOfNumbers = fmap parseNumber