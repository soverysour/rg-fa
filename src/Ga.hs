{-# LANGUAGE DeriveGeneric #-}

module Ga
  ( Structure(..)
  , NonTerminal(..)
  , Terminal(..)
  , Production(..)
  , nonTerminals
  , terminals
  , productions
  , specificProduction
  , isRegular
  , terminalToText
  , terminalToName
  ) where

import           ClassyPrelude
import           Data.Aeson
import           Data.Maybe    (listToMaybe)

nonTerminals :: Structure -> Text
nonTerminals ga = mconcat . intersperse ";;" $ _nonterminalName <$> nonterms
  where
    nonterms = uncurry (:) $ _nonterminals ga

terminalToText :: Terminal -> Text
terminalToText (Terminal n) = n
terminalToText Epsilon      = "ε"

terminalToName :: Terminal -> Maybe Text
terminalToName (Terminal n) = Just n
terminalToName _            = Nothing

productions :: Structure -> Text
productions ga =
  mconcat . intersperse "\n" $ productionToText <$> _productions ga

specificProduction :: Structure -> Text -> Maybe Text
specificProduction ga name =
  productionToText <$> (listToMaybe . catMaybes $ guard' isThatOne <$> prods)
  where
    prods = _productions ga
    isThatOne prod = name == getName prod
    getName = _nonterminalName . _prodLhs
    guard' f a =
      if f a
        then Just a
        else Nothing

productionToText :: Production -> Text
productionToText (Production lhs rhs) = _nonterminalName lhs <> " -> " <> rhs'
  where
    rhs' = mconcat $ either _nonterminalName terminalToText <$> rhs

terminals :: Structure -> Text
terminals ga = mconcat . intersperse ";;" $ terminalToText <$> terms
  where
    terms = _terminals ga

isRegular :: Structure -> Bool
isRegular (Structure nonterms _ prods) = all isRegularRhs prods
  where
    start = fst nonterms
    isRegularRhs (Production lhs rhs) =
      case rhs of
        [Right (Terminal _)]         -> True
        [Right (Terminal _), Left _] -> True
        [Right Epsilon]              -> lhs == start
        _                            -> False

data Structure =
  Structure
    { _nonterminals :: (NonTerminal, [NonTerminal]) -- ^ The fst nonterminal is the starting symbol.
    , _terminals    :: [Terminal]
    , _productions  :: [Production]
    }
  deriving (Eq, Show, Generic)

newtype NonTerminal =
  NonTerminal
    { _nonterminalName :: Text
    }
  deriving (Eq, Show, Generic)

data Terminal
  = Terminal
      { _terminalName :: Text
      }
  | Epsilon
  deriving (Eq, Show, Generic)

data Production =
  Production
    { _prodLhs :: NonTerminal
    , _prodRhs :: [Either NonTerminal Terminal]
    }
  deriving (Eq, Show, Generic)

instance FromJSON Structure

instance ToJSON Structure

instance FromJSON NonTerminal

instance ToJSON NonTerminal

instance FromJSON Terminal

instance ToJSON Terminal

instance FromJSON Production

instance ToJSON Production
