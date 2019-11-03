{-# LANGUAGE DeriveGeneric #-}

module Fa
  ( Structure(..)
  , State(..)
  , Symbol
  , StateName
  , Transition
  , states
  , statesFinal
  , alphabet
  , transitions
  ) where

import           ClassyPrelude
import           Data.Aeson

data Structure =
  Structure
    { _states      :: (State, [State]) -- ^ The fst state is the starting state.
    , _alphabet    :: [Symbol]
    , _transitions :: [Transition]
    }
  deriving (Eq, Show, Generic)

data State =
  State
    { _accepting :: Bool
    , _name      :: StateName
    }
  deriving (Eq, Show, Generic)

type Symbol = Text

type StateName = Text

type Transition = (StateName, Symbol, StateName)

statesWith :: (State -> Maybe State) -> Structure -> Text
statesWith f fa = mconcat . intersperse ";;" $ _name <$> states'
  where
    states' = catMaybes $ fmap f <$> uncurry (:) $ _states fa

states :: Structure -> Text
states = statesWith Just

statesFinal :: Structure -> Text
statesFinal = statesWith isFinal
  where
    isFinal s@(State True _) = Just s
    isFinal _                = Nothing

alphabet :: Structure -> Text
alphabet = mconcat . intersperse ";;" . _alphabet

showTransition :: Transition -> Text
showTransition (from, through, to) = from <> " --" <> through <> ">> " <> to

transitions :: Structure -> Text
transitions = mconcat . intersperse "\n" . fmap showTransition . _transitions

instance ToJSON Structure

instance FromJSON Structure

instance ToJSON State

instance FromJSON State
