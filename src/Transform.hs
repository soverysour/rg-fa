module Transform
  ( gaToFa
  , faToGa
  ) where

import           ClassyPrelude

import qualified Fa
import qualified Ga

terminalName :: Text
terminalName = "TERMINAL"

terminalState :: Fa.State
terminalState = Fa.State True terminalName

nonTermToState :: Bool -> Ga.NonTerminal -> Fa.State
nonTermToState accepting (Ga.NonTerminal name) = Fa.State accepting name

hasEpsilonTransition :: Ga.NonTerminal -> [Ga.Production] -> Bool
hasEpsilonTransition (Ga.NonTerminal name) = any epsilonRhs
  where
    epsilonRhs (Ga.Production (Ga.NonTerminal name') [Right Ga.Epsilon]) =
      name == name'
    epsilonRhs _ = False

productionToTransition :: Ga.Production -> Maybe Fa.Transition
productionToTransition (Ga.Production (Ga.NonTerminal name) rhs) =
  case rhs of
    [Right (Ga.Terminal termName)] -> Just (name, termName, terminalName)
    [Right (Ga.Terminal termName), Left (Ga.NonTerminal nontermName)] ->
      Just (name, termName, nontermName)
    _ -> Nothing

gaToFa :: Ga.Structure -> Fa.Structure
gaToFa (Ga.Structure (start, nonterms) terms prods) =
  Fa.Structure (start', states') alphabet transitions
  where
    states = nonTermToState False <$> nonterms
    states' = terminalState : states
    start' = nonTermToState (hasEpsilonTransition start prods) start
    alphabet = catMaybes $ Ga.terminalToName <$> terms
    transitions = catMaybes $ productionToTransition <$> prods

termToState :: Fa.State -> (Ga.NonTerminal, Maybe Ga.Production)
termToState (Fa.State accepting name) =
  let nonterm = Ga.NonTerminal name
   in if accepting
        then (nonterm, Just $ Ga.Production nonterm [Right Ga.Epsilon])
        else (nonterm, Nothing)

transitionToProduction :: (Text -> Bool) -> Fa.Transition -> [Ga.Production]
transitionToProduction isTerminal (from, through, to) =
  let nonterm = Ga.NonTerminal from
      production = Ga.Production
               nonterm
               [Right (Ga.Terminal through), Left (Ga.NonTerminal to)]
   in if isTerminal to
        then [Ga.Production nonterm [Right (Ga.Terminal through)], production]
        else [production]

faToGa :: Fa.Structure -> Ga.Structure
faToGa (Fa.Structure (start, states) alphabet transitions) =
  Ga.Structure (start', nonterms) terms prods'
  where
    nonterms = Ga.NonTerminal <$> filter (/= terminalName) (Fa._name <$> states)
    (start', maybeEpsilonProduction) = termToState start
    terms = Ga.Terminal <$> alphabet
    isTerminal x = foldr (||) False $ fmap (\y -> if x == Fa._name y then Fa._accepting y else False) states
    prods = transitionToProduction isTerminal <$> transitions
    prods' =
      case maybeEpsilonProduction of
        Nothing -> mconcat prods
        Just p  -> p : mconcat prods
