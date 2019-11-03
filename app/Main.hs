module Main
  ( main
  ) where

import           ClassyPrelude
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding   as T
import           Text.Pretty.Simple   (pPrint)

import qualified Fa
import qualified Ga
import qualified Transform

tryObtain :: Maybe a -> IO a
tryObtain Nothing  = fail "Unparsable input was given."
tryObtain (Just a) = pure a

loop :: Monad m => m () -> m ()
loop action = do
  action
  loop action

getWsntLine :: IO Text
getWsntLine = filter noWs <$> getLine
  where
    noWs = (/=) ' '

menuGa :: Ga.Structure -> IO ()
menuGa ga = do
  putStrLn "0 - Exit."
  putStrLn "1 - Display the nonterminals."
  putStrLn "2 - Display the terminals."
  putStrLn "3 - Display the productions."
  putStrLn "4 - Display a given production."
  putStrLn "5 - Verify regularity of the grammar."
  putStrLn "6 - Generate a FA and show it."
  optionInput <- getWsntLine
  case optionInput of
    "1" -> putStrLn $ Ga.nonTerminals ga
    "2" -> putStrLn $ Ga.terminals ga
    "3" -> putStrLn $ Ga.productions ga
    "4" -> do
      putStrLn "Input your production name: "
      productionInput <- getWsntLine
      case Ga.specificProduction ga productionInput of
        Nothing -> putStrLn "That production is not present."
        Just p  -> putStrLn p
    "5" ->
      putStrLn $
      if Ga.isRegular ga
        then "It is regular."
        else "It is not regular"
    "6" -> pPrint $ Transform.gaToFa ga
    "0" -> fail "Program finished."
    _ -> fail "Not a valid option."

menuFa :: Fa.Structure -> IO ()
menuFa fa = do
  putStrLn "0 - Exit."
  putStrLn "1 - Display the states."
  putStrLn "2 - Display the alphabet."
  putStrLn "3 - Display the transitions."
  putStrLn "4 - Display the final states."
  putStrLn "5 - Generate a RG and show it."
  optionInput <- getWsntLine
  case optionInput of
    "1" -> putStrLn $ Fa.states fa
    "2" -> putStrLn $ Fa.alphabet fa
    "3" -> putStrLn $ Fa.transitions fa
    "4" -> putStrLn $ Fa.statesFinal fa
    "5" -> pPrint $ Transform.faToGa fa
    "0" -> fail "Program finished."
    _   -> fail "Not a valid option."

main :: IO ()
main = do
  putStrLn "1 - Read from STDIN."
  putStrLn "2 - Read from file."
  optionInput <- getWsntLine
  putStrLn "1 - Read RG."
  putStrLn "2 - Read FA."
  optionData <- getWsntLine
  case (optionInput, optionData) of
    ("1", "1") ->
      getLine >>= tryObtain . decode . BSL.fromStrict . T.encodeUtf8 >>=
      loop . menuGa
    ("1", "2") ->
      getLine >>= tryObtain . decode . BSL.fromStrict . T.encodeUtf8 >>=
      loop . menuFa
    ("2", "1") ->
      BSL.readFile "input.ga" >>= tryObtain . decode >>= loop . menuGa
    ("2", "2") ->
      BSL.readFile "input.fa" >>= tryObtain . decode >>= loop . menuFa
    _ -> fail "Not a valid option."
