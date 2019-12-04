{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import Control.Monad.Trans.Maybe

newtype App = App {appLogFunc :: LogFunc}

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

testJust :: Maybe Int
testJust = return 42

main :: IO ()
main = do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf ->
    let app = App {appLogFunc = lf}
     in runRIO app run

run :: RIO App ()
run = do
  wut <- runMaybeT inner
  case wut of
    Nothing -> logInfo "there was Nothing"
    Just (s1, s2) -> logInfo ("there is some numberThere " <> displayShow s1 <> " and " <> displayShow s2)


inner :: MaybeT (RIO App) (Int, Int)
inner = do
  something <- MaybeT $ return testJust
  somethingElse <- MaybeT $ return testJust
  logInfo "all good"
  return (something, somethingElse)
