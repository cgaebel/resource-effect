{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Concurrent.MVar
import Control.Exception (Exception, ErrorCall, catch)
import Control.Monad (void)
import Data.Functor
import Data.Typeable

import Test.Framework ( defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit hiding ( Test, State )
import Test.QuickCheck
import Test.QuickCheck.Property

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Resource

main :: IO ()
main = defaultMain tests

statefulResourceTest :: Integer -> Property
statefulResourceTest x = morallyDubiousIOProperty $ do
  m <- newMVar True
  v <- runLift $ runResource $ do
                let alloc = swapMVar m False
                    dealloc old = () <$ swapMVar m old
                (k, v) <- allocate alloc dealloc
                return v
  mv <- takeMVar m
  return $ v && mv

tests :: [Test]
tests = [
    testProperty "stateful resource" statefulResourceTest
  ]
