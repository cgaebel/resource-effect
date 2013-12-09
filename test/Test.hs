{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main ( main ) where

import Control.Exception (Exception, ErrorCall, catch)
import Control.Monad (void)
import Data.Typeable

import Test.Framework ( defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit hiding ( Test, State )
import Test.QuickCheck

import Control.Eff
import Control.Eff.Resource
import Control.Eff.State.Strict

main :: IO ()
main = defaultMain tests

statefulResourceTest :: Integer -> Bool
statefulResourceTest x =
  let (s, r) = run $ runState x $ runResource $ do
                modify (+ (1 :: Integer))
                let alloc = do
                      old <- get
                      put (old + (1 :: Integer))
                      return old
                    dealloc old =
                      put old
                (k, v) <- allocate alloc dealloc
                return v
   in s == x+1 && r == x+2

tests :: [Test]
tests = [
    testProperty "stateful resource" statefulResourceTest
  ]
