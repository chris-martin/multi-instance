{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import MultiInstance

import Control.Monad   (when)
import Hedgehog        (Property, checkParallel, discover, property, withTests,
                        (===))
import Numeric.Natural (Natural)
import System.Exit     (exitFailure)
import System.IO       (hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    ok <- checkParallel $$discover
    when (not ok) exitFailure

prop_1 :: Property
prop_1 = withTests 1 $ property $
    multi'fold @Addition [2, 3, 5] === (10 :: Integer)

prop_2 :: Property
prop_2 = withTests 1 $ property $
    multi'append @Addition 6 7 === (13 :: Integer)

prop_3 :: Property
prop_3 = withTests 1 $ property $
    multi'append @Multiplication 6 7 === (42 :: Integer)

prop_4 :: Property
prop_4 = withTests 1 $ property $
    multi'stimes @Addition (3 :: Natural) (4 :: Integer) === 12

prop_5 :: Property
prop_5 = withTests 1 $ property $
    multi'stimes @Multiplication (3 :: Natural) (4 :: Integer) === 64

prop_6 :: Property
prop_6 = withTests 1 $ property $
    multi'fold @Addition [] === (0 :: Integer)

prop_7 :: Property
prop_7 = withTests 1 $ property $
    multi'fold @Addition [2, 3, 5] === (10 :: Integer)

prop_8 :: Property
prop_8 = withTests 1 $ property $
    multi'fold @Multiplication [] === (1 :: Integer)

prop_9 :: Property
prop_9 = withTests 1 $ property $
    multi'fold @Multiplication [2, 3, 5] === (30 :: Integer)
