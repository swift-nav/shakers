#!/usr/bin/env stack
{- stack
    runghc
    --package shakers
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake makefile for project.
--
import Development.Shakers

-- | Main entry point.
--
main :: IO ()
main = shakeMain $ do
  let pats =
        [ "stack.yaml"
        , "Shakefile.hs"
        , "src//*.hs"
        ]
      pats' = delete "stack.yaml" pats

  -- | Haskell rules.
  --
  hsRules "." pats'

  -- | Cabal rules.
  --
  cabalRules "." "shakers.cabal"

  -- | Stack rules.
  --
  stackRules "." pats

  -- | Default things to run.
  --
  want [ "build-error", "lint" ]
