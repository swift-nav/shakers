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

  -- | Cabal rules.
  --
  cabalRules "shakers.cabal"

  -- | Stack rules.
  --
  stackRules "shakers" pats

  -- | sanity
  --
  fake' pats "sanity" $ const $
    need [ fakeFile "build-error", "lint" ]

  -- | Default things to run.
  --
  want [ "build-error", "lint", "format" ]
