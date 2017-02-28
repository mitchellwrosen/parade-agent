#!/usr/bin/env stack
{- stack
    --resolver lts-8.3
    exec
    --package shake
    ghc
    --
    -o make
    -odir .build
    -hidir .build
    -rtsopts
    -with-rtsopts=-I0
-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

opts :: ShakeOptions
opts = shakeOptions
  -- Use as many threads as processors
  { shakeThreads = 0

  -- Print entire commands run, not just prog name
  , shakeVerbosity = Loud
  }

main :: IO ()
main =
  shakeArgs opts $ do
    want
      -- The makefile itself is a target to rebuild every time this file
      -- changes. This avoids running with runghc every time (which has a
      -- noticeable startup time).
      [ "make"

      -- The auto-generated elm modules.
      , "elm/Parade/Types.elm"
      ]

    "make" %> \out -> do
      need [".make.hs"]
      cmd "./.make.hs"

    "elm/Parade/Types.elm" %> \out -> do
      need ["haskell/app/GenerateElmTypes.hs"]
      () <-
        cmd "stack --stack-yaml haskell/stack.yaml build parade-agent:exe:generate-elm-types"
      cmd "stack --stack-yaml haskell/stack.yaml exec generate-elm-types"
