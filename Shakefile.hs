#!/usr/bin/env stack
{- stack
    runghc
    --package basic-prelude
    --package shake
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake makefile for project.
--
import BasicPrelude
import Data.Char
import Development.Shake

-- | This file used for version change detection.
--
thisFile :: FilePath
thisFile = "Shakefile.hs"

-- | Location of build supporting files.
--
buildDir :: FilePath
buildDir = ".build"

-- | Location of stack's work files.
--
stackDir :: FilePath
stackDir = ".stack-work"

-- | Build directory where "touch" files are kept.
--
fakeDir :: FilePath
fakeDir = buildDir </> "fake"

-- | Fake directory path builder.
--
fd :: FilePath -> FilePath
fd = (fakeDir </>)

-- | Meta directory where "virtual" files are kept.
--
metaDir :: FilePath
metaDir = buildDir </> "meta"

-- | Meta directory path builder.
--
md :: FilePath -> FilePath
md = (metaDir </>)

-- | Remove right excess on string.
--
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

-- | Typeful command args with return string.
--
cmdArgs :: String -> [String] -> Action String
cmdArgs c as = rstrip . fromStdout <$> cmd c as

-- | Typeful command args with no return.
--
cmdArgs_ :: String -> [String] -> Action ()
cmdArgs_ c as = unit $ cmd c as

-- | Stack command.
--
stack :: [String] -> Action ()
stack = cmdArgs_ "stack"

-- | Sylish command.
--
stylish :: [String] -> Action ()
stylish = cmdArgs_ "stylish-haskell"

-- | Lint command.
--
lint :: [String] -> Action ()
lint = cmdArgs_ "hlint"

-- | Git command.
--
git :: [String] -> Action String
git = cmdArgs "git"

-- | m4 command.
--
m4 :: [String] -> Action String
m4 = cmdArgs "m4"

-- | Version.
--
version :: Action String
version = git [ "describe", "--tags", "--abbrev=0" ]

-- | Touch a file for fake files.
--
touchFile :: FilePath -> Action ()
touchFile = flip writeFile' mempty

-- | Use a fake file to keep track of the last time an file-free action ran.
--
fake :: [FilePattern] -> String -> ([FilePath] -> Action ()) -> Rules ()
fake pats target act = do
  fd target %> \out -> do
    files <- getDirectoryFiles "." pats
    need files
    act files
    touchFile out

  phony target $
    need [ fd target ]

-- | Use a meta file to keep track of vitual content
--
meta :: FilePath -> Action String -> Rules ()
meta target act =
  md target %> \out -> do
    alwaysRerun
    content <- act
    writeFileChanged out content

-- | Preprocess a file with m4
--
preprocess :: FilePattern -> FilePath -> Action [(String, String)] -> Rules ()
preprocess target file macros =
  target %> \out -> do
    need [ file ]
    let f k v = "-D" <> k <> "=" <> v
    macros' <- macros
    content <- m4 $ file : (uncurry f <$> macros')
    writeFileChanged out content

-- | Global rules
--
globalRules :: Rules ()
globalRules = do
  let pats =
        [ "stack.yaml"
        , "Shakefile.hs"
        , "src//*.hs"
        ]

  -- | version
  --
  meta "version"
    version

  -- | shakers.cabal
  --
  preprocess "shakers.cabal" "shakers.cabal.m4" $ do
    need [ md "version" ]
    v <- version
    return [ ("VERSION", v) ]

  -- | build
  --
  fake pats "build" $ \_files -> do
    need [ "shakers.cabal" ]
    stack [ "build", "--fast" ]

  -- | build-error
  --
  fake pats "build-error" $ \_files -> do
    need [ "shakers.cabal" ]
    stack [ "build", "--fast", "--ghc-options=-Werror" ]

  -- | ghci
  --
  phony "ghci" $ do
    need [ "shakers.cabal" ]
    stack [ "ghci", "--fast" ]

  -- | install
  --
  fake pats "install" $ \_files -> do
    need [ "shakers.cabal" ]
    stack [ "build", "--fast", "--copy-bins" ]

  -- | publish
  --
  phony "publish" $ do
    need [ "shakers.cabal" ]
    stack [ "sdist" ]
    stack [ "upload", ".", "--no-signature" ]

  -- | clean
  --
  phony "clean" $ do
    need [ "shakers.cabal" ]
    stack [ "clean" ]
    removeFilesAfter buildDir [ "//*" ]

  -- | clear
  --
  phony "clear" $
    forM_ [ fakeDir ] $ \dir ->
      removeFilesAfter dir [ "//*" ]

  -- | wipe
  --
  phony "wipe" $ do
    removeFilesAfter buildDir [ "//*" ]
    removeFilesAfter stackDir [ "//*" ]

  -- | sanity
  --
  phony "sanity" $
    need [ "build-error", "lint" ]

-- | Haskell source rules
--
hsRules :: Rules ()
hsRules = do
  let pats =
        [ "Shakefile.hs"
        , "src//*.hs"
        ]

  -- | format
  --
  fake pats "format" $ \files -> do
    need [ ".stylish-haskell.yaml" ]
    stylish $ [ "-c", ".stylish-haskell.yaml", "-i" ] <> files

  -- | lint
  --
  fake pats "lint" $ \files ->
    lint files

-- | Main entry point
--
main :: IO ()
main = do
  v <- getHashedShakeVersion [thisFile]
  shakeArgs shakeOptions { shakeFiles = buildDir, shakeVersion = v } $ do
    want [ "build-error", "lint", "format" ]
    globalRules
    hsRules
