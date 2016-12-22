{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake helpers.
--
module Development.Shakers
  ( module Exports
  , fakeFile
  , metaFile
  , cmdArgs
  , cmdArgs_
  , stack
  , git
  , m4
  , fake
  , fake'
  , meta
  , preprocess
  , stackRules
  , cabalRules
  , shakeMain
  ) where

import BasicPrelude      as Exports hiding ((*>))
import Data.Char
import Development.Shake as Exports

-- | File used for version change detection.
--
shakeFile :: FilePath
shakeFile = "Shakefile.hs"

-- | Location of build supporting files.
--
buildDir :: FilePath
buildDir = ".build"

-- | Build directory where "touch" files are kept.
--
fakeDir :: FilePath
fakeDir = buildDir </> "fake"

-- | Fake file path builder.
--
fakeFile :: FilePath -> FilePath
fakeFile = (fakeDir </>)

-- | Meta directory where "virtual" files are kept.
--
metaDir :: FilePath
metaDir = buildDir </> "meta"

-- | Meta file path builder.
--
metaFile :: FilePath -> FilePath
metaFile = (metaDir </>)

-- | Location of stack's work files.
--
stackDir :: FilePath
stackDir = ".stack-work"

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

-- | Git version.
--
gitVersion :: Action String
gitVersion = git [ "describe", "--tags", "--abbrev=0" ]

-- | Touch a file for fake files.
--
touchFile :: FilePath -> Action ()
touchFile = flip writeFile' mempty

-- | Use a fake file to keep track of the last time an file-free action ran.
--
fake :: [FilePattern] -> String -> ([FilePath] -> Action ()) -> Rules ()
fake pats target act =
  fakeFile target %> \out -> do
    files <- getDirectoryFiles "." pats
    need files
    act files
    touchFile out

-- | Wraps fake with a phony target.
--
fake' :: [FilePattern] -> String -> ([FilePath] -> Action ()) -> Rules ()
fake' pats target act = do
  fake pats target act

  phony target $
    need [ fakeFile target ]

-- | Use a meta file to keep track of vitual content.
--
meta :: FilePath -> Action String -> Rules ()
meta target act =
  metaFile target %> \out -> do
    alwaysRerun
    content <- act
    writeFileChanged out content

-- | Preprocess a file with m4
--
preprocess :: FilePath -> Action [(String, String)] -> Rules ()
preprocess file macros =
  file %> \out -> do
    let template = file <.> "m4"
    need [ template ]
    let f k v = "-D" <> k <> "=" <> v
    macros' <- macros
    content <- m4 $ template : (uncurry f <$> macros')
    writeFileChanged out content

-- | Haskell source rules
--
hsRules :: Rules ()
hsRules = do
  let pats = [ "//*.hs" ]

  -- | format
  --
  fake' pats "format" $ \files -> do
    need [ ".stylish-haskell.yaml" ]
    stylish $ [ "-c", ".stylish-haskell.yaml", "-i" ] <> files

  -- | lint
  --
  fake' pats "lint" $ \files ->
    lint files

-- | Build-in rules.
--
shakeRules :: Rules ()
shakeRules = do
  -- | clear
  --
  phony "clear" $
    forM_ [ fakeDir, metaDir ] $
      flip removeFilesAfter [ "//*" ]

  -- | clean
  --
  phony "clean" $ do
    stack [ "clean" ]
    removeFilesAfter buildDir [ "//*" ]

  -- | clobber
  --
  phony "clobber" $
    forM_ [ buildDir, stackDir ] $
      flip removeFilesAfter [ "//*" ]

-- | Stack rules.
--
stackRules :: [FilePattern] -> Rules ()
stackRules pats = do
  -- | build
  --
  fake' pats "build" $ const $
    stack [ "build", "--fast" ]

  -- | build-error
  --
  fake' pats "build-error" $ const $
    stack [ "build", "--fast", "--ghc-options=-Werror" ]

  -- | build-tests
  --
  fake' pats "build-tests" $ const $
    stack [ "build", "--fast", "--test", "--no-run-tests" ]

  -- | build-tests-error
  --
  fake' pats "build-tests-error" $ const $
    stack [ "build", "--fast", "--test", "--no-run-tests", "--ghc-options=-Werror" ]

  -- | install
  --
  fake' pats "install" $ const $
    stack [ "build", "--fast", "--copy-bins" ]

  -- | tests
  --
  phony "tests" $
    stack [ "build", "--fast", "--test" ]

  -- | tests-error
  --
  phony "tests-error" $
    stack [ "build", "--fast", "--test", "--ghc-options=-Werror" ]

  -- | ghci
  --
  phony "ghci" $
    stack [ "ghci", "--fast" ]

  -- | ghci-tests
  --
  phony "ghci-tests" $
    stack [ "ghci", "--fast", "--test" ]

-- | Cabal and hackage rules.
--
cabalRules :: FilePath -> Rules ()
cabalRules file = do
  -- | "gitVersion"
  --
  meta "gitVersion"
    gitVersion

  -- | cabal
  --
  preprocess file $ do
    need [ metaFile "gitVersion" ]
    version <- gitVersion
    return [ ("VERSION", version) ]

  -- | publish
  --
  phony "publish" $ do
    need [ file ]
    stack [ "sdist" ]
    stack [ "upload", ".", "--no-signature" ]

-- | Main entry point.
--
shakeMain :: Rules () -> IO ()
shakeMain act = do
  version <- getHashedShakeVersion [ shakeFile ]
  shakeArgs shakeOptions { shakeFiles = buildDir, shakeVersion = version } $ do
    shakeRules
    hsRules
    act
