{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake helpers.
--
module Development.Shakers
  ( module Exports
  , (<:>)
  , fakeFile
  , metaFile
  , mirrorDir
  , parentDir
  , getVar
  , getFlag
  , cmdArgs
  , cmdArgs_
  , cmdArgsDir
  , cmdArgsDir_
  , stack
  , stack_
  , stackExec
  , stackExec_
  , git
  , git_
  , gitDir
  , gitDir_
  , schemaApply_
  , m4
  , aws
  , rsync_
  , ssh
  , ssh_
  , sshDir
  , sshDir_
  , rmirror_
  , rssh
  , rssh_
  , rdocker_
  , docker_
  , convox_
  , fake
  , fake'
  , meta
  , preprocess
  , mirror
  , getHashedVersion
  , stackRules
  , cabalRules
  , dbRules
  , dockerRules
  , shakeMain
  ) where

import BasicPrelude               as Exports hiding ((*>))
import Data.Char
import Development.Shake          as Exports
import Development.Shake.FilePath
import System.Directory

-- | Join strings with ":"
--
(<:>) :: (IsString m, Monoid m) => m -> m -> m
(<:>) = (<>) . (<> ":")

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

-- | Parent directory.
--
parentDir :: Action FilePath
parentDir = liftIO $ takeFileName <$> getCurrentDirectory

-- | Mirror directory of current parent directory.
--
mirrorDir :: Action FilePath
mirrorDir = (buildDir </>) <$> parentDir

-- | Wrapper around getting the environment that throws error.
--
getVar :: String -> Action String
getVar k = getEnv k >>= maybe (liftIO $ throwIO $ userError $ "No env: " <> k) return

-- | Wrapper round getting the environment that returns a bool if env is present.
--
getFlag :: String -> Action Bool
getFlag k = isJust <$> getEnv k

-- | Remove host env.
--
remoteVar :: Action String
remoteVar = getVar "REMOTE"

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

-- | Run commands in a dir with return string.
--
cmdArgsDir :: FilePath -> String -> [String] -> Action String
cmdArgsDir d c as = rstrip . fromStdout <$> cmd (Cwd d) c as

-- | Run commands in a dir with no return.
--
cmdArgsDir_ :: FilePath -> String -> [String] -> Action ()
cmdArgsDir_ d c as = unit $ cmd (Cwd d) c as

-- | Stack command without return.
--
stack :: [String] -> Action String
stack = cmdArgs "stack"

-- | Stack command without return.
--
stack_ :: [String] -> Action ()
stack_ = cmdArgs_ "stack"

-- | Stack exec command.
--
stackExec :: String -> [String] -> Action String
stackExec cmd' as = stack $ "exec" : cmd' : "--" : as

-- | Stack exec command without return.
--
stackExec_ :: String -> [String] -> Action ()
stackExec_ cmd' as = stack_ $ "exec" : cmd' : "--" : as

-- | Sylish command.
--
stylish_ :: [String] -> Action ()
stylish_ = cmdArgs_ "stylish-haskell"

-- | Lint command.
--
lint_ :: [String] -> Action ()
lint_ = cmdArgs_ "hlint"

-- | Git command.
--
git :: [String] -> Action String
git = cmdArgs "git"

-- | Git command without return.
--
git_ :: [String] -> Action ()
git_ = cmdArgs_ "git"

-- | Git command in a directory.
--
gitDir :: FilePath -> [String] -> Action String
gitDir d = cmdArgsDir d "git"

-- | Git command in a directory with no return.
--
gitDir_ :: FilePath -> [String] -> Action ()
gitDir_ d = cmdArgsDir_ d "git"

-- | Schema apply command.
--
schemaApply_ :: [String] -> Action ()
schemaApply_ = cmdArgs_ "schema-apply"

-- | m4 command.
--
m4 :: [String] -> Action String
m4 = cmdArgs "m4"

-- | AWS command.
--
aws :: [String] -> Action String
aws = cmdArgs "aws"

-- | Rsync command.
--
rsync_ :: [String] -> Action ()
rsync_ = cmdArgs_ "rsync"

-- | SSH command.
--
ssh :: String -> [String] -> Action String
ssh host as = cmdArgs "ssh" $ host : as

-- | SSH command with no return.
--
ssh_ :: String -> [String] -> Action ()
ssh_ host as = cmdArgs_ "ssh" $ host : as

-- | SSH command in a remote directory.
--
sshDir :: String -> FilePath -> [String] -> Action String
sshDir host dir as = ssh host $ "cd" : dir : "&&" : as

-- | SSH command in a remote directory with no return.
--
sshDir_ :: String -> FilePath -> [String] -> Action ()
sshDir_ host dir as = ssh_ host $ "cd" : dir : "&&" : as

-- | Mirror directory remotely.
--
rmirror_ :: Action ()
rmirror_ = do
  r <- remoteVar
  p <- parentDir
  rsync_ [ "-Laz", "--delete", buildDir </> p <> "/", r <:> p <> "/" ]

-- | Remote SSH command.
--
rssh :: [String] -> Action String
rssh as = do
  r <- remoteVar
  p <- parentDir
  sshDir r p as

-- | Remote SSH command.
--
rssh_ :: [String] -> Action ()
rssh_ as = do
  r <- remoteVar
  p <- parentDir
  sshDir_ r p as

-- | Run docker command remotely.
--
rdocker_ :: [String] -> Action ()
rdocker_ = rssh_ . ("docker" :)

-- | Run docker command in mirro dir.
--
docker_ :: [String] -> Action ()
docker_ as = do
  d <- mirrorDir
  cmdArgsDir_ d "docker" as

-- | Run convox command in mirro dir.
--
convox_ :: [String] -> Action ()
convox_ as = do
  d <- mirrorDir
  cmdArgsDir_ d "convox" as

-- | Git version.
--
gitVersion :: Action String
gitVersion = git [ "describe", "--tags", "--abbrev=0" ]

-- | Use a fake file to keep track of the last time an file-free action ran.
--
fake :: [FilePattern] -> String -> ([FilePath] -> Action ()) -> Rules ()
fake pats target act =
  fakeFile target %> \out -> do
    files <- getDirectoryFiles "." pats
    act files
    writeFile' out mempty

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

-- | Preprocess a file with m4.
--
preprocess :: FilePattern -> FilePath -> Action [(String, String)] -> Rules ()
preprocess target file macros =
  target %> \out -> do
    need [ file ]
    let f k v = "-D" <> k <> "=" <> v
    macros' <- macros
    content <- m4 $ file : (uncurry f <$> macros')
    writeFileChanged out content

-- | Mirror files to the mirror directory.
--
mirror :: [FilePattern] -> Rules ()
mirror pats =
  fake' pats "mirror" $ mapM_ $ \file -> do
    dir <- mirrorDir
    liftIO $ createDirectoryIfMissing True $ dropFileName (dir </> file)
    copyFileChanged file (dir </> file)

-- | Build a hash version from a directory and file pattern.
--
getHashedVersion :: FilePath -> [FilePattern] -> Action String
getHashedVersion dir pats = do
  files <- getDirectoryFiles dir pats
  liftIO $ getHashedShakeVersion $ (dir </>) <$> files

-- | Haskell source rules
--
hsRules :: Rules ()
hsRules = do
  let pats = [ "//*.hs" ]

  -- | format
  --
  fake' pats "format" $ \files -> do
    need [ ".stylish-haskell.yaml" ]
    stylish_ $ [ "-c", ".stylish-haskell.yaml", "-i" ] <> files

  -- | lint
  --
  fake' pats "lint" $ \files ->
    lint_ files

-- | Built-in rules.
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
    stack_ [ "clean" ]
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
    stack_ [ "build", "--fast" ]

  -- | build-error
  --
  fake' pats "build-error" $ const $
    stack_ [ "build", "--fast", "--ghc-options=-Werror" ]

  -- | build-tests
  --
  fake' pats "build-tests" $ const $
    stack_ [ "build", "--fast", "--test", "--no-run-tests" ]

  -- | build-tests-error
  --
  fake' pats "build-tests-error" $ const $
    stack_ [ "build", "--fast", "--test", "--no-run-tests", "--ghc-options=-Werror" ]

  -- | install
  --
  fake' pats "install" $ const $
    stack_ [ "build", "--fast", "--copy-bins" ]

  -- | tests
  --
  phony "tests" $
    stack_ [ "build", "--fast", "--test" ]

  -- | tests-error
  --
  phony "tests-error" $
    stack_ [ "build", "--fast", "--test", "--ghc-options=-Werror" ]

  -- | ghci
  --
  phony "ghci" $
    stack_ [ "ghci", "--fast" ]

  -- | ghci-tests
  --
  phony "ghci-tests" $
    stack_ [ "ghci", "--fast", "--test" ]

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
  preprocess file (file <.> "m4") $ do
    need [ metaFile "gitVersion" ]
    version <- gitVersion
    return [ ("VERSION", version) ]

  -- | publish
  --
  phony "publish" $ do
    need [ file ]
    stack_ [ "sdist" ]
    stack_ [ "upload", ".", "--no-signature" ]

-- | Database rules
--
dbRules :: Rules ()
dbRules =
  -- | schema:apply
  --
  phony "schema:apply" $
    schemaApply_ [ "--dir", "schema/migrations" ]

-- | Docker rules.
--
dockerRules :: [FilePattern] -> Rules ()
dockerRules pats = do
  -- | mirror
  --
  mirror pats

  -- | docker:login
  --
  phony "docker:login" $ do
    login <- aws [ "ecr", "get-login", "--region", "us-west-2" ]
    unit $ cmd login

  -- | docker:login-remote
  --
  phony "docker:login-remote" $ do
    login <- aws [ "ecr", "get-login", "--region", "us-west-2" ]
    rssh_ [ login ]

-- | Main entry point.
--
shakeMain :: Rules () -> IO ()
shakeMain act = do
  version <- getHashedShakeVersion [ shakeFile ]
  shakeArgs shakeOptions { shakeFiles = buildDir, shakeVersion = version, shakeThreads = 0 } $ do
    shakeRules
    hsRules
    act
