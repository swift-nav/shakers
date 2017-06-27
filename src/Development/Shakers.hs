{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake helpers.
--
module Development.Shakers
  ( module Exports
  , (<:>)
  , timestamp
  , buildFile
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
  , schemaApply_
  , m4
  , aws
  , rsync_
  , ssh
  , ssh_
  , sshScreen
  , sshScreen_
  , sshDir
  , sshDir_
  , sshScreenDir
  , sshScreenDir_
  , rmirror_
  , rssh
  , rssh_
  , rdocker_
  , docker_
  , convox_
  , fake
  , meta
  , preprocess
  , getHashedVersion
  , hsRules
  , stackRules
  , stackTargetRules
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

-- | Unix timestamp.
--
timestamp :: Action String
timestamp = cmdArgs "date" [ "+%s" ]

-- | File used for version change detection.
--
shakeFile :: FilePath
shakeFile = "Shakefile.hs"

-- | Location of build supporting files.
--
buildDir :: FilePath
buildDir = ".build"

-- | Build file path builder.
--
buildFile :: FilePath -> FilePath
buildFile = (buildDir </>)

-- | Build directory where "touch" files are kept.
--
fakeDir :: FilePath
fakeDir = buildFile "fake"

-- | Fake file path builder.
--
fakeFile :: FilePath -> FilePath
fakeFile = (fakeDir </>)

-- | Meta directory where "virtual" files are kept.
--
metaDir :: FilePath
metaDir = buildFile "meta"

-- | Meta file path builder.
--
metaFile :: FilePath -> FilePath
metaFile = (metaDir </>)

-- | Parent directory.
--
parentDir :: Action FilePath
parentDir = liftIO $ takeFileName <$> getCurrentDirectory

-- | Mirror directory of current parent directory.
--
mirrorDir :: Action FilePath
mirrorDir = buildFile <$> parentDir

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
stack :: FilePath -> [String] -> Action String
stack d = cmdArgsDir d "stack"

-- | Stack command without return.
--
stack_ :: FilePath -> [String] -> Action ()
stack_ d = cmdArgsDir_ d "stack"

-- | Stack exec command.
--
stackExec :: FilePath -> String -> [String] -> Action String
stackExec d cmd' as = stack d $ "exec" : cmd' : "--" : as

-- | Stack exec command without return.
--
stackExec_ :: FilePath -> String -> [String] -> Action ()
stackExec_ d cmd' as = stack_ d $ "exec" : cmd' : "--" : as

-- | Sylish command.
--
stylish_ :: [String] -> Action ()
stylish_ = cmdArgs_ "stylish-haskell"

-- | Lint command.
--
lint_ :: [String] -> Action ()
lint_ = cmdArgs_ "hlint"

-- | Git command in a directory.
--
git :: FilePath -> [String] -> Action String
git d = cmdArgsDir d "git"

-- | Git command in a directory with no return.
--
git_ :: FilePath -> [String] -> Action ()
git_ d = cmdArgsDir_ d "git"

-- | Schema apply command.
--
schemaApply_ :: FilePath -> [String] -> Action ()
schemaApply_ d = cmdArgsDir_ d "schema-apply"

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
ssh h as = cmdArgs "ssh" $ h : as

-- | SSH command with no return.
--
ssh_ :: String -> [String] -> Action ()
ssh_ h as = cmdArgs_ "ssh" $ h : as

-- | SSH command with screen.
--
sshScreen :: String -> [String] -> Action String
sshScreen h as = do
  t <- timestamp
  ssh h $ "-t" : "screen" : "-S" : t : as

-- | SSH command with screen no return.
--
sshScreen_ :: String -> [String] -> Action ()
sshScreen_ h as = do
  t <- timestamp
  ssh_ h $ "-t" : "screen" : "-S" : t : as

-- | SSH command in a remote directory.
--
sshDir :: String -> FilePath -> [String] -> Action String
sshDir h d as = ssh h $ "cd" : d : "&&" : as

-- | SSH command in a remote directory with no return.
--
sshDir_ :: String -> FilePath -> [String] -> Action ()
sshDir_ h d as = ssh_ h $ "cd" : d : "&&" : as

-- | SSH command with screen in a remote directory.
--
sshScreenDir :: String -> FilePath -> [String] -> Action String
sshScreenDir h d as = sshScreen h $ "cd" : d : "&&" : as

-- | SSH command with screen in a remote directory with no return.
--
sshScreenDir_ :: String -> FilePath -> [String] -> Action ()
sshScreenDir_ h d as = sshScreen_ h $ "cd" : d : "&&" : as

-- | Mirror directory remotely.
--
rmirror_ :: Action ()
rmirror_ = do
  r <- remoteVar
  p <- parentDir
  rsync_ [ "-Laz", "--delete", buildFile p <> "/", r <:> p <> "/" ]

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
gitVersion :: FilePath -> Action String
gitVersion d = git d [ "describe", "--tags", "--abbrev=0" ]

-- | Use a fake file to keep track of the last time an file-free action ran.
--
fake :: FilePath -> [FilePattern] -> String -> ([FilePath] -> Action ()) -> Rules ()
fake dir pats target act = do
  meta target $
    getDirectoryFiles dir pats >>=
      liftIO . getHashedShakeVersion

  fakeFile target %> \out -> do
    need [ metaFile target ]
    getDirectoryFiles dir pats >>=
      act
    writeFile' out mempty

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
    alwaysRerun
    let f k v = "-D" <> k <> "=" <> v
    macros' <- macros
    content <- m4 $ file : (uncurry f <$> macros')
    writeFileChanged out content

-- | Build a hash version from a directory and file pattern.
--
getHashedVersion :: FilePath -> [FilePattern] -> Action String
getHashedVersion dir pats = do
  files <- getDirectoryFiles dir pats
  liftIO $ getHashedShakeVersion $ (dir </>) <$> files

-- | Built-in rules.
--
shakeRules :: Rules ()
shakeRules =
  -- | clear
  --
  phony "clear" $
    removeFilesAfter buildDir [ "//*" ]

-- | Haskell source rules
--
hsRules :: FilePath -> Rules ()
hsRules dir = do
  let pats = [ "//*.hs" ]

  -- | format
  --
  fake dir pats "format" $ \files -> do
    need [ ".stylish-haskell.yaml" ]
    stylish_ $ [ "-c", ".stylish-haskell.yaml", "-i" ] <> files

  -- | lint
  --
  fake dir pats "lint" $ \files ->
    lint_ files

-- | Stack rules.
--
stackRules :: FilePath -> [FilePattern] -> Rules ()
stackRules dir pats = do
  -- | build
  --
  fake dir pats "build" $ const $
    stack_ dir [ "build", "--fast" ]

  -- | build-error
  --
  fake dir pats "build-error" $ const $
    stack_ dir [ "build", "--fast", "--ghc-options=-Werror" ]

  -- | build-tests
  --
  fake dir pats "build-tests" $ const $
    stack_ dir [ "build", "--fast", "--test", "--no-run-tests" ]

  -- | build-tests-error
  --
  fake dir pats "build-tests-error" $ const $
    stack_ dir [ "build", "--fast", "--test", "--no-run-tests", "--ghc-options=-Werror" ]

  -- | install
  --
  fake dir pats "install" $ const $
    stack_ dir [ "build", "--fast", "--copy-bins" ]

  -- | tests
  --
  phony "tests" $
    stack_ dir [ "build", "--fast", "--test" ]

  -- | tests-error
  --
  phony "tests-error" $
    stack_ dir [ "build", "--fast", "--test", "--ghc-options=-Werror" ]

  -- | ghci
  --
  phony "ghci" $
    stack_ dir [ "ghci", "--fast" ]

  -- | ghci-tests
  --
  phony "ghci-tests" $
    stack_ dir [ "ghci", "--fast", "--test" ]

  -- | clean
  --
  phony "clean" $ do
    need [ "clear" ]
    stack_ dir [ "clean" ]

  -- | clobber
  --
  phony "clobber" $ do
    need [ "clear" ]
    removeFilesAfter dir [ "//*.stack-work" ]

-- | Stack rules.
--
stackTargetRules :: FilePath -> String -> [FilePattern] -> Rules ()
stackTargetRules dir target pats = do
  -- | build
  --
  fake dir pats ("build:" <> target) $ const $
    stack_ dir [ "build", target, "--fast" ]

  -- | build-error
  --
  fake dir pats ("build-error:" <> target) $ const $
    stack_ dir [ "build", target, "--fast", "--ghc-options=-Werror" ]

  -- | build-tests
  --
  fake dir pats ("build-tests:" <> target) $ const $
    stack_ dir [ "build", target, "--fast", "--test", "--no-run-tests" ]

  -- | build-tests-error
  --
  fake dir pats ("build-tests-error:" <> target) $ const $
    stack_ dir [ "build", target, "--fast", "--test", "--no-run-tests", "--ghc-options=-Werror" ]

  -- | install
  --
  fake dir pats ("install:" <> target) $ const $
    stack_ dir [ "build", target, "--fast", "--copy-bins" ]

  -- | tests
  --
  phony ("tests:" <> target) $
    stack_ dir [ "build", target, "--fast", "--test" ]

  -- | tests-error
  --
  phony ("tests-error:" <> target) $
    stack_ dir [ "build", target, "--fast", "--test", "--ghc-options=-Werror" ]

  -- | ghci
  --
  phony ("ghci:" <> target) $
    stack_ dir [ "ghci", target, "--fast" ]

  -- | ghci-tests
  --
  phony ("ghci-tests:" <> target) $
    stack_ dir [ "ghci", target, "--fast", "--test" ]

-- | Cabal and hackage rules.
--
cabalRules :: FilePath -> FilePath -> Rules ()
cabalRules dir file = do
  -- | "gitVersion"
  --
  meta "cabalVersion" $ gitVersion dir

  -- | cabal
  --
  preprocess file (file <.> "m4") $ do
    need [ metaFile "cabalVersion" ]
    version <- gitVersion dir
    return [ ("VERSION", version) ]

  -- | publish
  --
  phony "publish" $ do
    need [ file ]
    stack_ dir [ "sdist", dir ]
    stack_ dir [ "upload", dir, "--no-signature" ]

-- | Database rules
--
dbRules :: FilePath -> Rules ()
dbRules dir =
  -- | schema:apply
  --
  phony "schema:apply" $
    schemaApply_ dir [ "--dir", "schema/migrations" ]

-- | Docker rules.
--
dockerRules :: FilePath -> [FilePattern] -> Rules ()
dockerRules dir pats = do
  -- | mirror
  --
  phony "mirror" $ do
    dir' <- mirrorDir
    liftIO $ removeFiles dir' [ "//*" ]
    files <- getDirectoryFiles dir pats
    forM_ files $ \file ->
      liftIO $ do
        createDirectoryIfMissing True $ dropFileName (dir' </> file)
        copyFile file (dir' </> file)

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
    act
