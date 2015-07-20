#!/usr/bin/env runhaskell
import Development.Shake
import Development.Shake.FilePath
import Data.List

cabalDeploy :: CmdResult b => FilePath -> Action b
cabalDeploy dir = do
  () <- cmd (Cwd dir) "cabal install --dependencies-only -j"
  maybePwd <- getEnv "PWD"
  let pwd = maybe "" id maybePwd
  () <- cmd (Cwd dir) $ "cabal configure --prefix=" ++ (pwd </> "_build")
  () <- cmd (Cwd dir) "cabal build"
  () <- cmd (Cwd dir) "cabal copy"
  cmd (Cwd dir) "cabal configure"

cabalSandbox :: FilePath -> Action ()
cabalSandbox dir = do
  () <- cmd (Cwd dir) "cabal sandbox init"
  (Stdout out) <- cmd (Cwd dir) "cabal sandbox list-sources" :: Action (Stdout String)
  let hasCommon = null $ filter (isInfixOf "Common") $ lines out
  if hasCommon
    then cmd (Cwd dir) "cabal sandbox add-source ../Common"
    else return ()

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["_build/bin/DailyFeeling-backend"]

  ["Backend/cabal.sandbox.config", "Backend/.cabal-sandbox"] &%> \_ -> cabalSandbox "Backend"

  ["Frontend/cabal.sandbox.config", "Frontend/.cabal-sandbox"] &%> \_ -> cabalSandbox "Frontend"

  "_build/bin/DailyFeeling-frontend.jsexe" %> \_ -> do
    need ["Frontend/cabal.sandbox.config", "Frontend/.cabal-sandbox"]
    cabalDeploy "Frontend"

  "_build/bin/DailyFeeling-backend" %> \_ -> do
    need ["Backend/cabal.sandbox.config", "Backend/.cabal-sandbox"
         , "_build/bin/DailyFeeling-frontend.jsexe"]
    () <- cmd (Cwd "Backend") "cp" "-r" "../_build/bin/DailyFeeling-frontend.jsexe"
          "-T" "frontend"
    cabalDeploy "Backend"

  phony "clean" $ do
    () <- cmd (Cwd "Frontend") "cabal clean"
    () <- cmd (Cwd "Backend") "cabal clean"
    removeFilesAfter "_build" ["//*"]

  phony "run" $ do
    need ["_build/bin/DailyFeeling-backend"]
    cmd (Cwd "Backend") "cabal run"
