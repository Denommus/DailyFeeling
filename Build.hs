#!/usr/bin/env runhaskell
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["_build/DailyFeeling-backend" <.> exe]

  ["Backend/cabal.sandbox.config", "Backend/.cabal-sandbox"] &%> \_ -> do
    cmd (Cwd "Backend") "cabal sandbox init"

  ["Frontend/cabal.sandbox.config", "Frontend/.cabal-sandbox"] &%> \_ -> do
    cmd (Cwd "Frontend") "cabal sandbox init"

  "_build/DailyFeeling-frontend.jsexe" %> \out -> do
    need ["Frontend/cabal.sandbox.config", "Frontend/.cabal-sandbox"]
    () <- cmd (Cwd "Frontend") "cabal install --dependencies-only -j"
    () <- cmd (Cwd "Frontend") "cabal build"
    cmd "cp" "-r"
      "Frontend/dist/build/DailyFeeling-frontend/DailyFeeling-frontend.jsexe"
      out

  "_build/DailyFeeling-backend" <.> exe %> \out -> do
    need ["Backend/cabal.sandbox.config", "Backend/.cabal-sandbox"
         , "_build/DailyFeeling-frontend.jsexe"]
    () <- cmd (Cwd "Backend") "cabal install --dependencies-only -j"
    () <- cmd (Cwd "Backend") "cabal build"
    copyFile'
      ("Backend/dist/build/DailyFeeling-backend/DailyFeeling-backend" <.> exe)
      out
