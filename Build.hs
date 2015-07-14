#!/usr/bin/env runhaskell
import Development.Shake

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
  want ["_build/DailyFeeling-backend"]

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
      "-T" out

  "_build/DailyFeeling-backend" %> \out -> do
    need ["Backend/cabal.sandbox.config", "Backend/.cabal-sandbox"
         , "_build/DailyFeeling-frontend.jsexe"]
    () <- cmd (Cwd "Backend") "cp" "-r" "../_build/DailyFeeling-frontend.jsexe"
          "-T" "frontend"
    () <- cmd (Cwd "Backend") "cabal install --dependencies-only -j"
    () <- cmd (Cwd "Backend") "cabal build"
    cmd "cp" "-r"
      "Backend/dist/build/DailyFeeling-backend"
      "-T" out

  phony "clean" $ do
    () <- cmd (Cwd "Frontend") "cabal clean"
    () <- cmd (Cwd "Backend") "cabal clean"
    removeFilesAfter "_build" ["//*"]

  phony "run" $ do
    need ["_build/DailyFeeling-backend"]
    cmd (Cwd "Backend") "cabal run"
