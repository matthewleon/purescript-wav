module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Node.FS.Sync (readFile)

import Audio.Wav (decode)

main :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  gongBuffer <- readFile "test/gong.wav"
  gongWav     = decode gongBuffer
