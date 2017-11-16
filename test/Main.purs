module Test.Main where

import Prelude

import Audio.Wav as Wav
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.Buffer (BUFFER, toArrayBuffer)
import Node.FS (FS)
import Node.FS.Sync (readFile)

main :: forall e. Eff (buffer :: BUFFER, fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  gongBuffer <- readFile "test/gong.wav"
  gongAB <- toArrayBuffer gongBuffer
  let gongWav = Wav.decode gongAB
  logShow $ Wav.toString <$> gongWav
