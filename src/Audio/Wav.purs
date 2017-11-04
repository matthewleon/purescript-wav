module Audio.Wav where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView.Serialization (Decoder, getASCIIString, getUint16le, getUint32le, runDecoder, skipBytes)
import Data.ArrayBuffer.TypedArray as TA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Wav =
  { sampleRate  :: Int
  , channelData :: Array TA.Float32Array
  }

decode :: AB.ArrayBuffer -> Either String Wav
decode ab =
  let dv = DV.fromArrayBuffer ab
  in  case runDecoder (runExceptT wavDecoder) dv 0 of
      Just (Tuple _ (Right _)) -> Right {sampleRate: 0, channelData: []}
      Just (Tuple _ l)         -> l
      Nothing                  -> Left "WAV parse error"

-- http://soundfile.sapp.org/doc/WaveFormat/
wavDecoder :: ExceptT String Decoder Wav
wavDecoder =
  let throw  = except <<< Left
      get16  = lift getUint16le
      get32  = lift getUint32le
      getStr = lift $ getASCIIString 4
  in do
    -- https://en.wikipedia.org/wiki/FourCC
    fourCC <- getStr
    unless (fourCC == "RIFF") $
      throw ("Invalid WAV file fourCC: " <> fourCC)

    lift $ skipBytes 4 -- ChunkSize... unnecessary?

    format <- getStr
    unless (format == "WAVE") $
      throw ("Invalid WAV format: " <> format)

    subChunk1Id <- getStr
    unless (subChunk1Id == "fmt ") $
      throw ("Invalid WAV subchunk1 id: " <> subChunk1Id)
    
    subChunk1Size <- get32
    unless (subChunk1Size == 16) $
      throw ("Invalid subchunk1 size: " <> show subChunk1Size)

    audioFormat <- get16
    unless (audioFormat == 1) $
      throw ("Invalid audio format (only PCM supported): " <> show audioFormat)

    numChannels   <- get16
    sampleRate    <- get32
    byteRate      <- get32
    blockAlign    <- get16
    bitsPerSample <- get16

    pure {sampleRate: sampleRate, channelData: []}
