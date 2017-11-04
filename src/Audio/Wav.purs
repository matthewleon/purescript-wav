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

-- TODO: parameterize datatype of channelData
type Wav =
  { metadata    :: WavMetadata
  , channelData :: Array TA.Float32Array
  }

-- TODO: user UInt
type WavMetadata =
  { numChannels   :: Int
  , sampleRate    :: Int
  , byteRate      :: Int
  , blockAlign    :: Int
  , bitsPerSample :: Int
  }

decode :: AB.ArrayBuffer -> Either String Wav
decode ab =
  let dv = DV.fromArrayBuffer ab
  in  case runDecoder (runExceptT wavDecoder) dv 0 of
      Just (Tuple _ (Right _)) -> Left "TODO"
      Just (Tuple _ l)         -> l
      Nothing                  -> Left "WAV parse error"

-- http://soundfile.sapp.org/doc/WaveFormat/
wavDecoder :: ExceptT String Decoder Wav
wavDecoder = do
  -- https://en.wikipedia.org/wiki/FourCC
  checkStr4 "RIFF" "Invalid WAV file fourCC: "
  lift $ skipBytes 4 -- ChunkSize... unnecessary?
  checkStr4 "WAVE" "Invalid WAV format: "
  metadata <- decodeSubchunk1
  pure {metadata: metadata, channelData: []}

  where
    getu16  = lift getUint16le
    getu32  = lift getUint32le
    getStr4 = lift $ getASCIIString 4

    checkEq
      :: forall a
       . Eq a
      => ExceptT String Decoder a
      -> a
      -> (a -> String)
      -> ExceptT String Decoder Unit
    checkEq getter shouldEq mkErr = do
      got <- getter
      unless (got == shouldEq) $ except <<< Left $ mkErr got

    checkStr4 shouldEq errStr = checkEq getStr4 shouldEq (errStr <> _)

    decodeSubchunk1 = do
      checkStr4 "fmt " "Invalid WAV subchunk1 id: "
      checkEq getu32 16 \size -> "Invalid subchunk1 size: " <> show size
      checkEq getu16 1 \fmt -> "Invalid audio format: " <> show fmt

      numChannels   <- getu16
      sampleRate    <- getu32
      byteRate      <- getu32
      blockAlign    <- getu16
      bitsPerSample <- getu16

      pure {
        numChannels:   numChannels
      , sampleRate:    sampleRate
      , byteRate:      byteRate
      , blockAlign:    blockAlign
      , bitsPerSample: bitsPerSample
      }

