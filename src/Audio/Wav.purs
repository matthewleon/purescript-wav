module Audio.Wav where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView.Serialization (Decoder, getASCIIString, getInt32le, getUint16le, getUint32le, getTypedArrayWithLength, runDecoder, skipBytes)
import Data.ArrayBuffer.TypedArray as TA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Data.UInt as UInt

type Wav =
  { metadata    :: WavMetadata
  , audioData   :: WavAudioData
  }

type WavMetadata =
  { numChannels   :: UInt
  , sampleRate    :: UInt
  , byteRate      :: UInt
  , blockAlign    :: UInt
  , bitsPerSample :: UInt
  }

data WavAudioData =
  PCM8Data  TA.Uint8Array
| PCM16Data TA.Int16Array

decode :: AB.ArrayBuffer -> Either String Wav
decode ab =
  let dv = DV.fromArrayBuffer ab
  in  case runDecoder (runExceptT wavDecoder) dv 0 of
      Just (Tuple _ (Right _)) -> Left "TODO"
      Just (Tuple _ l)         -> l
      Nothing                  -> Left "WAV parse error"

type ExceptDecoder a = ExceptT String Decoder a

-- http://soundfile.sapp.org/doc/WaveFormat/
wavDecoder :: ExceptDecoder Wav
wavDecoder = do
  -- https://en.wikipedia.org/wiki/FourCC
  checkStr4 "RIFF" "Invalid WAV file fourCC: "
  lift $ skipBytes 4 -- ChunkSize... unnecessary?
  checkStr4 "WAVE" "Invalid WAV format: "
  metadata <- decodeSubchunk1
  audioData <- decodeSubchunk2 metadata.bitsPerSample
  pure {metadata: metadata, audioData: audioData}

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

    decodeSubchunk1 :: ExceptDecoder WavMetadata
    decodeSubchunk1 = do
      checkStr4 "fmt " "Invalid WAV subchunk1 id: "
      checkEq getu32 (UInt.fromInt 16) \size ->
        "Invalid subchunk1 size: " <> show size
      checkEq getu16 (UInt.fromInt 1) \fmt ->
        "Invalid audio format: " <> show fmt

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

    decodeSubchunk2 :: UInt -> ExceptDecoder WavAudioData
    decodeSubchunk2 bitsPerSample = do
      checkStr4 "data" "Invalid subchunk2 data tag: "
      -- technically we should get a uint, but length param below is Int
      size <- lift getInt32le
      case UInt.toInt bitsPerSample of
        8  -> lift $ PCM8Data  <$> getTypedArrayWithLength size
        16 -> lift $ PCM16Data <$> getTypedArrayWithLength size
        n  -> except <<< Left $ "Unsupported bits per sample: " <> show n
