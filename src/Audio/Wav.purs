module Audio.Wav where

import Prelude

import Control.Monad.Except (ExceptT, except, lift, runExceptT)
import Control.Monad.Loops (iterateUntil)
import Data.ArrayBuffer.DataView.Serialization (Decoder, getASCIIString, getInt32le, getUint16le, getUint32le, getTypedArray, getDataViewWithLength, runDecoder, skipBytes)
import Data.ArrayBuffer.Safe.ArrayBuffer as AB
import Data.ArrayBuffer.Safe.DataView as DV
import Data.ArrayBuffer.Safe.TypedArray as TA
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Record.ShowRecord (showRecord)
import Data.Tuple (Tuple(..), snd)
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

toString :: Wav -> String
toString {metadata, audioData}
  = "WAV (length: " <> show dataLength <>
         ", metadata: " <> showRecord intMetadata <> ")"
  where
  intMetadata = {
      numChannels:   UInt.toInt metadata.numChannels
    , sampleRate:    UInt.toInt metadata.sampleRate
    , byteRate:      UInt.toInt metadata.byteRate
    , blockAlign:    UInt.toInt metadata.blockAlign
    , bitsPerSample: UInt.toInt metadata.bitsPerSample
    }
  dataLength = case audioData of
    PCM8Data  ta -> TA.byteLength ta
    PCM16Data ta -> TA.byteLength ta

decode :: AB.ArrayBuffer -> Either String Wav
decode = runExceptDecoder wavDecoder "WAV parse error" <<< DV.fromArrayBuffer

-- a Decoder monad that throws descriptive error strings
type ExceptDecoder a = ExceptT String Decoder a

runExceptDecoder :: forall a. ExceptDecoder a -> String -> DV.DataView -> Either String a
runExceptDecoder dec errStr =
  maybe (Left errStr) snd <<< runDecoder (runExceptT dec)

type FormatID = String

-- http://soundfile.sapp.org/doc/WaveFormat/
wavDecoder :: ExceptDecoder Wav
wavDecoder = do
  -- https://en.wikipedia.org/wiki/FourCC
  checkStr4 "RIFF" "Invalid WAV file fourCC: "
  lift $ skipBytes 4 -- ChunkSize... unnecessary?
  checkStr4 "WAVE" "Invalid WAV format: "

  metadata <- decodeFmtChunk

  audioDataChunkDataView <-
    snd <$> iterateUntil (\(Tuple fid _) -> fid == "data") (lift decodeChunk)
  audioData <- except $ runExceptDecoder
    (decodeDataChunk metadata.bitsPerSample)
    "Error decoding audio data."
    audioDataChunkDataView

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

    decodeChunk :: Decoder (Tuple FormatID DV.DataView)
    decodeChunk =
      Tuple <$> getASCIIString 4 <*> (getInt32le >>= getDataViewWithLength)

    decodeFmtChunk :: ExceptDecoder WavMetadata
    decodeFmtChunk = do
      Tuple fid dv <- lift decodeChunk
      unless (fid == "fmt ") $
        except <<< Left $ "Invalid WAV subchunk1 id: " <> fid
      unless (DV.byteLength dv == 16) $
        except <<< Left $ "Invalid subchunk1 size: " <> show (DV.byteLength dv)
      except $ runExceptDecoder decodeMetadata "Error decoding metadata." dv
        where
        decodeMetadata = do
          checkEq getu16 (UInt.fromInt 1) \fmt ->
            "Invalid audio format: " <> show fmt

          { numChannels:   _
          , sampleRate:    _
          , byteRate:      _
          , blockAlign:    _
          , bitsPerSample: _
          } <$> getu16 <*> getu32 <*> getu32 <*> getu16 <*> getu16

    decodeDataChunk :: UInt -> ExceptDecoder WavAudioData
    decodeDataChunk bitsPerSample =
      case UInt.toInt bitsPerSample of
        8  -> lift $ PCM8Data  <$> getTypedArray
        16 -> lift $ PCM16Data <$> getTypedArray
        n  -> except <<< Left $ "Unsupported bits per sample: " <> show n
