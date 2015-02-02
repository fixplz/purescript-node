module Node.Stream where

foreign import data Streaming :: !

foreign import data EndEv :: *
foreign import data ReadableEv :: *
foreign import data DataEv :: *
foreign import data DrainEv :: *
foreign import endEv "var endEv = 'end'" :: EndEv
foreign import readableEv "var readableEv = 'readable'" :: ReadableEv
foreign import dataEv "var dataEv = 'data'" :: DataEv
foreign import drainEv "var drainEv = 'drain'" :: DrainEv


module Node.Stream.Char where

import Data.Null
import Data.Object
import Data.Function.Eff
import Control.Monad.Eff
import Node.Stream
import Node.Events

type ReadSize = Number

class (EventEmitter (forall chars. stream chars)) <= ReadStreamChar stream where
  streamPipe :: forall eff chars dest. (WriteStreamChar dest) =>
    stream chars -> dest chars -> Eff (streaming :: Streaming | eff) Unit
  streamRead :: forall eff chars.
    stream chars -> N ReadSize -> Eff (streaming :: Streaming | eff) (N chars)
  streamPause :: forall eff chars.
    stream chars -> Eff (streaming :: Streaming | eff) Unit
  streamResume :: forall eff chars.
    stream chars -> Eff (streaming :: Streaming | eff) Unit

class (EventEmitter (stream chars)) <= WriteStreamChar stream where
  streamWrite :: forall eff chars.
    stream chars -> chars -> Eff (streaming :: Streaming | eff) Boolean
  streamEnd :: forall eff chars.
    stream chars -> Eff (streaming :: Streaming | eff) Unit

class (ReadStreamChar stream, WriteStreamChar stream) <= StreamDuplex stream

instance readStreamCharEndEv :: (ReadStreamChar stream) => Event (stream chars) EndEv Unit
instance readStreamCharReadableEv :: (ReadStreamChar stream) => Event (stream chars) ReadableEv Unit
instance readStreamCharDataEv :: (ReadStreamChar stream) => Event (stream chars) DataEv chars
instance writeStreamCharDrainEv :: (WriteStreamChar stream) => Event (stream chars) DrainEv Unit

{-
class NativeReadStreamChar (stream :: * -> *)

instance nativeReadStreamChar :: (NativeReadStreamChar stream) => ReadStreamChar stream where
  streamPipe = runMethod2 "pipe"
  streamRead = runMethod2 "read"
  streamPause = runMethod0 "pause"
  streamResume = runMethod0 "resume"
-}

foreign import data Thing :: * -> *

instance thingEv :: NativeEventEmitter (Thing val)
instance thingStream :: WriteStreamChar Thing where
  streamWrite = runMethod1 "write"
  streamEnd = runMethod0 "end"

{-class (EventEmitter (forall val. stream val)) <= NativeWriteStreamChar (stream :: * -> *)

instance nativeWriteStreamChar
  :: (NativeWriteStreamChar stream, EventEmitter (stream val))
    => WriteStreamChar stream where
  streamWrite = runMethod1 "write"
  streamEnd = runMethod0 "end"  -}

-- 

module Node.Stream.Object where

import Data.Null
import Data.Object
import Data.Function.Eff
import Control.Monad.Eff
import Node.Stream
import Node.Events

class (EventEmitter (forall val. stream val)) <= ReadStream stream where
  streamPipe :: forall eff val dest. (WriteStream dest) =>
    dest val -> Eff (streaming :: Streaming | eff) Unit
  streamRead :: forall eff val.
    stream val -> Eff (streaming :: Streaming | eff) (N val)
  streamPause :: forall eff val.
    stream val -> Eff (streaming :: Streaming | eff) Unit
  streamResume :: forall eff val.
    stream val -> Eff (streaming :: Streaming | eff) Unit

class (EventEmitter (forall val. stream val)) <= WriteStream stream where
  streamWrite :: forall eff val.
    stream val -> val -> Eff (streaming :: Streaming | eff) Boolean
  streamEnd :: forall eff val.
    stream val -> Eff (streaming :: Streaming | eff) Unit

class (ReadStream stream, WriteStream stream) <= StreamThrough stream

instance readStreamEndEv :: (ReadStream stream) => Event (stream val) EndEv Unit
instance readStreamReadableEv :: (ReadStream stream) => Event (stream val) ReadableEv Unit
instance readStreamDataEv :: (ReadStream stream) => Event (stream val) DataEv val
instance writeStreamDrainEv :: (WriteStream stream) => Event (stream val) DrainEv Unit

{-
class NativeReadStream stream

instance nativeReadStream :: (NativeReadStream stream) => ReadStream stream where
  streamPipe = runMethod2 "pipe"
  streamRead = runMethod1 "read"
  streamPause = runMethod0 "pause"
  streamResume = runMethod0 "resume"

class NativeWriteStream stream

instance nativeWriteStream :: (NativeWriteStream stream) => WriteStream stream where
  streamWrite = runMethod2 "write"
  streamEnd = runMethod0 "end"
-}
