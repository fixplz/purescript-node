module Node.Stream where

foreign import data Streaming :: !

--

module Node.Stream.Char where

import Data.Null
import Data.Object
import Data.Function.Eff
import Control.Monad.Eff
import Node.Stream
import Node.Events

type ReadSize = Number

class ReadStreamChar stream chars
instance readStreamEv :: (ReadStreamChar stream) => EventEmitter stream

class WriteStreamChar stream chars
instance writeStreamEv :: (WriteStreamChar stream) => EventEmitter stream

class (ReadStreamChar stream chars, WriteStreamChar stream chars) <= DuplexStream stream chars

streamPipe ::
  forall eff stream dest chars. (ReadStreamChar stream chars, WriteStreamChar dest chars) =>
    stream -> dest -> Eff (streaming :: Streaming | eff) Unit

streamPipe = runMethodEff1 "pipe"

streamRead ::
  forall eff stream chars. (ReadStreamChar stream chars) =>
    stream -> N ReadSize -> Eff (streaming :: Streaming | eff) (N chars)

streamRead = runMethodEff1 "read"

streamPause ::
  forall eff stream chars. (ReadStreamChar stream chars) =>
    stream -> Eff (streaming :: Streaming | eff) Unit

streamPause = runMethodEff0 "pause"

streamResume ::
  forall eff stream chars. (ReadStreamChar stream chars) =>
    stream -> Eff (streaming :: Streaming | eff) Unit

streamResume = runMethodEff0 "resume"

streamWrite ::
  forall eff stream chars. (WriteStreamChar stream chars) =>
    stream -> chars -> Eff (streaming :: Streaming | eff) Boolean

streamWrite = runMethodEff1 "write"

streamEnd ::
  forall eff stream chars. (WriteStreamChar stream chars) =>
    stream -> Eff (streaming :: Streaming | eff) Unit

streamEnd = runMethodEff0 "end"

endEv      = event "end"      :: forall stream chars. (ReadStreamChar (stream chars) chars) => Event (stream chars) Unit
readableEv = event "readable" :: forall stream chars. (ReadStreamChar (stream chars) chars) => Event (stream chars) Unit
dataEv     = event "data"     :: forall stream chars. (ReadStreamChar (stream chars) chars) => Event (stream chars) chars
drainEv    = event "drain"    :: forall stream chars. (WriteStreamChar (stream chars) chars) => Event (stream chars) Unit

--

module Node.Stream.Object where

import Data.Null
import Data.Object
import Data.Function.Eff
import Control.Monad.Eff
import Node.Stream
import Node.Events

class ReadStream stream val
instance readStreamEv :: (ReadStream stream) => EventEmitter stream

class WriteStream stream val
instance writeStreamEv :: (WriteStream stream) => EventEmitter stream

class (ReadStream stream val, WriteStream stream val) <= ThroughStream stream val

streamPipe ::
  forall eff stream dest val. (ReadStream stream val, WriteStream dest val) =>
    stream -> dest -> Eff (streaming :: Streaming | eff) Unit

streamPipe = runMethodEff1 "pipe"

streamRead ::
  forall eff stream val. (ReadStream stream val) =>
    stream -> Eff (streaming :: Streaming | eff) (N val)

streamRead = runMethodEff0 "read"

streamPause ::
  forall eff stream val. (ReadStream stream val) =>
    stream -> Eff (streaming :: Streaming | eff) Unit

streamPause = runMethodEff0 "pause"

streamResume ::
  forall eff stream val. (ReadStream stream val) =>
    stream -> Eff (streaming :: Streaming | eff) Unit

streamResume = runMethodEff0 "resume"

streamWrite ::
  forall eff stream val. (WriteStream stream val) =>
    stream -> val -> Eff (streaming :: Streaming | eff) Boolean

streamWrite = runMethodEff1 "write"

streamEnd ::
  forall eff stream val. (WriteStream stream val) =>
    stream -> Eff (streaming :: Streaming | eff) Unit

streamEnd = runMethodEff0 "end"

endEv      = event "end"      :: forall stream val. (ReadStream (stream val) val) => Event (stream val) Unit
readableEv = event "readable" :: forall stream val. (ReadStream (stream val) val) => Event (stream val) Unit
dataEv     = event "data"     :: forall stream val. (ReadStream (stream val) val) => Event (stream val) val
drainEv    = event "drain"    :: forall stream val. (WriteStream (stream val) val) => Event (stream val) Unit
