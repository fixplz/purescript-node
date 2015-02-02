module Node.Events
(
  EventEmitter,
  EvEff(..),
  EventListener(), asListener,
  Event(..), event,
  onEv,
  onceEv,
  emitEv,
  removeListener,
  removeAllListeners,
  listeners,
  errorEv
)
where

import Data.Object
import Data.Function
import Data.Function.Eff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

class EventEmitter emitter

foreign import data Event :: * -> * -> *

foreign import event
  "function event(name) { return name }"
  :: forall emitter val. String -> Event emitter val

type EventListener val = Fn1 val Unit

errorEv = event "error" :: forall emitter. Event emitter Error

foreign import data EvEff :: !

onEv ::
  forall eff emitter evval. (EventEmitter emitter) =>
    emitter -> Event emitter evval -> EventListener evval -> Eff (ev :: EvEff | eff) Unit

onEv = runMethodEff2 "on"

onceEv ::
  forall eff emitter evval. (EventEmitter emitter) =>
    emitter -> Event emitter evval -> EventListener evval -> Eff (ev :: EvEff | eff) Unit

onceEv = runMethodEff2 "once"

emitEv ::
  forall eff emitter evval. (EventEmitter emitter) =>
    emitter -> Event emitter evval -> evval -> Eff (ev :: EvEff | eff) Boolean

emitEv = runMethodEff2 "emit"

removeListener ::
  forall eff emitter evval. (EventEmitter emitter) =>
    emitter -> Event emitter evval -> EventListener evval -> Eff (ev :: EvEff | eff) Unit

removeListener = runMethodEff2 "removeListener"

removeAllListeners ::
  forall eff emitter evval. (EventEmitter emitter) =>
    emitter -> Event emitter evval -> Eff (ev :: EvEff | eff) Unit

removeAllListeners = runMethodEff1 "removeAllListeners"

listeners ::
  forall eff emitter evval. (EventEmitter emitter) =>
    emitter -> Event emitter evval -> Eff (ev :: EvEff | eff) [EventListener evval]

listeners = runMethodEff1 "listeners"

asListener :: forall val eff. (val -> Eff eff Unit) -> EventListener val
asListener cb = mkFn1 \val -> runEff (cb val)

foreign import runEff
  "function runEff(eff) { return eff() }"
  :: forall eff val. Eff eff val -> val
