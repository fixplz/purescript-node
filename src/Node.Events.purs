module Node.Events
(
  EventEmitter,
  EvConfig(..),
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

class EventEmitter (emitter :: # ! -> *)

foreign import data Event :: (# ! -> *) -> * -> *

foreign import event
  "function event(name) { return name }"
  :: forall emitter val. String -> Event emitter val

type EventListener (eveff :: # !) val = Fn1 val Unit

errorEv = event "error" :: forall emitter. Event emitter Error

foreign import data EvConfig :: !

onEv ::
  forall eff eveff emitter evval. (EventEmitter emitter) =>
    emitter eveff -> Event emitter evval -> EventListener eveff evval -> Eff (evcfg :: EvConfig | eff) Unit

onEv = runMethodEff2 "on"

onceEv ::
  forall eff eveff emitter evval. (EventEmitter emitter) =>
    emitter eveff -> Event emitter evval -> EventListener eveff evval -> Eff (evcfg :: EvConfig | eff) Unit

onceEv = runMethodEff2 "once"

emitEv ::
  forall eveff emitter evval. (EventEmitter emitter) =>
    emitter eveff -> Event emitter evval -> evval -> Eff eveff Boolean

emitEv = runMethodEff2 "emit"

removeListener ::
  forall eff eveff emitter evval. (EventEmitter emitter) =>
    emitter eveff -> Event emitter evval -> EventListener eveff evval -> Eff (evcfg :: EvConfig | eff) Unit

removeListener = runMethodEff2 "removeListener"

removeAllListeners ::
  forall eff eveff emitter evval. (EventEmitter emitter) =>
    emitter eveff -> Event emitter evval -> Eff (evcfg :: EvConfig | eff) Unit

removeAllListeners = runMethodEff1 "removeAllListeners"

listeners ::
  forall eff eveff emitter evval. (EventEmitter emitter) =>
    emitter eveff -> Event emitter evval -> Eff (evcfg :: EvConfig | eff) [EventListener eveff evval]

listeners = runMethodEff1 "listeners"

asListener :: forall val eff. (val -> Eff eff Unit) -> EventListener eff val
asListener cb = mkFn1 \val -> runEff (cb val)

foreign import runEff
  "function runEff(eff) { return eff() }"
  :: forall eff val. Eff eff val -> val
