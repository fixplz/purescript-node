module Node.Events
(
  EventEmitter,
  NativeEventEmitter,
  onEv,
  onceEv,
  emitEv,
  removeListener,
  removeAllListeners,
  listeners,
  EvEff(..),
  Event,
  ErrorEv(), errorEv,
  EventListener(),
  asListener
)
where

import Data.Object
import Data.Function
import Data.Function.Eff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (Error())

type EventListener val = Fn1 val Unit

foreign import data EvEff :: !

class EventEmitter emitter where
  onEv :: forall eff ev evval. (Event emitter ev evval) =>
    emitter -> ev -> EventListener evval -> Eff (ev :: EvEff | eff) Unit
  onceEv :: forall eff ev evval. (Event emitter ev evval) =>
    emitter -> ev -> EventListener evval -> Eff (ev :: EvEff | eff) Unit
  emitEv :: forall eff ev evval. (Event emitter ev evval) =>
    emitter -> ev -> evval -> Eff (ev :: EvEff | eff) Boolean
  removeListener :: forall eff ev evval. (Event emitter ev evval) =>
    emitter -> ev -> EventListener evval -> Eff (ev :: EvEff | eff) Unit
  removeAllListeners :: forall eff ev evval. (Event emitter ev evval) =>
    emitter -> ev -> Eff (ev :: EvEff | eff) Unit
  listeners :: forall eff ev evval. (Event emitter ev evval) =>
    emitter -> ev -> Eff (ev :: EvEff | eff) [EventListener evval]

class Event emitter event eventValue

foreign import data ErrorEv :: *
foreign import errorEv "var errorEv = 'error'" :: ErrorEv

instance emitterErrorEv :: (EventEmitter emitter) => Event emitter ErrorEv Error

asListener :: forall val eff. (val -> Eff eff Unit) -> EventListener val
asListener cb = mkFn1 \val -> runEff (cb val)

foreign import runEff """function runEff(eff) { return eff() }"""
  :: forall eff val. Eff eff val -> val

-- 

class NativeEventEmitter emitter

instance nativeEventEmitter :: (NativeEventEmitter emitter) => EventEmitter emitter where
  onEv = runMethod2 "on"
  onceEv = runMethod2 "once"
  emitEv = runMethod2 "emit"
  removeListener = runMethod2 "removeListener"
  removeAllListeners = runMethod1 "removeAllListeners"
  listeners = runMethod1 "listeners"
