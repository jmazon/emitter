{- |
Module: Control.Emitter
Description: Simple local pub/sub
Copyright: © 2020 Jean-Baptiste Mazon
License: BSD3
Stability: unstable
Portability: STM

This is a simple port of NPM's
<https://www.npmmjs.com/package/component-emitter Emitter> component,
based on 'IO' and 'TVar's.

The mixin interface isn't supported.

> do
>   emitter <- newEmitter
>   emit emitter "something" []

Note that the event names are 'Text', to integrate with `aeson`.
-}

{-# LANGUAGE NamedFieldPuns #-}

module Control.Emitter
  ( -- * Main interface
    newEmitter
  , on
  , once
  , off
  , emit
  , listeners
  , hasListeners
    -- * Types
  , Emitter
  , Event
  , Handler
  , HandlerId
  ) where

import Control.Monad
import Control.Arrow
import Control.Concurrent.STM

import Data.Text (Text)
import Data.Aeson (Value)
import Data.Unique
import Data.HashMap.Strict as H

type Event = Text
type Handler = [Value] -> IO ()
newtype HandlerId = HandlerId { unHandlerId :: Unique }

type Handlers = HashMap Unique (Bool,Handler)
type Listeners = HashMap Event Handlers

newtype Emitter = Emitter { eListeners :: TVar Listeners }

-- | Create a standalone 'Emitter' instance.
newEmitter :: IO Emitter
newEmitter = Emitter <$> newTVarIO empty

addListener :: Bool -> Emitter -> Event -> Handler -> IO HandlerId
addListener keep Emitter{eListeners} evt fn = do
  let v = (keep,fn)
  i <- newUnique
  atomically $
    modifyTVar' eListeners $
      alter (Just . maybe (singleton i v) (insert i v)) evt
  pure (HandlerId i)

-- | Register an event handler.
on :: Emitter -> Event -> Handler -> IO HandlerId
on = addListener True

-- | Register a single-shot event handler, removed immediately after
-- it is invoked the first time.
once :: Emitter -> Event -> Handler -> IO HandlerId
once = addListener False

-- | Remove a listener, all if passed 'Nothing'.
off :: Emitter -> Maybe Event -> Maybe HandlerId -> IO ()
off Emitter{eListeners} mbE mbH = do
  let filterHandlers hs = (`delete` hs) . unHandlerId <$> mbH
      filterListeners ls = maybe (mapMaybe filterHandlers ls)
                             (flip (update filterHandlers) ls) mbE
  atomically $ modifyTVar' eListeners filterListeners

-- | Emit an event with variable option arguments.
emit :: Emitter -> Event -> [Value] -> IO ()
emit Emitter{eListeners} evt args = do
  let stateListeners :: Listeners -> ([Handler],Listeners)
      stateListeners = maybe [] (Prelude.map snd . elems) . H.lookup evt
                       &&& update filterHandlers evt
      filterHandlers :: Handlers -> Maybe Handlers
      filterHandlers = mfilter (not . H.null) . pure . H.filter fst
  hs <- atomically $ stateTVar eListeners stateListeners
  mapM_ ($ args) hs

-- | Return a list of callbacks, or an empty list.
listeners :: Emitter -> Event -> IO [Handler]
listeners Emitter{eListeners} evt =
  maybe [] (Prelude.map snd . elems) . H.lookup evt <$> readTVarIO eListeners

-- | Check whether this emitter has handlers for `event`.
hasListeners :: Emitter -> Event -> IO Bool
hasListeners Emitter{eListeners} evt =
  maybe False (not . H.null) . H.lookup evt <$> readTVarIO eListeners
