{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Control.Monad
import Data.IORef

import Control.Emitter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Emitter"
  [ testOn, testOnce, testOff, testListeners, testHasListeners ]

testOn :: TestTree
testOn = testGroup "#on"
  [ testCase "fires once" $ do
      (e,r) <- setup on
      emit e "a" []
      ((1,0) @=?) =<< r
  , testCase "fires twice" $ do
      (e,r) <- setup on
      emit e "a" []
      emit e "a" []
      ((2,0) @=?) =<< r
  ]

testOnce :: TestTree
testOnce = testGroup "#once"
  [ testCase "fires once" $ do
      (e,r) <- setup once
      emit e "a" []
      ((1,0) @=?) =<< r
  , testCase "doesn't fire twice" $ do
      (e,r) <- setup once
      emit e "a" []
      emit e "a" []
      ((1,0) @=?) =<< r
  ]

setup f = do
  e <- newEmitter
  a <- newIORef 0
  b <- newIORef 0
  f e "a" (const $ modifyIORef a succ)
  f e "b" (const $ modifyIORef b succ)
  pure (e,liftA2 (,) (readIORef a) (readIORef b))

testOff :: TestTree
testOff = testGroup "#off"
  [ testCase "event,fn" $ do
      (e,i,r) <- s
      off e (Just "a") (Just i)
      p e
      ([0,1,1,1] @=?) =<< r
  , testCase "event" $ do
      (e,_,r) <- s
      off e (Just "a") Nothing
      p e
      ([0,0,1,1] @=?) =<< r
  , testCase "fn" $ do -- not in NPM
      (e,i,r) <- s
      off e Nothing (Just i)
      p e
      ([0,1,1,1] @=?) =<< r
  , testCase "nothing" $ do
      (e,_,r) <- s
      off e Nothing Nothing
      p e
      ([0,0,0,0] @=?) =<< r
  ]
  where
    s = do
      e <- newEmitter
      rs@[a1,a2,b1,b2] <- replicateM 4 (newIORef 0)
      [i,_] <- mapM (on e "a" . const . incrIORef) [a1,a2]
      mapM_ (on e "b". const . incrIORef) [b1,b2]
      pure (e,i,mapM readIORef rs)
    p e = do
      emit e "a" []
      emit e "b" []

setup2 evt = do
  e <- newEmitter
  on e evt (const (pure ()))
  pure e

testListeners :: TestTree
testListeners = testGroup "#listeners"
  [ testCase "empty" $
    assertBool "" . null =<< flip listeners "a" =<< newEmitter

  , testCase "one" $
    (1 @=?) . length =<< flip listeners "a" =<< setup2 "a"

  , testCase "miss" $
    assertBool "" . null =<< flip listeners "b" =<< setup2 "a"
  ]

testHasListeners :: TestTree
testHasListeners = testGroup "#hasListeners"
  [ testCase "empty" $
    assertBool "" . not =<< flip hasListeners "a" =<< newEmitter

  , testCase "one" $
    assertBool "" =<< flip hasListeners "a" =<< setup2 "a"

  , testCase "miss" $
    assertBool "" . not =<< flip hasListeners "a" =<< setup2 "b"

  , testCase "removed" $ do
      e <- newEmitter
      i <- on e "a" undefined
      off e Nothing (Just i)
      assertBool "" . not =<< hasListeners e "a"
  ]

incrIORef :: IORef Int -> IO ()
incrIORef r = modifyIORef r succ
