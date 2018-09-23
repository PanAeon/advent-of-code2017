{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module FreeMonadsIn7EasySteps
    (
    ) where

-- https://joashc.github.io/posts/2015-09-13-free-monad-steps.html
import Control.Monad.Free
import Control.Monad.Trans.State.Lazy
import Control.Monad.Free.TH
import Control.Monad.IO.Class

-- Step 1
data DcNodeOperator state error next =
    InitState next
  | AwaitStateCondition (state -> Bool) (state -> next)
  | GetState (state -> next)
  | GetUserInput (String -> next)
  | ModifyState (state -> state) next
  | DisplayMessage String next
  | GetRandomInt Int (Int -> next)
  | Throw error next
  deriving (Functor)

-- Step 2
-- instance Functor (DcNodeOperator state error next) where
--   fmap f (InitState next) = InitState (f next)
--   fmap f (AwaitStateCondition g h) = AwaitStateCondition g (f . h)
--   fmap f (GetState g) = GetState (f . g)
  -- ...

-- Step 3
-- initState :: Free (DcNodeOperator state error) ()
-- initState = Free (InitServer state error (Pure ()))
--
-- getState :: Free (GetState state error) state
-- getState = Free (GetState state error) id
-- ...

-- stuff
data PeerState = PeerState Int
data PeerError = PeerError String
data Participant = Participant Int

-- stuff


port = id


-- port = 32

numPeers :: PeerState -> Integer
numPeers _ = 3


peers = []

getRandomNumber :: Int -> IO Int
getRandomNumber = pure


makeFree ''DcNodeOperator

-- step 4
type DcPeerOperator = DcNodeOperator PeerState PeerError
type DcPeer = Free DcPeerOperator

-- step 5

-- initPeer :: DcPeer ()
-- initPeer = do
--   initState
--   displayMessage "What port do you want?"
--   enteredPort <- getUserInput
--   modifyState $ port .~ (parse enteredPort)
--   state <- getState
--   displayMessage $ "Using port: " ++ show state ^. port
--
-- awaitPeers :: DcPeer [Participant]
-- awaitPeers = do
--   state <- awaitStateCondition $ (> 1) . numPeers
--   return $ state ^. peers



-- peerProgram :: DcPeer ()
-- peerProgram = do
--   initPeer
--   peers <- awaitPeers
--   displayMessage $ "Peers: " ++ show peers

-- step 6

type DcPeerIO = StateT (PeerState) IO

-- peerInterpreter :: DcPeerOperator (DcPeerIO next) -> DcPeerIO next
-- peerInterpreter (GetUserInput next) = do
--   userInput <- liftIO getLine
--   next userInput
-- peerInterpreter (GetState next) = get >>= next
-- peerInterpreter (DisplayMessage m next) = do
--   liftIO $ putStrLn m
--   next
-- peerInterpreter (GetRandomInt max next) = do
--   num <- liftIO $ getRandomNumber max
--   next num
-- etc

-- Step 7: Run your program
-- initialState = PeerState [] 0 ""

-- runPeer = runStateT (iterM peerInterpreter $ peerProgram) initialState
