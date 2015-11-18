module HiePlugin
    (
        runGhciNg
      -- , hieBehavior
      -- , hieToGhci
      , OutData(..)
      , InData(..)
    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.Knob as K
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           MainRunner
import           System.Console.Haskeline
import           System.IO

-- ---------------------------------------------------------------------
{-
Stage 1
-------

We have two different TChans. The one is used for stdout/stderr, the other for
stdin

The stdout one is simply written to here, assuming that the intrgrator of this
library is listening on it.

The stdin one is a local process, that listens on the Chan for input passed in,
buffers it by line, and, makes it available for the underlying ghci process via
a `Knob`.

-}

data OutData = Stdout String
             | Stderr String
             deriving (Show)

data InData = Stdin String
             deriving (Show)


-- ---------------------------------------------------------------------

-- | Start up a ghci-ng instance
runGhciNg :: Chan InData -> Chan OutData -> IO ()
runGhciNg chin chout = do
  knob <- K.newKnob B.empty
  _ <- forkIO (hieToGhci chin knob)
  runMain (runInputTBehavior (hieBehavior knob chout))

-- ---------------------------------------------------------------------

-- | Process to receive a stdin command from HIE and pass it on to the ghci
-- session.
hieToGhci :: Chan InData -> K.Knob -> IO ()
hieToGhci cin knob = do
  forever $ do
    inMsg <- readChan cin
    case inMsg of
      Stdin str -> do
        cur <- K.getContents knob
        K.setContents knob (B.append cur (C8.pack str))
    return ()

-- ---------------------------------------------------------------------

hieBehavior :: K.Knob -> Chan OutData -> Behavior
hieBehavior knob cout = Behavior (runHieTerm knob cout)

runHieTerm :: K.Knob -> Chan OutData -> IO RunTerm
runHieTerm knob cout = do
  hin <- K.newFileHandle knob "stdin" ReadMode
  return
    RunTerm
      { putStrOut = \str -> writeChan cout (Stdout str)
      , closeTerm = return ()
      , wrapInterrupt = id
      , termOps = Right FileOps
         { inputHandle = hin
         , wrapFileInput = id
         , getLocaleChar = maybeGetChar hin
         , maybeReadNewline = hMaybeReadNewline hin
         , getLocaleLine = maybeGetLine hin
         }
      }

maybeGetChar :: Handle -> MaybeT IO Char
maybeGetChar h = lift $ hGetChar h

maybeGetLine :: Handle -> MaybeT IO String
maybeGetLine h = lift $ hGetLine h

{-
FileOps
  inputHandle :: Handle -- e.g. for turning off echoing.
  wrapFileInput :: forall a. IO a -> IO a
  getLocaleLine :: MaybeT IO String
  getLocaleChar :: MaybeT IO Char
  maybeReadNewline :: IO ()
-}
