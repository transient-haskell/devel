
module Main where

import Transient.Base
--import Transient.Internals((!>))
import Transient.Move
import Transient.Move.Utils
import Transient.Logged
import Transient.Move.Services
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class

import TestService

main= keep' $ runCloud $ do

      runTestNodes [2001]
--      local $ option "start1" "start1"
      clientStub ("hello","world")







