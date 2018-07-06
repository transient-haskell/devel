
module TestService where

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



clientStub params= do
      r <- callService  "" ("https://github.com/agocorona/testService","testserviceexe") params
      lliftIO $ print (r :: String)







