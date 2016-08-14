-- | This test is a port from the [mmorph tutorial](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html#g:6) section on mixing diverse transformers.
module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Morph (generalize, hoist)
import Control.Monad.State.Trans (StateT, runStateT, get, modify)
import Control.Monad.Writer.Trans (WriterT, execWriterT, lift, tell)
import Data.Identity (Identity)
import Data.Unfoldable (replicateA)

tick :: StateT Int Identity Unit
tick = modify (_ +1)

type MyEnv = WriterT (Array Int) Identity
type MyState = StateT Int MyEnv

save :: MyState Unit
save = do
  n <- get
  lift $ tell [n :: Int]

tock :: forall eff. StateT Int (Eff (console :: CONSOLE | eff)) Unit
tock = do
  hoist generalize tick
  lift $ log "Tock!"

program :: forall eff. StateT Int (WriterT (Array Int) (Eff (console :: CONSOLE | eff))) (Array Unit)
program = replicateA 4 $ do
  hoist lift tock
  hoist (hoist generalize) save

main :: forall eff. Eff (console :: CONSOLE | eff) (Array Int)
main = execWriterT (runStateT (void program) 0)
