-- | This test is a port from the [mmorph tutorial](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html#g:6) section on mixing diverse transformers.
module Test.Main where

import Prelude
import Data.Foldable
import Data.Identity
import Data.Array
import Control.Monad.State.Trans
import Control.Monad.Eff.Console
import Control.Monad.Writer.Trans
import Control.Monad.Morph

tick :: StateT Int Identity Unit
tick = modify (+1)

type MyEnv = WriterT (Array Int) Identity
type MyState = StateT Int MyEnv

save :: MyState Unit
save = do
  n <- get
  lift $ tell [n :: Int]

replicateM_ :: forall m a. (Monad m) => Int -> m a -> m Unit
replicateM_ n m = sequence_ (replicate n m)

tock :: StateT Int _ Unit
tock = do
  hoist generalize tick
  lift $ log "Tock!"

program :: StateT Int (WriterT (Array Int) _) Unit
program = replicateM_ 4 $ do
  hoist lift tock
  hoist (hoist generalize) save

main = execWriterT (runStateT program 0)
