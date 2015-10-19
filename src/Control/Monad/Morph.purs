-- | A port of Haskell's [mmorph library](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html)
module Control.Monad.Morph where

import Prelude

import Control.Monad.Trans (MonadTrans, lift)
import qualified Control.Monad.Except.Trans as E
import qualified Control.Monad.Maybe.Trans as M
import qualified Control.Monad.Reader.Trans as R
import qualified Control.Monad.RWS.Trans as RWS
import qualified Control.Monad.State.Trans as S
import qualified Control.Monad.Writer.Trans as W
import qualified Control.Monad.List.Trans as L

import Data.Monoid (Monoid)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Functor.Compose (Compose(Compose))
import Data.Identity (runIdentity, Identity(Identity))
import Data.Functor.Product (Product(Product))
import Data.Tuple (Tuple(Tuple))

class MFunctor t where
  hoist :: forall m n a b. (Monad m) => (forall a. m a -> n a) -> t m b -> t n b

instance mfunctorExceptT :: MFunctor (E.ExceptT e) where
  hoist nat m = E.ExceptT (nat (E.runExceptT m))

--instance mfunctorList :: MFunctor L.ListT where
  --hoist nat m = L.ListT (nat (L.runListT m))

instance mfunctorMaybe :: MFunctor M.MaybeT where
  hoist nat m = M.MaybeT (nat (M.runMaybeT m))

instance mfunctorReaderT :: MFunctor (R.ReaderT r) where
  hoist nat m = R.ReaderT (\ i -> nat (R.runReaderT m i))

instance mfunctorWriterT :: MFunctor (W.WriterT w) where
  hoist nat m = W.WriterT (nat (W.runWriterT m))

instance mfunctorStateT :: MFunctor (S.StateT s) where
  hoist nat m = S.StateT (\ s -> nat (S.runStateT m s))

instance mfunctorRWS :: MFunctor (RWS.RWST r w s) where
  hoist nat m = RWS.RWST (\ r s -> nat (RWS.runRWST m r s))

instance mfunctorCompose :: (Functor f) => MFunctor (Compose f) where
  hoist nat (Compose f) = Compose (map nat f)

instance mfunctorProduct :: MFunctor (Product f) where
  hoist nat (Product (Tuple f g)) = Product (Tuple f (nat g))

generalize :: forall m a. (Monad m) => Identity a -> m a
generalize = pure <<< runIdentity

class (MFunctor t, MonadTrans t) <= MMonad t where
  embed :: forall n m b. (Monad n) => (forall a. m a -> t n a) -> t m b -> t n b

squash :: forall a m t. (Monad m, MMonad t) => t (t m) a -> t m a
squash = embed id

infixr 2 >|>
infixr 2 =<|
infixl 2 <|<
infixl 2 |>=

(>|>) :: forall m1 m2 m3 t c. (MMonad t, Monad m3) => (forall a. m1 a -> t m2 a)
                                                   -> (forall b. m2 b -> t m3 b)
                                                   ->            m1 c -> t m3 c
(>|>) f g m = embed g (f m)

(<|<) :: forall m1 m2 m3 t c. (MMonad t, Monad m3) => (forall b. m2 b -> t m3 b)
                                                   -> (forall a. m1 a -> t m2 a)
                                                   ->            m1 c -> t m3 c
(<|<) g f m = embed g (f m)

(=<|) :: forall t m n b. (MMonad t, Monad n) => (forall a. m a -> t n a)
                                             ->          t m b -> t n b
(=<|) = embed

(|>=) :: forall t m n b. (MMonad t, Monad n) => t m b
                                             -> (forall a. m a -> t n a)
                                             -> t n b
(|>=) t f = embed f t

instance mmonadExceptT :: MMonad (E.ExceptT e) where
  embed f m = E.ExceptT (do
                        x <- E.runExceptT (f (E.runExceptT m))
                        return (case x of
                               Left e -> Left e
                               Right (Left e) -> Left e
                               Right (Right a) -> Right a))

--instance mmonadListT :: MMonad L.ListT where
  --embed f m = L.ListT (do
                      --x <- L.runListT (f (L.runListT m))
                      --return (concat x))

instance mmonadMaybeT :: MMonad M.MaybeT where
  embed f m = M.MaybeT (do
                       x <- M.runMaybeT (f (M.runMaybeT m))
                       return (case x of
                              Nothing -> Nothing
                              Just Nothing -> Nothing
                              Just (Just a) -> Just a))

instance mmonadReaderT :: MMonad (R.ReaderT r) where
  embed f m = R.ReaderT (\ i -> R.runReaderT (f (R.runReaderT m i)) i)

instance mmonadWriterT :: (Monoid w) => MMonad (W.WriterT w) where
  embed f m = W.WriterT (do
                        Tuple (Tuple a w1) w2 <- W.runWriterT (f (W.runWriterT m))
                        return (Tuple a (append w1 w2)))
