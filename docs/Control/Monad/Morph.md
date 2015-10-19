## Module Control.Monad.Morph

A port of Haskell's [mmorph library](http://hackage.haskell.org/package/mmorph-1.0.0/docs/Control-Monad-Morph.html)

#### `MFunctor`

``` purescript
class MFunctor t where
  hoist :: forall m n a b. (Monad m) => (forall a. m a -> n a) -> t m b -> t n b
```

##### Instances
``` purescript
instance mfunctorExceptT :: MFunctor (ExceptT e)
instance mfunctorMaybe :: MFunctor MaybeT
instance mfunctorReaderT :: MFunctor (ReaderT r)
instance mfunctorWriterT :: MFunctor (WriterT w)
instance mfunctorStateT :: MFunctor (StateT s)
instance mfunctorRWS :: MFunctor (RWST r w s)
instance mfunctorCompose :: (Functor f) => MFunctor (Compose f)
instance mfunctorProduct :: MFunctor (Product f)
```

#### `generalize`

``` purescript
generalize :: forall m a. (Monad m) => Identity a -> m a
```

#### `MMonad`

``` purescript
class (MFunctor t, MonadTrans t) <= MMonad t where
  embed :: forall n m b. (Monad n) => (forall a. m a -> t n a) -> t m b -> t n b
```

##### Instances
``` purescript
instance mmonadExceptT :: MMonad (ExceptT e)
instance mmonadMaybeT :: MMonad MaybeT
instance mmonadReaderT :: MMonad (ReaderT r)
instance mmonadWriterT :: (Monoid w) => MMonad (WriterT w)
```

#### `squash`

``` purescript
squash :: forall a m t. (Monad m, MMonad t) => t (t m) a -> t m a
```

#### `(>|>)`

``` purescript
(>|>) :: forall m1 m2 m3 t c. (MMonad t, Monad m3) => (forall a. m1 a -> t m2 a) -> (forall b. m2 b -> t m3 b) -> m1 c -> t m3 c
```

_right-associative / precedence 2_

#### `(<|<)`

``` purescript
(<|<) :: forall m1 m2 m3 t c. (MMonad t, Monad m3) => (forall b. m2 b -> t m3 b) -> (forall a. m1 a -> t m2 a) -> m1 c -> t m3 c
```

_left-associative / precedence 2_

#### `(=<|)`

``` purescript
(=<|) :: forall t m n b. (MMonad t, Monad n) => (forall a. m a -> t n a) -> t m b -> t n b
```

_right-associative / precedence 2_

#### `(|>=)`

``` purescript
(|>=) :: forall t m n b. (MMonad t, Monad n) => t m b -> (forall a. m a -> t n a) -> t n b
```

_left-associative / precedence 2_


