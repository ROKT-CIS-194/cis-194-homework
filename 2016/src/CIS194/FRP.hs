module CIS194.FRP where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bifunctor (bimap)

import Prelude hiding (id, (.))

data FRP m i o = Step (i -> m (o, FRP m i o))

instance Monad m => Category (FRP m) where
  id = Step $ \i -> pure (i, id)
  Step gm . Step fm = Step $ \i -> do
    (fo, fk) <- fm i
    (go, gk) <- gm fo
    return (go, gk . fk)

instance Monad m => Arrow (FRP m) where
  arr f = Step $ \i -> pure (f i, arr f)
  first (Step m) = Step $ \(il,ir) ->
    fmap (\(ol,k) -> ((ol,ir), first k)) (m il)

instance Monad m => ArrowChoice (FRP m) where
  left (Step m) = Step $ \i ->
    case i of
      Left x -> fmap (bimap Left left) (m x)
      Right x -> pure (Right x, left (Step m))

instance Functor m => Functor (FRP m i) where
  fmap f (Step m) = Step $ fmap (bimap f (fmap f)) . m

instance Applicative m => Applicative (FRP m i) where
  pure x = Step $ \_ -> pure (x, pure x)
  Step fm <*> Step xm = Step $ \i ->
    (\(fo, fk) (xo, xk) -> (fo xo, fk <*> xk)) <$> fm i <*> xm i

instance (Monad m, Alternative m) => Alternative (FRP m i) where
  empty = Step $ \_ -> empty
  Step m <|> y = Step $ fmap (fmap (<|> y)) . m

nSteps :: (Monad m, Alternative m) => Int -> FRP m i o -> FRP m i o
nSteps n (Step m) =
  if n <= 0 then empty else Step $ fmap (fmap (nSteps (n-1))) . m

feedMT :: Monad m => [i] -> FRP m i o -> m [o]
feedMT [] _ = pure []
feedMT (i:is) (Step m) = do
  (o, k) <- m i
  (o:) <$> feedMT is k

liftMT :: Monad m => (i -> m o) -> FRP m i o
liftMT f = Step $ \i -> do
  x <- f i
  return (x, liftMT f)

runReaderMT :: Functor m => r -> FRP (ReaderT r m) i o -> FRP m i o
runReaderMT r (Step m) = Step $ \i ->
  fmap (fmap (runReaderMT r)) (runReaderT (m i) r)

execStateMT :: Functor m => s -> FRP (StateT s m) i o -> FRP m i s
execStateMT s (Step m) = Step $ \i ->
  fmap (\((_, k), s') -> (s', execStateMT s' k)) (runStateT (m i) s)

runMaybeMT :: Monad m => FRP (MaybeT m) i o -> FRP m i (Maybe o)
runMaybeMT (Step m) = Step $ fmap (maybe f g) . runMaybeT . m
  where
    done = Step $ \_ -> pure (Nothing, done)
    f = (Nothing, done)
    g = bimap Just maskMaybeMT

maskMaybeMT :: Monad m => FRP (MaybeT m) i o -> FRP m i (Maybe o)
maskMaybeMT (Step m) = Step $ fmap (maybe f g) . runMaybeT . m
  where
    f = (Nothing, maskMaybeMT (Step m))
    g = bimap Just maskMaybeMT

catchMT :: Monad m => FRP (MaybeT m) i o -> FRP m i o -> FRP m i o
catchMT (Step m) (Step m') = Step $ \i -> do
  result <- runMaybeT (m i)
  maybe (m' i) (\(o, k) -> return (o, catchMT k (Step m'))) result
