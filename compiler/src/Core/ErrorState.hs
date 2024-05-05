{-# LANGUAGE TupleSections #-}

module Core.ErrorState
  ( ErrorState (ErrorState, runErrorState),
    getState,
    setState,
    liftWithErrors,
    throwError,
    andFinally,
    traverse',
  )
where

import Core.Errors
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable
import Data.Sequence (Seq (Empty), (<|), (|>))

newtype ErrorState state a = ErrorState {runErrorState :: state -> (state, WithErrors a)}

instance Functor (ErrorState state) where
  fmap f errorState = ErrorState $ \state -> case runErrorState errorState state of
    (newState, Success a) -> (newState, Success $ f a)
    (newState, Error es) -> (newState, Error es)

instance Applicative (ErrorState state) where
  pure a = ErrorState (,Success a)
  (<*>) errorStateF errorStateA = ErrorState $ \state ->
    case runErrorState errorStateF state of
      (newState, Success f) -> runErrorState (f <$> errorStateA) newState
      (newState, Error e) -> (newState, Error e)

instance Monad (ErrorState state) where
  errorStateA >>= makeErrorStateB = ErrorState $ \state ->
    case runErrorState errorStateA state of
      (newState, Success a) -> runErrorState (makeErrorStateB a) newState
      (newState, Error e) -> (newState, Error e)

getState :: ErrorState state state
getState = ErrorState $ \state -> (state, Success state)

setState :: state -> ErrorState state ()
setState newState = ErrorState $ const (newState, Success ())

liftWithErrors :: WithErrors a -> ErrorState state a
liftWithErrors a = ErrorState (,a)

throwError :: Error -> ErrorState state a
throwError = liftWithErrors . singleError

-- andFinally enables running a computation after another, even if the first computation errors
andFinally :: ErrorState state a -> ErrorState state b -> ErrorState state a
binder1 `andFinally` binder2 = ErrorState $ \state1 ->
  let (state2, result) = runErrorState binder1 state1
   in let (state3, _) = runErrorState binder2 state2
       in (state3, result)

{- A variant of the traverse function where if multiple of the computations have errors, they are all included in the
resulting Error output.
-}
traverse' :: (a -> ErrorState state b) -> Seq a -> ErrorState state (Seq b)
traverse' makeBinder xs = sequenceA' $ makeBinder <$> xs

sequenceA' :: Seq (ErrorState state a) -> ErrorState state (Seq a)
sequenceA' binders = ErrorState $ \state -> second collectResults (runBinders state)
  where
    runBinders initialState = foldl' (\(state, results) binder -> let (newState, result) = runErrorState binder state in (newState, results |> result)) (initialState, Empty) binders
    collectResults xs = foldrWithErrors (<|) Empty xs