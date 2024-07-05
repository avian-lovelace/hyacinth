module Core.EitherIO
  ( EitherIO (EitherIO, runEitherIO),
    liftIO,
    liftEither,
    throwError,
    liftWithErrors,
    runAndOutputErrors,
  )
where

import Core.Errors
import Data.Bifunctor
import Data.Sequence (Seq)

newtype EitherIO e a = EitherIO {runEitherIO :: IO (Either e a)}
  deriving (Functor)

instance Applicative (EitherIO e) where
  pure x = EitherIO $ return (Right x)
  eioF <*> eioX = EitherIO $ do
    resultF <- runEitherIO eioF
    case resultF of
      Left e -> return $ Left e
      Right f -> runEitherIO $ f <$> eioX

instance Monad (EitherIO e) where
  eioX >>= makeEioY = EitherIO $ do
    resultX <- runEitherIO eioX
    case resultX of
      Left e -> return $ Left e
      Right x -> runEitherIO $ makeEioY x

instance Bifunctor EitherIO where
  first f eio = EitherIO $ do
    result <- runEitherIO eio
    case result of
      Left e -> return . Left $ f e
      Right x -> return . Right $ x

  second = fmap

liftIO :: IO a -> EitherIO e a
liftIO io = EitherIO $ Right <$> io

liftEither :: Either e a -> EitherIO e a
liftEither withErrors = EitherIO $ return withErrors

throwError :: String -> EitherIO [String] a
throwError e = EitherIO . return $ Left [e]

liftWithErrors :: WithErrors a -> EitherIO (Seq Error) a
liftWithErrors (Success a) = return a
liftWithErrors (Error es) = EitherIO . return . Left $ es

runAndOutputErrors :: EitherIO [String] () -> IO ()
runAndOutputErrors eio = do
  result <- runEitherIO eio
  case result of
    Left es -> mapM_ putStrLn es
    Right () -> return ()