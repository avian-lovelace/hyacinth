module Core.ErrorIO
  ( ErrorIO (ErrorIO, runErrorIO),
    liftIO,
    liftWithErrors,
    runAndOutputErrors,
  )
where

import Core.Errors
import Core.Utils

newtype ErrorIO a = ErrorIO {runErrorIO :: IO (WithErrors a)}
  deriving (Functor)

instance Applicative ErrorIO where
  pure a = ErrorIO $ return (Success a)
  eioF <*> eioA = ErrorIO $ do
    resultF <- runErrorIO eioF
    case resultF of
      Success f -> runErrorIO $ f <$> eioA
      Error es -> return $ Error es

instance Monad ErrorIO where
  eioA >>= makeEioB = ErrorIO $ do
    resultA <- runErrorIO eioA
    case resultA of
      Success a -> runErrorIO $ makeEioB a
      Error es -> return $ Error es

liftIO :: IO a -> ErrorIO a
liftIO io = ErrorIO $ Success <$> io

liftWithErrors :: WithErrors a -> ErrorIO a
liftWithErrors withErrors = ErrorIO $ return withErrors

runAndOutputErrors :: ErrorIO () -> IO ()
runAndOutputErrors eio = do
  result <- runErrorIO eio
  case result of
    Success () -> return ()
    Error es -> mapM_ (putStrLn . pretty) es