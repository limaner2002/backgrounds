{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}

module MachineUtils
  ( downloadHttp
  , Control.Arrow.Machine.tee
  , Control.Arrow.Machine.run_
  , Control.Arrow.Machine.run
  , inputCommand
  , sink
  , createRequest
  , NothingYet (..)
  , machine
  ) where

import ClassyPrelude hiding (first)
import Control.Arrow.Machine
import Control.Arrow
import Network
import qualified Network.Socket as NS
import qualified Data.Streaming.Network as SN
import Network.HTTP.Client
import Control.Monad.Trans.Resource
import Network.HTTP.Client.TLS
import qualified System.IO as SIO
import Network.HTTP.Simple
import Numeric
import System.Console.ANSI

sink :: (Show a, MonadIO m) => (a -> m ()) -> ProcessA (Kleisli m) (Event a) (Event ())
sink act = repeatedlyT kleisli0 $ do
  x <- await
  lift $ act x

showItSink x = do
  liftIO $ do
    clearLine
    setCursorColumn 0
  putStr $ tshow x

sourceListen :: NS.Socket -> ProcessA (Kleisli IO) (Event ()) (Event (Socket, NS.SockAddr))
sourceListen sock = repeatedlyT kleisli0 $ do
  _ <- await
  accepted <- lift $ NS.accept sock
  yield accepted

sourceSocket :: ProcessA (Kleisli IO) (Event (Socket, NS.SockAddr)) (Event ByteString)
sourceSocket = constructT kleisli0 go
  where
    go = do
      (sock, addr) <- await
      lift $ putStrLn $ "Accepted connection from " <> tshow addr
      loop sock
    loop sock = do
      bs <- lift $ SN.safeRecv sock 4096
      if null bs
        then do
          lift $ putStrLn "Socket is now empty"
          lift (NS.close sock)
          go
        else do
          yield bs
          loop sock

decodeIt :: ArrowApply a => ProcessA a (Event ByteString) (Event Text)
decodeIt = repeatedly $ do
  x <- await
  yield $ decodeUtf8 x

sourceHttp :: MonadResource m => ProcessA
  (Kleisli m)
  (Event (Request, Manager))
  (Event ByteString)
sourceHttp = makeRequest >>> sourceHttp_ >>> evMap snd

data DownloadProgress = DownloadProgress TotalSize SoFar

instance Show DownloadProgress where
  show (DownloadProgress (TotalSize total) (SoFar soFar)) = showFFloat (Just 2) progress $ "%"
    where
      progress = (fromInteger soFar / fromInteger total) * 100

newtype TotalSize = TotalSize Integer
newtype SoFar = SoFar Integer

getTotalSize :: Arrow a => a (Response BodyReader, ByteString) (Maybe TotalSize, ByteString)
getTotalSize = arr (fmap TotalSize . readMay . unpack . decodeUtf8 . concat . getResponseHeader "Content-Length") *** arr id

getProgress :: Monad m => PlanT (Maybe TotalSize, ByteString) (Maybe DownloadProgress, ByteString) m ()
getProgress = go 0
  where
    go n = do
      (totalSize, bs) <- await
      let chunkSize = toInteger $ length bs
          soFar = n + chunkSize
      yield (DownloadProgress <$> totalSize <*> pure (SoFar soFar), bs)
      go soFar

downloadHttp :: MonadResource m =>
  FilePath -> 
     ProcessA
       (Kleisli m) (Event (Request, Manager)) (Event (), Event ())
downloadHttp fp = makeRequest >>> sourceHttp_ >>> anytime getTotalSize >>> construct getProgress
  >>> (evMap fst >>> sink showItSink)
  &&& (evMap snd >>> sinkFile fp)

f :: ArrowApply a => ProcessA a (Event Integer) (Event Integer)
f = proc x ->
   do
     y <- accum 0 -< (+) <$> x
     returnA -< y <$ x

makeRequest :: MonadResource m => ProcessA (Kleisli m) (Event (Request, Manager)) (Event (ReleaseKey, Response BodyReader))
makeRequest = constructT kleisli0 $ do
  (req, mgr) <- await
  resrc <- lift $ allocate (responseOpen req mgr) (\conn -> (liftIO $ putStrLn "Closing connection...") >> responseClose conn)
  yield resrc

sourceHttp_ :: MonadResource m => ProcessA (Kleisli m) (Event (ReleaseKey, Response BodyReader)) (Event (Response BodyReader, ByteString))
sourceHttp_ = constructT kleisli0 go
  where
    go = do
      (key, res) <- await
      loop key res (responseBody res)
    loop key res bodyReader = do
      bs <- lift $ liftIO $ brRead $ bodyReader
      case null bs of
        True -> lift $ liftIO $ release key
        False -> do
          yield (res, bs)
          loop key res bodyReader

sinkFile :: (MonadResource m, IOData a) => FilePath -> ProcessA (Kleisli m) (Event a) (Event ())
sinkFile fp = constructT kleisli0 $ go
  where
    go = do
      (key, h) <- lift $ allocate (SIO.openFile fp SIO.WriteMode) (\h -> (liftIO $ putStrLn "Closing sink handle...") >> SIO.hClose h)
      loop key h
    loop key h = do
      x <- await
      case onull x of
        True -> lift $ liftIO $ release key
        False -> do
          lift . liftIO $ hPut h x
          loop key h

sourceFile :: (MonadResource m, IOData a) => ProcessA (Kleisli m) (Event FilePath) (Event a)
sourceFile = constructT kleisli0 go
  where
    go = do
      fp <- await
      (key, h) <- lift $ allocate (SIO.openFile fp SIO.ReadMode) (\h -> (liftIO $ putStrLn "Closing source handle...") >> SIO.hClose h)
      loop key h
    loop key h = do
      x <- lift $ liftIO $ hGetChunk h
      case onull x of
        True -> lift $ liftIO $ release key
        False -> do
          yield x
          loop key h

inputCommand :: (MonadIO m, Read a) => ProcessA (Kleisli m) (Event ()) (Event a)
inputCommand = constructT kleisli0 go
  where
    go = do
      _ <- await
      input <- lift $ liftIO getLine
      case input of
        "end" -> stop
        _ -> case readMay input of
          Nothing -> do
            lift $ liftIO $ putStrLn $ "invalid command: " <> input
            go
          Just x -> do
            yield x
            go

createRequest :: MonadThrow m => Manager -> ProcessA (Kleisli m) (Event String) (Event (Request, Manager))
createRequest mgr = constructT kleisli0 $ do
  url <- await
  req <- lift $ parseUrlThrow url
  yield (req, mgr)

data NothingYet = NothingYet
  deriving (Show, Eq)

instance Exception NothingYet

machine f = anytime (kleisli f)
