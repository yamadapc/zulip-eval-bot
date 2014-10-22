{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar,
                           threadDelay)
import Control.Concurrent.Async
import Control.Exception (bracketOnError, evaluate)
import Control.Monad (unless, liftM, void, when)

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B (pack)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString.Internal as B (c2w)
import Data.Char (isSpace)
import Data.List (stripPrefix)

import GHC.IO.Handle (hClose, hGetContents, hFlush, hPutStr)
import GHC.IO.Handle.Types (Handle)

import System.Environment (getEnv)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..),
                       createProcess, proc, terminateProcess, waitForProcess)

import Web.HZulip

-- Entry point
--------------------------------------------------------------------------------

-- |
-- Main entry point.
main :: IO ()
main = do
    user <- getEnv "ZULIP_USER"
    key <- getEnv "ZULIP_KEY"

    zulip <- newZulip user key

    putStrLn "Listening for events"
    loggingA <- async $ onNewEvent zulip ["message", "subscriptions"] logEvent
    handlingMessagesA <- async $ onNewMessage zulip (handleMessage zulip)

    void $ waitAny [loggingA, handlingMessagesA]
    putStrLn "Bye! Something went wrong, this shouldn't have happened!"
    putStrLn "Report bugs to:"
    putStrLn "https://github.com/yamadapc/hzulip/issues"

-- Concurrency helpers
--------------------------------------------------------------------------------

-- |
-- Waits for a child to finish, but kills it after a timeout expires
waitUntilTimeout :: Int -> Async a -> IO a
waitUntilTimeout to c = forkIO (threadDelay to >> cancel c) >> wait c

forth :: (a, b, c, d) -> d
forth (_, _, _, x) = x

-- |
-- A patched version of readProcess which will correctly kill zombie PIDs
-- if an asynchronous exception is thrown.
readProcessWithCancellation :: FilePath -> [String] -> String -> IO String
readProcessWithCancellation cmd args input =
    bracketOnError (prepareProcess cmd args)
                   (terminateProcess . forth)
                   (consumeProcess input)

-- |
-- Takes some input, a process and its input/output handles created with
-- `createProcess`, feeds the process' input handle with the input and
-- consumes its output, returning it.
--
-- This is just the body of the standard `readProcess` function, with some
-- bits removed.
consumeProcess
  :: String ->
     (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) ->
     IO String
consumeProcess input (Just inh, Just outh, _, pid) = do
    -- Fork off a thread to consume the output
    outMVar <- newEmptyMVar
    output <- hGetContents outh
    forkIO $ evaluate (length output) >> putMVar outMVar ()

    -- Now write and flush any input
    unless (null input) $ hPutStr inh input >> hFlush inh

    -- Wait on the output to be consumed
    -- (I don't know what will happen if an exception hits here; the
    -- external process will be killed, which is the most crucial part, but
    -- I think this spark will be left leaked).
    takeMVar outMVar
    hClose outh

    -- On our implementation we don't care about errors. I'll fix this
    -- later :P
    _ <- waitForProcess pid
    return output

-- |
-- A helper wrapper around `createProcess`. Will return a tuple with open
-- input/output `Handle`s and a `ProcessHandle`
prepareProcess
  :: FilePath -> -- ^ command to run
     [String] -> -- ^ list of arguments
     IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
prepareProcess cmd args = createProcess (proc cmd args) { std_in = CreatePipe
                                                        , std_out = CreatePipe
                                                        , std_err = Inherit
                                                        }

-- Logger
--------------------------------------------------------------------------------

-- |
-- Logs events as they come in
logEvent :: EventCallback
logEvent (Event t i _) = putStrLn $ "New " ++ t ++
                                    " (ID: " ++ show i ++ ")'"

-- Bot structural helpers
--------------------------------------------------------------------------------

-- |
-- Handles a message event
handleMessage :: ZulipClient -> MessageCallback
handleMessage z msg = do
    let eml = userEmail $ messageSender msg
        cnt = messageContent msg

    print eml
    print cnt
    -- Don't execute commands we sent to ourselves
    unless (eml == clientEmail z) $ case stripPrefix "@eval " cnt of
        Nothing -> return ()
        Just input -> withAsync (handleEval input) $ \a -> do
            tid <- forkIO $ threadDelay 20000000 >> cancel a >>
                handleResult z msg "That's taking way too long... Good luck?"
            wait a >>= \r -> killThread tid >> handleResult z msg r

-- |
-- Handles the result of a command and sends it back the whoever sent it
handleResult :: ZulipClient -> Message -> String -> IO ()
handleResult _ _ "" = return ()
handleResult z msg result = void $ case messageDisplayRecipient msg of
    Left stream -> sendStreamMessage z stream (messageSubject msg) result
    Right users -> sendPrivateMessage z (map userEmail users) result

-- Eval Command
--------------------------------------------------------------------------------

type Language = String
type Code = String
type Expression = (Language, Code)

supportedLanguages :: [String]
supportedLanguages = [ "haskell", "javascript", "lisp", "go" ]

-- |
-- Handles `@eval` commands
handleEval :: String -> IO String
handleEval input = do print input
                      liftM unlines $ mapM (\e -> print e >> evaluateImpl e)
                                           (parseExpressions input)

-- |
-- Parses "expressions" out of messages
parseExpressions :: String -> [Expression]
parseExpressions input = case break isSpace input of
    ("markdown", md) -> parseMarkdown md
    expr -> [expr]

-- |
-- Parses "expressions" out of markdown code blocks
parseMarkdown :: String -> [Expression]
parseMarkdown md = case parseOnly parseMarkdownCodeBlocks (C.pack md) of
    Right expr -> expr
    Left _ -> [("", "")]

parseMarkdownCodeBlocks :: Parser [Expression]
parseMarkdownCodeBlocks = many' parseMarkdownCodeBlock

parseMarkdownCodeBlock :: Parser Expression
parseMarkdownCodeBlock = do
    _ <- manyTill anyWord8 "```"
    language <- many1 (notWord8 $ B.c2w '\n')
    _ <- word8 $ B.c2w '\n'
    code <- manyTill anyWord8 (string "```")
    return (C.unpack $ B.pack language, C.unpack $ B.pack code)

-- |
-- Evaluates an expression with the existing `external-evaluator` scripts
evaluateImpl :: Expression -> IO String
evaluateImpl ("", _) = return "Usage: @eval <language> <funny-expression>"
evaluateImpl (lg, _) | lg `notElem` supportedLanguages =
    return $ "I don't speak " ++ lg ++ " sorry..."

evaluateImpl ("lisp", _) = return "(this (is (not (yet (supported (sorry))))))"
evaluateImpl (lg, code) =
    readProcessWithCancellation ("./external-evaluators/" ++ lg) [code] ""
