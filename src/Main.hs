{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent as Concurrent
    (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Lifted (fork, threadDelay)
import qualified Control.Concurrent.Async as Async
    (Async(..), async, cancel, poll, wait)
import qualified Control.Concurrent.Async.Lifted as Async.Lifted
    (async, wait)
import Control.Exception (bracketOnError, evaluate, handle)
import Control.Monad ((>=>), unless, liftM, void, when)
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B (pack)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString.Internal as B (c2w)
import Data.Char (isSpace)
import qualified Data.Foldable as Foldable (forM_)
import Data.List (stripPrefix)
import Data.Maybe (isNothing)
import GHC.IO.Handle (hClose, hGetContents, hFlush, hPutStr)
import GHC.IO.Handle.Types (Handle)
import System.Environment (getEnv)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..),
                       createProcess, proc, terminateProcess, waitForProcess)

import Logging
import Web.HZulip

-- Entry point
--------------------------------------------------------------------------------

-- |
-- Main entry point.
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    user <- getEnv "ZULIP_USER"
    key <- getEnv "ZULIP_KEY"
    handle (logException >=> const main)
           (withZulipCreds user key startBot)

startBot :: ZulipM ()
startBot = do
    -- Subscribe to all avaiable streams
    streams <- getStreams
    addSubscriptions streams

    handlingMessagesA <- Async.Lifted.async $ onNewMessage handleMessage
    void $ lift $ Async.wait handlingMessagesA
    lift $ logError
        ( "Bye! Something went wrong, this shouldn't have happened!\n"
       ++ "Report bugs to:\n"
       ++ "https://github.com/yamadapc/zulip-eval-bot/issues"
        )

-- Concurrency helpers
--------------------------------------------------------------------------------

-- |
-- Waits for a child to finish, but kills it after a timeout expires
waitUntilTimeout :: ZulipM () -> Int -> Async.Async a -> ZulipM a
waitUntilTimeout onTo to c = do
    _ <- fork $ do
        threadDelay to
        done <- lift $ Async.poll c
        when (isNothing done) $ do
            lift $ Async.cancel c
            onTo
    lift $ Async.wait c

{-
:t fork

:info MonadBaseControl

:set prompt "> "

import Control.Monad.Trans.Reader

:info ReaderT
-}

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
consumeProcess :: String
               -> (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO String
consumeProcess input (Just inh, Just outh, _, pid) = do
    -- Fork off a thread to consume the output
    outMVar <- newEmptyMVar
    output <- hGetContents outh
    void $ forkIO $ evaluate (length output) >> putMVar outMVar ()

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
    void $ waitForProcess pid
    return output
consumeProcess _ _ = fail "There's a mistake in this code."

-- |
-- A helper wrapper around `createProcess`. Will return a tuple with open
-- input/output `Handle`s and a `ProcessHandle`
prepareProcess :: FilePath -- ^ command to run
               -> [String] -- ^ list of arguments
               -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
prepareProcess cmd args = createProcess (proc cmd args) { std_in = CreatePipe
                                                        , std_out = CreatePipe
                                                        , std_err = Inherit
                                                        }

-- Bot structural helpers
--------------------------------------------------------------------------------

-- |
-- Handles a message event
handleMessage :: MessageCallback
handleMessage msg = do
    let eml = userEmail $ messageSender msg
        cnt = messageContent msg
        mid = show $ messageId msg

    ceml <- clientEmail `fmap` ask

    lift $ logInfo $ "(" ++ mid ++ ") Message from " ++ eml
    -- Don't execute commands we sent to ourselves
    unless (eml == ceml) $ Foldable.forM_ (stripPrefix "@eval " cnt)
                                          handleEvalWithTO
  where handleEvalWithTO input = do
            r <- waitUntilTimeout
                    onTimeout
                    20000000 =<< lift (Async.async (handleEval input))
            handleResult msg r
        onTimeout = do
            lift $ logWarn $
                "(" ++ show (messageId msg) ++ ") Message timed-out"

            handleResult msg "That's taking way too long... Good luck?"


-- |
-- Handles the result of a command and sends it back the whoever sent it
handleResult :: Message -> String -> ZulipM ()
handleResult _ "" = return ()
handleResult msg result = do
    let r = messageDisplayRecipient msg
        mid = show $ messageId msg

    lift $ logInfo $
        "(" ++ mid ++ ") Sending \"" ++ result ++ "\" to " ++ show r

    void $ case r of
        Left stream -> sendStreamMessage stream (messageSubject msg) result
        Right users -> sendPrivateMessage (map userEmail users) result

-- Eval Command
--------------------------------------------------------------------------------

type Language = String
type Code = String
type Expression = (Language, Code)

supportedLanguages :: [String]
supportedLanguages = [ "haskell", "haskell-type", "javascript", "go" ]

-- |
-- Handles `@eval` commands
handleEval :: String -> IO String
handleEval input = do
    print input
    liftM unlines $ mapM
        (\e -> print e >> evaluateImpl e)
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
evaluateImpl (t, _) | t == "" || t == "help" =
    return $ "Usage: @eval <language> <funny-expression>\n" ++
             "Currently supported languages are:\n" ++
             foldr (\l m -> show l ++ ", " ++ m) "" supportedLanguages
evaluateImpl (lg, _) | lg `notElem` supportedLanguages =
    return $ "I don't speak " ++ lg ++ " sorry..."
evaluateImpl (lg, code) =
    readProcessWithCancellation ("./external-evaluators/" ++ lg) [code] ""
