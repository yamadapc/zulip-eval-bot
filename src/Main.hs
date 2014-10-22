{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Monad (unless, liftM, void)

import Data.Attoparsec.ByteString
import qualified Data.ByteString as B (pack)
import qualified Data.ByteString.Char8 as C (pack, unpack)
import qualified Data.ByteString.Internal as B (c2w)
import Data.Char (isSpace)
import Data.List (stripPrefix)

import System.Environment (getEnv)
import System.Process (readProcess)

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

-- Logger
--------------------------------------------------------------------------------

logEvent :: EventCallback
logEvent (Event t i _) = putStrLn $ "New " ++ t ++
                                    " (ID: " ++ show i ++ ")'"

-- Bot structural helpers
--------------------------------------------------------------------------------

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

handleEval :: String -> IO String
handleEval input = do print input
                      liftM unlines $ mapM (\e -> print e >> evaluateImpl e)
                                           (parseExpressions input)

parseExpressions :: String -> [Expression]
parseExpressions input = case break isSpace input of
    ("markdown", md) -> parseMarkdown md
    expr -> [expr]

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

evaluateImpl :: Expression -> IO String
evaluateImpl ("", _) = return "Usage: @eval <language> <funny-expression>"
evaluateImpl (lg, _) | lg `notElem` supportedLanguages =
    return $ "I don't speak " ++ lg ++ " sorry..."

evaluateImpl ("lisp", _) = return "(this (is (not (yet (supported (sorry))))))"
evaluateImpl (lg, code) = readProcess ("./external-evaluators/" ++ lg) [code] ""
