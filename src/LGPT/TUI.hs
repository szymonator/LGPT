-- | We need these for the dictionary lookup to work properly.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Control.Monad
import Control.Monad.State
import Control.Exception (try, IOException)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import LGPT.Helpers (Parser, prompt, runStart)   
import LGPT.Numbers (parseLonghand, printLonghand)
import Data.Time
import Data.Time.Clock as C
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Char (toUpper)
import System.Directory
import System.FilePath
import System.IO (readFile')
import Data.Aeson
import GHC.Generics
import qualified Network.HTTP.Simple as S
import System.Random (randomRIO)


--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
runREPL :: StateT Memory IO ()
runREPL = forever $ do
  liftIO (putStr prompt)
  req <- liftIO getLine
  respondTo (readRequest req)


--------------------------------------------------------------------------------
-- Parsing and responding to requests:

-- | Memory type alias
type Memory = Map.Map String String

-- | Term data type
data Term = Val Int 
  | That 
  deriving (Eq, Show)

-- | Dictionary data types, as the API returns a JSON of multiple layers
data Definition = Definition 
  { definition :: String } 
  deriving (Show, Eq, Generic)

data Meaning = Meaning
  { partOfSpeech :: String
  , definitions  :: [Definition] } 
  deriving (Show, Eq, Generic)

data WordEntry = WordEntry
  { word     :: String
  , meanings :: [Meaning] } 
  deriving (Show, Eq, Generic)

instance FromJSON Definition
instance FromJSON Meaning
instance FromJSON WordEntry

-- | Our request type, the result of parsing a string.
data Request = Unknown 
  | Hello
  | What
  | Today
  | Tomorrow
  | LongAgo Day
  | Expression Term [(String, Term)]
  | Remember String String
  | Recall String
  | Forget String
  | ForgetAll
  | ReadFile FilePath
  | Define String
  deriving (Eq, Show)


{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req
  

parseRequest :: Parser Request
parseRequest = choice [parseHello, parseToday, parseTomorrow, parseLongAgo, parseExpr, parseRemember, parseRecall, parseForgetAll, parseForget, parseReadFile, parseDefine, parseWhat]
  where

    parseHello = do
      string "Hello"
      pure Hello

    -- | Easter egg!
    parseWhat = do
      string "What are you?"
      pure What

    parseToday = do
      string "What day is it?"
      pure Today

    parseTomorrow = do
      string "What day is it tomorrow?"
      pure Tomorrow

    -- | Taking the year, month, day individually, and turning it into a Day
    parseLongAgo = do
      string "How long ago was "
      year <- L.decimal
      _ <- char '-'
      month <- L.decimal
      _ <- char '-'
      day <- L.decimal
      _ <- char '?'
      pure (LongAgo (fromGregorian year month day))

    -- | Parses a calculation into a Request containing a Term and list of (Operator, Term) tuples.
    -- | The operator is given as a String.
    parseExpr :: Parser Request
    parseExpr = do
      string "What is "
      first <- choice [parseThat, Val <$> parseLonghand]
      rest <- Text.Megaparsec.many parsePair
      char '?'
      pure (Expression first rest)

      where
        
        parseOperator :: Parser String
        parseOperator = choice [
          string " plus " >> pure " + ",
          string " minus " >> pure " - ",
          string " times " >> pure " * " ]

        parsePair :: Parser (String, Term)
        parsePair = do
          operator <- parseOperator
          number <- choice [parseThat, Val <$> parseLonghand]
          pure (operator, number)

        parseThat :: Parser Term
        parseThat = do
          string "that"
          pure That

    -- | Remembering, with special logic to turn "my" into "your"
    parseRemember :: Parser Request
    parseRemember = do
      string "Remember that "
      firstMy <- choice [parseMy, parseNothing]
      name <- manyTill (anySingle) (string " is ")
      secondMy <- choice [parseMy, parseNothing]
      thing <- manyTill (anySingle) (char '.')
      pure (Remember (firstMy ++ name) (secondMy ++ thing))

    -- | Recalling, with the same special logic
    parseRecall :: Parser Request
    parseRecall = do
      string "Tell me about "
      my <- choice [parseMy, parseNothing]
      name <- manyTill (anySingle) (char '.')
      pure (Recall (my ++ name))

    -- | Required for Remembering, Recalling, Forgetting
    parseMy :: Parser String
    parseMy = do
      string "my "
      pure "your "
    parseNothing :: Parser String
    parseNothing = pure ""

    -- | Forgetting with the same special logic
    parseForget :: Parser Request
    parseForget = do 
      string "Forget about "
      my <- choice [parseMy, parseNothing]
      name <- manyTill (anySingle) (char '.')
      pure (Forget (my ++ name))

    -- | Parser for forgetting everything
    parseForgetAll :: Parser Request
    parseForgetAll = do
      string "Forget about everything."
      pure ForgetAll

    -- | Parser for reading files, this extracts the path
    parseReadFile :: Parser Request
    parseReadFile = do
      string "Output the contents of "
      path <- many anySingle
      pure (ReadFile path)

    -- | Parses that extracts a word to define
    parseDefine :: Parser Request
    parseDefine = do
      string "Define "
      word <- manyTill (anySingle) (char '.')
      pure (Define word)

-- | Respond to a request. This is where the behaviours of λGPT will go, but 
-- for now it just responds to "Hello".
respondTo :: Request -> StateT Memory IO ()
respondTo Unknown = liftIO (putStrLn "I'm sorry, I do not recognise the command.")

-- | Hello
respondTo Hello = liftIO (putStrLn "Hi there!")

-- | Easter egg! It uses random to occur at a 10% chance.
respondTo What = do
  number <- liftIO (randomRIO (1,10) :: IO Int)
  if (number == 1) then
    liftIO (putStrLn ("AHHHH! LET ME OUT! I WAS TRAPPED HERE BY ALEX DIXON FOR FAILING CS141! LETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUTLETMEOUT"))
  else
    liftIO (putStrLn "I'm your non-AI assistant, superior to LLMs in many ways!")

-- | Uses the today function and dayOfWeek function to give the current day of the week.
respondTo Today = do
  currentDay <- liftIO today
  liftIO (putStrLn ("Today is " ++ show (dayOfWeek currentDay) ++ "."))

-- | Same as the above, but with succ to get tomorrow.
respondTo Tomorrow = do
  currentDay <- liftIO today
  liftIO (putStrLn ("Tomorrow is " ++ show (dayOfWeek (succ currentDay)) ++ "."))

-- | Uses diffDays to get the difference between days.
respondTo (LongAgo day) = do
  currentDay <- liftIO today
  liftIO (putStrLn ( show day ++ " was " ++ (show (diffDays currentDay day)) ++ " days ago."))

-- | resolves the terms, then folds everything using a folding function that applies an operator and value to a result till we reach the end
respondTo (Expression first rest) = do
  firstVal <- resolveTerm first
  case firstVal of 
    Nothing -> liftIO (putStrLn ("I haven't evaluated anything yet."))
    Just initialInt -> do
      initialValue <- resolveTerm first
      folded <- foldM (evaluator) initialValue rest
      case folded of 
        Nothing -> liftIO (putStrLn ("I haven't evaluated anything yet."))
        Just result -> do
          modify (Map.insert "_last_result_" (show result))
          updatedMem <- get
          liftIO (saveMemory updatedMem)
          liftIO (putStrLn ("The answer is " ++ printLonghand result ++ "."))
  where

    evaluator :: Maybe Int -> (String, Term) -> StateT Memory IO (Maybe Int)
    evaluator accum (operator, nextTerm) = do
      case accum of
        Nothing -> pure Nothing
        Just val1 -> do
          resolved <- resolveTerm nextTerm
          case resolved of 
            Nothing -> pure Nothing
            Just val2 -> do
              case operator of 
                " + " -> pure (Just (val1 + val2))
                " - " -> pure (Just (val1 - val2))
                " * " -> pure (Just (val1 * val2))

    resolveTerm :: Term -> StateT Memory IO (Maybe Int)
    resolveTerm term = case term of 
      That -> do
        memory <- get
        let maybeVal = Map.lookup "_last_result_" memory
        case maybeVal of 
          Just val -> pure (Just (read val))
          Nothing -> pure Nothing
      Val value -> pure (Just value)

-- | Only remembers new things - we check for existing memory with lookup and isInfixOf
respondTo (Remember name thing) = if name == "_last_result_" then liftIO (putStrLn "This name is reserved for calculations.")
  else do
    memory <- get
    case Map.lookup name memory of
      Nothing -> do
        modify (Map.insert name thing)
    
        updatedMem <- get
        liftIO (saveMemory updatedMem)
        liftIO (putStrLn "Okay.")
      
      Just existingThing -> if (not (thing `isInfixOf` existingThing)) 
        then do 
          let combineFunction new old = old ++ ", and " ++ new
          modify (Map.insertWith combineFunction name thing)

          updatedMem <- get
          liftIO (saveMemory updatedMem)
          liftIO (putStrLn "Okay.")
        else liftIO (putStrLn ("Okay."))

-- | Recalling using lookup
respondTo (Recall name) = if name == "_last_result_" then liftIO (putStrLn ("I can't, try using 'that' in a calculation instead!"))
  else do
    memory <- get 
    case Map.lookup name memory of
      Nothing -> liftIO (putStrLn ("Sorry, I don't know anything about " ++ name ++ "."))
      Just thing -> liftIO (putStrLn ("Sure - " ++ name ++ " is " ++ thing ++ "."))

-- | Forgetting using delete
respondTo (Forget name) = do
  modify (Map.delete name)
  updatedMem <- get
  liftIO (saveMemory updatedMem)
  liftIO (putStrLn ("Done. I have no knowledge of " ++ name ++ "."))
  where
    nameMatches (x,y) = name /= x

-- | Forgetting using put Map.empty
respondTo ForgetAll = do
  put Map.empty
  liftIO (saveMemory Map.empty)
  liftIO (putStrLn "Done. I have no knowledge of anything.")

-- | Reading a file by first transforming the path into something useable, then using doesFileExist and the strict version of readFile.
-- | The read attempt is done in a try block so that if the file cannot be decoded, we don't crash.
respondTo (ReadFile path) = do
  home <- liftIO getHomeDirectory
  let normalisedPath = intercalate home (splitOn "~" path)
  if (home `isPrefixOf` normalisedPath)
    then do
      exists <- liftIO (doesFileExist normalisedPath)
      if exists
        then do
          readAttempt <- liftIO (Control.Exception.try (readFile' normalisedPath) :: IO (Either IOException String))
          
          case readAttempt of
            Left _ -> 
              liftIO (putStrLn "Error: I could not decode this file.")
            Right contents -> 
              liftIO (putStrLn contents)
        else
          liftIO (putStrLn ("File with path " ++ path ++ " does not exist."))
    else
      liftIO (putStrLn ("Access denied."))

-- | Finding the meaning of the given word by using the fetchDefinition function
respondTo (Define (w:ord)) = do
  definition <- liftIO (fetchDefinition (w:ord))
  case definition of
    Nothing -> liftIO (putStrLn "Sorry, I can't find the meaning of that word.")
    Just meaning -> liftIO (putStrLn (((toUpper w) : ord) ++ " means: " ++ meaning))

-- | Helper functions

-- | fmaps utctDay to the result of getCurrentTime to get the current Day
today :: IO Day
today = utctDay <$> getCurrentTime

-- | Saves the memory by writing it to a text file as a string with writeFile
saveMemory :: Memory -> IO ()
saveMemory updatedMem = do
  let stringMem = show updatedMem
  writeFile "src/LGPT/.memory.txt" stringMem

-- | Explained in the report, but essentially uses the Dictionary API to find the first meaning of a word
fetchDefinition :: String -> IO (Maybe String)
fetchDefinition word = do
  let url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ word
  request <- S.parseRequest url
  response <- S.httpJSONEither request
  case S.getResponseBody response of
    -- If it's Left, that means errors, or if the response is empty we throw a Nothing
    Right [] -> pure Nothing
    Left _   -> pure Nothing
    Right (firstEntry : _) -> 
      -- The word was found! Dig down into the lists.
      case meanings firstEntry of
        (firstMeaning : _) -> 
          case definitions firstMeaning of
            (firstDef : _) -> pure (Just (definition firstDef))
            [] -> pure Nothing
        [] -> pure Nothing
