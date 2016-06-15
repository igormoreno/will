{-# LANGUAGE FlexibleContexts #-}

import Text.ParserCombinators.Parsec
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad (guard)
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Data.List
import Data.Int
import Data.Hashable
import System.Random
import System.Process
import System.Environment (getArgs, getProgName)
import System.IO.Unsafe -- :D
import Data.Function (on)

data Program = Program [CommandSet] deriving Show
data CommandSet = CommandSet Context [Command] deriving Show

data Context = Global | In [String] deriving (Show, Eq, Ord)

data Command = Command Trigger Action deriving Show

data Trigger = Trigger [TriggerElement] deriving Show
data TriggerElement = Word String
  | Optional String
  | Variable String Range deriving Show
data Range = Range Int Int deriving Show

data Action = Action ActionType [ActionElement] deriving Show
data ActionType = Keystroke | Text | Open | ShellScript deriving (Show, Eq)
data ActionElement = Keys String
  | S String
  | VariableUse String
  | Repeat RepeatNumber [ActionElement] deriving Show
data RepeatNumber = RVariable String | RNumber Int deriving Show




---------
-- Will parser
---------

parsing :: String -> Either Error Program
parsing input = case parse program "Will" (stripComments input) of
  Right program -> Right program
  Left problem -> Left $ "Parsing error:\n" ++ (show problem)

{-
line 1
line 2#comment
line 3
=>
line 1
line 2
line 3
-}
stripComments [] = []
stripComments code = strip '#' '\n' code
  where
  strip start finish code =
    let (p1, p2) = span (/= start) code
        (p3, p4) = span (/= finish) p2
    in p1 ++ stripComments p4

program :: Parser Program
program = do
  spaces
  set <- many1 commandSet
  eof
  return $ Program set

commandSet = do
  c <- context
  commands <- many1 command
  return $ CommandSet c commands

context :: Parser Context
context = do
  c <- everywhere <|> programList
  punctuation ":"
  spaces
  return c

everywhere = do
  string "Everywhere" <|> string "everywhere"
  return Global

programList = do
  string "In" <|> string "in"
  ws
  In <$> list
  where
  list = sepBy1 (many1 (noneOf ",:")) (punctuation ",")

command :: Parser Command
command = do
  string "saying"
  ws
  t <- trigger
  punctuation ":"
  a <- action
  spaces
  return $ Command t a

action :: Parser Action
action = do
  actionType <- try typeKeystroke <|> try typeText <|> typeShellScript
  elements <- actions
  return $ Action actionType elements

actions = sepEndBy1 actionElement ws

typeText = do
  try (punctuation "types") <|> punctuation "type"
  return Text

typeKeystroke = do
  try (punctuation "presses") <|> punctuation "press"
  return Keystroke

typeShellScript = do
  try (punctuation "runs") <|> punctuation "run"
  return ShellScript

--typeOpen = do
--  try (punctuation "opens") <|> punctuation "open"
--  return Open

actionElement = try repetition
  <|> try text
  <|> variableUse

repetition = do
  string "repeat"
  ws
  number <- repeatWithVariable <|> repeatWithNumber
  punctuation "("
  list <- actions
  punctuation ")"
  return $ Repeat number list

repeatWithNumber = do
  number <- many1 digit
  return $ RNumber (read number)

repeatWithVariable = fmap RVariable variableName

text = do
  char '"'
  content <- many1 chars
  char '"'
  return $ S content
  where
    chars = escaped <|> noneOf "\""
    escaped = char '\\' >> choice (zipWith escapedChar codes replacements)
    escapedChar code replacement = char code >> return replacement
    codes        = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
    replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']


variableUse = fmap VariableUse variableName


trigger :: Parser Trigger
trigger = fmap Trigger (sepBy1 triggerElement ws)

triggerElement = try variableDeclaration
  <|> triggerWord
  -- <|> try optionalWord 

triggerWord = fmap Word $ many1 (letter <|> char '/' <|> char '!')

--optionalWord = do
--  punctuation "["
--  word <- many1 (noneOf "]")
--  punctuation "]"
--  return $ Optional word

variableName :: Parser String
variableName =  (:) <$> letter <*> many (letter <|> digit <|> char '_')

variableDeclaration = do
  name <- variableName
  punctuation "="
  start <- many1 digit
  punctuation "-"
  end <- many1 digit
  return $ Variable name (Range (read start) (read end))


punctuation s = do
  ws
  p <- string s
  ws
  return p

ws :: Parser String
ws = many $ (char ' ' <|> char '\t')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"



---------
-- Keystroke parser
---------

-- Keystroke AST
data KeystrokeList = KeystrokeList [Keystroke]
data Keystroke = K [ModifierKey] Key
type ModifierKey = String
type Key = String

instance Show KeystrokeList where
  show (KeystrokeList keystrokes) = intercalate " " (map show keystrokes)

instance Show Keystroke where
  show (K modifiers key) = (concatMap (\s -> s ++ "-") modifiers) ++ key


parseKeystroke :: String -> Either Error KeystrokeList
parseKeystroke input = case parse keystrokeList "Keystroke" input of
  Right keystrokes -> Right keystrokes
  Left problem -> Left $  (show problem)

keystrokeList :: Parser KeystrokeList
keystrokeList = do
  ws
  keystrokes <- sepEndBy1 keystroke ws
  eof
  return $ KeystrokeList keystrokes

keystroke :: Parser Keystroke
keystroke = do
  modifiers <- many modifier
  k <- key
  return $ K modifiers k

modifier = do
  m <- modifierKey
  char '-'
  return m

modifierKey = try (string "Command")
  <|> try (string "Control")
  <|> try (string "Shift")
  <|> string "Option"

key = try (string "DownArrow")
  <|> try (string "LeftArrow")
  <|> try (string "RightArrow")
  <|> try (string "UpArrow")
  <|> try (string "Delete")
  <|> try (string "End")
  <|> try (string "Escape")
  <|> try (string "F1")
  <|> try (string "F2")
  <|> try (string "F3")
  <|> try (string "F4")
  <|> try (string "F5")
  <|> try (string "F6")
  <|> try (string "F7")
  <|> try (string "F8")
  <|> try (string "F9")
  <|> try (string "F10")
  <|> try (string "F11")
  <|> try (string "F12")
  <|> try (string "Home")
  <|> try (string "PageDown")
  <|> try (string "PageUp")
  <|> try (string "Return")
  <|> try (string "Space")
  <|> try (string "Tab")
  <|> normalKey

normalKey = do
  k <- lower <|> digit
       <|> char '`'
       <|> char ','
       <|> char '.'
       <|> char '/'
       <|> char '!'
       <|> char '~'
       <|> char '+'
       <|> char '*'
       <|> char ':'
  return [k]


---------
-- Semantic analysis
---------

semanticAnalysis :: Program -> Either Error Program
semanticAnalysis program = isVariableDeclared program >>= rangeValidation

isVariableDeclared :: Program -> Either Error Program
isVariableDeclared program @ (Program commandSetList) =
  let errorList = do
        CommandSet _ commands <- commandSetList
        Command (Trigger triggerElements) (Action _ actionElements) <- commands
        name <- allVariables actionElements
        guard $ name `notElem` [name | Variable name _ <- triggerElements]
        return $ "Variable '" ++ name ++ "' not defined"
  in reportErrors program errorList

allVariables :: [ActionElement] -> [String]
allVariables actionElements =
  [name | VariableUse name <- actionElements] ++
  [name | Repeat (RVariable name) _ <- actionElements] ++
  concat [allVariables elements | Repeat _ elements <- actionElements]

rangeValidation :: Program -> Either Error Program
rangeValidation program @ (Program commandSetList) =
  let errorList = do
        CommandSet _ commands <- commandSetList
        Command (Trigger triggerElements) _ <- commands
        Variable name (Range begin end) <- triggerElements
        guard $ begin >= end
        return $ "Variable '" ++ name ++ "' with inconsistent range"
  in reportErrors program errorList

---------
-- Option expansion
---------


---------
-- Variable unrolling
---------

{-
The AST of the program

in Firefox:
saying down number=1-3: types repeat number (DownArrow)
command without variables

 will be turned into =>

in Firefox:
saying down 1: types repeat 1 (DownArrow)
saying down 2: types repeat 2 (DownArrow)
saying down 3: types repeat 3 (DownArrow)
command without variables
-}

variableUnrolling :: Program -> Either Error Program
variableUnrolling (Program commandSetList) = Right $ Program (do
  CommandSet context commands <- commandSetList
  return $ CommandSet context (concatMap expandCommand commands))

expandCommand :: Command -> [Command]
expandCommand command @ (Command (Trigger triggerElements) _) =
  replaceCommand [variable | variable @ (Variable _ _) <- triggerElements] command

replaceCommand :: [TriggerElement] -> Command -> [Command]
replaceCommand [] command = [command]
replaceCommand ((Variable name (Range begin end)):variables) command =
  concatMap (replaceCommand variables) [replaceVariable name number command | number <- [begin..end]]


replaceVariable name number (Command trigger action) =
   Command (replaceTrigger trigger) (replaceAction action)
   where
   replaceTrigger (Trigger elements) = Trigger (map replaceTriggerElement elements)
   replaceAction (Action t elements) = Action t (map replaceActionElement elements)
   replaceTriggerElement element = case element of
     (Variable name2 _) -> if name == name2 then Word (show number) else element
     _ -> element
   replaceActionElement element = case element of
     (VariableUse name2) ->
       if name == name2 then S (show number) else element
     (Repeat (RNumber x) elements) ->
       Repeat (RNumber x) (map replaceActionElement elements)
     (Repeat (RVariable name2) elements) ->
       if name == name2
       then Repeat (RNumber number) (map replaceActionElement elements)
       else Repeat (RVariable name2) (map replaceActionElement elements)
     _ -> element


---------
-- Loop unrolling
---------

{-
The AST of the program

in Firefox:
saying down 1: types repeat 1 (DownArrow)
saying down 2: types repeat 2 (DownArrow)
saying down 3: types repeat 3 (DownArrow)
command without variables

 will be turned into =>

in Firefox:
saying down 1: types DownArrow
saying down 2: types DownArrow DownArrow
saying down 3: types DownArrow DownArrow DownArrow
command without variables
-}

loopUnrolling :: Program -> Either Error Program
loopUnrolling (Program commandSetList) = Right $ Program (do
  CommandSet context commands <- commandSetList
  return $ CommandSet context (map expand commands))
  where
  expand (Command trigger (Action actionType actionElements)) =
    Command trigger (Action actionType (concatMap expandRepeat actionElements))

expandRepeat :: ActionElement -> [ActionElement]
expandRepeat (Repeat (RNumber number) elements) =
  concat (replicate number (concatMap expandRepeat elements))
expandRepeat element  = [element]


---------
-- Context normalization
---------

{-
The AST of the program

in Firefox, Google Chrome:
command 1
command 2

in Firefox:
command 3

 will be turned into =>

in Firefox:
command 1
command 2
command 3

in Google Chrome:
command 1
command 2
-}

contextNormalization :: Program -> Either Error Program
contextNormalization (Program list) = (Right . Program . groupCommandsByContext . expandContext) list

expandContext :: [CommandSet] -> [CommandSet]
expandContext = concatMap expand1
  where
  expand1 c @ (CommandSet Global _) = [c]
  expand1 c @ (CommandSet (In []) _) = [c]
  expand1 c @ (CommandSet (In (name:[])) _) = [c]
  expand1 c @ (CommandSet (In (name:names)) commands) =
    (CommandSet (In (name:[])) commands):(expand1 $ CommandSet (In names) commands)

groupCommandsByContext :: [CommandSet] -> [CommandSet]
groupCommandsByContext = (map $ foldl1 combine) . (groupBy ((==) `on` f)) . (sortBy (compare `on` f))
  where
  f (CommandSet c _) = c
  combine (CommandSet context cmds1) (CommandSet _ cmds2) = CommandSet context (cmds1++cmds2)


---------
-- To low IR
---------

data LowIR = LowIR [LowCommandSet] deriving Show
data LowCommandSet = LowCommandSet LowContext [LowCommand] deriving Show

type LowContext = Maybe Application
data Application = Application {
  applicationName :: String,
  applicationVersion :: Int} deriving Show

data LowCommand = LowCommand {
  commandId :: String,
  uniqueId :: Int32,
  commandTrigger :: LowTrigger,
  commandAction :: LowAction} deriving Show --(Show, Eq, Ord)

data LowTrigger = LowTrigger {
  triggerId :: String,
  triggerContent :: String} deriving Show

data LowAction = LowAction {
  actionId :: String,
  actionType :: ActionType,
  actionContent :: String} deriving Show

showApplication :: String -> Maybe Application -> String
showApplication global (Just (Application name _)) = name
showApplication global _ = global


lowIR :: Program -> Either Error LowIR
lowIR (Program commandSetList) = LowIR <$> mapM lowCommandSet commandSetList

lowCommandSet :: CommandSet -> Either Error LowCommandSet
lowCommandSet (CommandSet context commands) = do
  newContext <- lowContext context
  let newCommands = zipWith (lowCommand newContext) [0..] commands
  return $ LowCommandSet newContext newCommands

lowContext :: Context -> Either Error LowContext
lowContext context = makeApplication <$> normalizeContext context
  where
  makeApplication :: (Maybe String) -> (Maybe Application)
  makeApplication c = do {name <- c; return $ Application name 1} -- hardcoded version

  normalizeContext :: Context -> Either Error (Maybe String)
  normalizeContext Global = Right Nothing
  normalizeContext (In (name:[])) = Right $ Just name
  normalizeContext context @ _ = Left $ "context should have been normalized: " ++ show context

lowCommand :: LowContext -> Int -> Command -> LowCommand
lowCommand context index (Command trigger action) = LowCommand {
  commandId = cId,
  uniqueId = uId,
  commandTrigger = newTrigger,
  commandAction = lowAction aId action}
  where
    newTrigger = lowTrigger tId trigger
    z i = "z" ++ show (3*index + i)
    (aId, tId, cId) = (z 1, z 2, z 3)
    uId = 0

lowTrigger id (Trigger list) = LowTrigger {
  triggerId = id,
  triggerContent = intercalate " " [word | Word word <- list]}

lowAction id (Action t elements) = LowAction {
  actionId = id,
  actionType = t,
  actionContent = actionContraction}
  where
  actionContraction = case t of
    Keystroke -> trim words
    ShellScript -> "#!/bin/bash\n" ++ words
    _ -> words
    where words = intercalate "" [word | S word <- elements]

trim = f . f where f = reverse . dropWhile  (== ' ')


---------
-- Generate unique ID
---------

generateUniqueId :: LowIR -> Either Error LowIR
generateUniqueId (LowIR commandSetList) = (Right . LowIR) (do
  LowCommandSet context commands <- commandSetList
  return $ LowCommandSet context (map (g context) commands))
  where
  g context command = updateUniqueId newId command
    where newId = makeUniqueId $ generateSeed (commandTrigger command) context

updateUniqueId id command = LowCommand {
  commandId = commandId command,
  uniqueId = id,
  commandTrigger = commandTrigger command,
  commandAction = commandAction command}

generateSeed :: LowTrigger -> LowContext -> String
generateSeed lowTrigger context = applicationName ++ (triggerContent lowTrigger)
  where applicationName = showApplication "" context

makeUniqueId :: String -> Int32
makeUniqueId string = uId
  where (uId, _) = random (mkStdGen (hash string)) :: (Int32, StdGen)


---------
-- Check same ID
---------

checkSameCommand :: LowIR -> Either Error LowIR
checkSameCommand lowIR @ (LowIR commandSetList) = reportErrors lowIR (concatMap g commandSetList)
  where
  g :: LowCommandSet -> [Error]
  g (LowCommandSet c commands) = map (error c) $ findRepeated (triggerContent . commandTrigger) commands
  error context command = "Multiple commands with trigger \"" ++ triggerContent (commandTrigger command) ++ "\" found in context " ++ showApplication "Everywhere" context

checkSameId :: LowIR -> Either Error LowIR
checkSameId (LowIR commandSetList) = (Right . LowIR) (do
  LowCommandSet context commands <- commandSetList
  return $ LowCommandSet context (map (g context) commands))
  where
  allCommands = concat [commands | LowCommandSet _ commands <- commandSetList]
  sortedCommands = sortBy (compare `on` uniqueId) allCommands
  repeatedCommands = findRepeated uniqueId allCommands

  g context command =
    if isPartOf command repeatedCommands
    then loop command (generateSeed (commandTrigger command) context)
    else command
    where
    loop command oldSeed =
      let newSeed = oldSeed ++ show (makeUniqueId oldSeed)
          newCommand = updateUniqueId (makeUniqueId newSeed) command
      in if isPartOf newCommand sortedCommands
         then loop newCommand newSeed
         else newCommand

isPartOf :: LowCommand -> [LowCommand] -> Bool
isPartOf command list = case find (((==) `on` uniqueId) command) list of
  Nothing -> False
  _ -> True

-- Find same things. Two things a and b are the same iff f a == f b
findRepeated :: (Eq a, Ord a) => (b -> a) -> [b] -> [b]
findRepeated f = (map head) . (filter $ \c -> (length c) > 1) . (groupBy ((==) `on` f)) . (sortBy (compare `on` f))


---------
-- Check keystroke language
---------

checkKeystrokeLanguage :: LowIR -> Either Error LowIR
checkKeystrokeLanguage (LowIR commandSetList) = LowIR <$> (compileErrors $ map g commandSetList)
  where
  g :: LowCommandSet -> Either Error LowCommandSet
  g (LowCommandSet context commands) =
    LowCommandSet context <$> (compileErrors $ map check commands)

  check :: LowCommand -> Either Error LowCommand
  check command = case actionType (commandAction command) of
    Keystroke -> case parseKeystroke content of
      Right keystrokes -> Right $ update command (show keystrokes)
      Left error -> Left $ "Incorrect keystroke format in \"" ++ content ++ "\" (triggered by \"" ++ triggerContent (commandTrigger command) ++ "\")\n" ++ error
    _ -> Right command
    where
    content = actionContent (commandAction command)

    update (LowCommand {commandId = cId, uniqueId = uId, commandTrigger = cTrigger, commandAction =
      LowAction {actionId = aId, actionType = aType, actionContent = _}}) keystrokes =
            LowCommand {commandId = cId, uniqueId = uId, commandTrigger = cTrigger, commandAction =
              LowAction {actionId = aId, actionType = aType, actionContent = keystrokes}}



---------
-- Context internal name
---------

contextInternalName :: LowIR -> Either Error LowIR
contextInternalName (LowIR commandSetList) = LowIR <$> mapM g commandSetList
  where
  g :: LowCommandSet -> Either Error LowCommandSet
  g (LowCommandSet context commands) = do
    newContext <- mapM f context
    return $ LowCommandSet newContext commands

  f :: Application -> Either Error Application
  f (Application name version) = Application <$> getBundleId name <*> return version


-- TODO: What about the version
-- TODO: get rid of unsafeIO
getBundleId :: String -> Either Error String
getBundleId name = unsafePerformIO (do
  paths <- findCommand (name ++ ".app") "/Applications"
  case paths of
    [] -> return $ Left $ "application '" ++ name ++ "' not found"
    [path] -> Right <$> bundleId path
    paths -> return $ Left $ "multiple applications with name '" ++ name ++ "' found:\n" ++ unlines paths)
  where
  bundleId :: String -> IO String
  bundleId path =
    dropLast <$> readProcess "/usr/libexec/PlistBuddy" ["-c", "Print CFBundleIdentifier", path ++ "/Contents/Info.plist"] ""
    where
    dropLast l = take (length l - 1) l

findCommand :: String -> String -> IO [String]
findCommand what folder = lines <$> readProcess "find" [folder, "-name", what] ""


---------
-- Code generation
---------

data XMLFile = XMLFile FileName Content deriving Show
type FileName = String
type Content = String

codeGeneration :: LowIR -> Either Error [XMLFile]
codeGeneration (LowIR list) = Right $ map generateXMLFile list

generateXMLFile :: LowCommandSet -> XMLFile
generateXMLFile (LowCommandSet context commands) = XMLFile fileName xml
  where
  fileName = (showApplication "global" context) ++ fileExtension
  xml = fullXML $ concatMap (generateCommandXML context) commands

generateCommandXML app (LowCommand {
  commandId = cid,
  uniqueId = uid,
  commandTrigger = (LowTrigger {
    triggerId = tid,
    triggerContent = tcontent}),
  commandAction = (LowAction {
    actionId = aid,
    actionType = atype,
    actionContent = acontent})}) =
  let vendor = "igormoreno"
      triggerDescription = ""
  in (commandXML app atype vendor cid aid tid uid) ++
     (triggerXML tcontent triggerDescription tid cid) ++
     (actionXML (xmlify acontent) aid cid)

xmlify :: String -> String
xmlify = replaceMultiple encodings
  where
  encodings = [("&", "&amp;"), ("<", "&lt;"), (">", "&gt;")] --, ("\\n", "\n")] --, ('\n', "&#xD;&#xA;")]

  replaceMultiple [] target = target
  replaceMultiple ((p, q):rest) target = replaceMultiple rest (replace p q target)

  -- replace all occurrences of "search" by "substitute" in "list"
  replace search substitute [] = []
  replace search substitute list @ (x:rest) =
    if isPrefixOf search list
    then substitute ++ (replace search substitute $ drop (length search) list)
    else x:(replace search substitute rest)



---------
-- XML
---------

--famousQuote = [str|Any dictator would admire the
--                  |uniformity and obedience of the U.S. media.
--                  |
--                  |    -- Noam Chomsky
--                  |]

fullXML body = "<database>\n" ++
      "  <databaseInfo>\n" ++
      "    <version>134481920</version>\n" ++
      "    <UUID>C6588137-5AC4-4FA0-ACA4-818FBA19D3AB</UUID>\n" ++
      "    <nextObjectID>113</nextObjectID>\n" ++
      "    <metadata>\n" ++
      "      <plist version=\"1.0\">\n" ++
      "        <dict>\n" ++
      "          <key>NSPersistenceFrameworkVersion</key>\n" ++
      "          <integer>407</integer>\n" ++
      "          <key>NSStoreModelVersionHashes</key>\n" ++
      "          <dict>\n" ++
      "            <key>action</key>\n" ++
      "            <data>Gl79yicU/qMmmjW+02T6r/N/3wY/MXt1/ETG6BgiQvk=</data>\n" ++
      "            <key>command</key>\n" ++
      "            <data>LnTEAxQizumf4LEx7vWu/AEuw8pYLvlU+A+QAxxNV1Q=</data>\n" ++
      "            <key>location</key>\n" ++
      "            <data>l1GW8zsQs6xToCTE303HdInkm0pvem69Qmej6Ixq3k4=</data>\n" ++
      "            <key>trigger</key>\n" ++
      "            <data>kWwewq0GT8KPB4ELML1wT0S2IYIZ5+/6CI0GsK9LDns=</data>\n" ++
      "          </dict>\n" ++
      "          <key>NSStoreModelVersionHashesVersion</key>\n" ++
      "          <integer>3</integer>\n" ++
      "          <key>NSStoreModelVersionIdentifiers</key>\n" ++
      "          <array>\n" ++
      "            <string></string>\n" ++
      "          </array>\n" ++
      "        </dict>\n" ++
      "      </plist>\n" ++
      "    </metadata>\n" ++
      "  </databaseInfo>\n" ++
         body ++
      "</database>\n"

commandXML app commandType vendor commandId actionId triggerId uniqueId
  = "<object type=\"COMMAND\" id=" ++ show commandId ++ ">\n" ++
  "  <attribute name=\"version\" type=\"int32\">1</attribute>\n" ++
  "  <attribute name=\"vendor\" type=\"string\">" ++ vendor ++"</attribute>\n" ++
  "  <attribute name=\"type\" type=\"string\">" ++ show commandType ++ "</attribute>\n" ++
  "  <attribute name=\"spokenlanguage\" type=\"string\">en_US</attribute>\n" ++
  "  <attribute name=\"oslanguage\" type=\"string\">en_GB</attribute>\n" ++
  "  <attribute name=\"isspelling\" type=\"bool\">0</attribute>\n" ++
  "  <attribute name=\"issleep\" type=\"bool\">0</attribute>\n" ++
  "  <attribute name=\"isdictation\" type=\"bool\">0</attribute>\n" ++
  "  <attribute name=\"iscorrection\" type=\"bool\">0</attribute>\n" ++
  "  <attribute name=\"iscommand\" type=\"bool\">1</attribute>\n" ++
  "  <attribute name=\"engineid\" type=\"int32\">-1</attribute>\n" ++
  "  <attribute name=\"display\" type=\"bool\">1</attribute>\n" ++
  "  <attribute name=\"commandid\" type=\"int32\">" ++ show uniqueId ++ "</attribute>\n" ++
  printApplication app ++
  "  <attribute name=\"active\" type=\"bool\">1</attribute>\n" ++
  "  <relationship name=\"currentaction\" type=\"1/1\" destination=\"ACTION\"></relationship>\n" ++
  "  <relationship name=\"currenttrigger\" type=\"1/1\" destination=\"TRIGGER\"></relationship>\n" ++
  "  <relationship name=\"location\" type=\"1/1\" destination=\"LOCATION\"></relationship>\n" ++
  "  <relationship name=\"action\" type=\"0/0\" destination=\"ACTION\" idrefs=" ++ show actionId ++ "></relationship>\n" ++
  "  <relationship name=\"trigger\" type=\"1/0\" destination=\"TRIGGER\" idrefs=" ++ show triggerId ++ "></relationship>\n" ++
  "</object>\n" 
  where
  printApplication (Just (Application name version)) = "  <attribute name=\"appversion\" type=\"int32\">" ++ show version ++ "</attribute>\n" ++ "  <attribute name=\"appbundle\" type=\"string\">" ++ name ++ "</attribute>\n"
  printApplication (Nothing) = "  <attribute name=\"appversion\" type=\"int32\">0</attribute>\n"

triggerXML triggerContent triggerDescription triggerId commandId
  ="<object type=\"TRIGGER\" id=" ++ show triggerId ++ ">\n" ++
  "  <attribute name=\"string\" type=\"string\">" ++ triggerContent ++"</attribute>\n" ++
  "  <attribute name=\"spokenlanguage\" type=\"string\">en_US</attribute>\n" ++
  "  <attribute name=\"isuser\" type=\"bool\">1</attribute>\n" ++
  "  <attribute name=\"desc\" type=\"string\">" ++ triggerDescription ++"</attribute>\n" ++
  "  <relationship name=\"command\" type=\"1/1\" destination=\"COMMAND\" idrefs=" ++ show commandId ++ "></relationship>\n" ++
  "  <relationship name=\"currentcommand\" type=\"1/1\" destination=\"COMMAND\"></relationship>\n" ++
  "</object>\n"

actionXML actionContent actionId commandId
  = "<object type=\"ACTION\" id="++ show actionId ++ ">\n" ++
  "  <attribute name=\"text\" type=\"string\">" ++ actionContent ++"</attribute>\n" ++
  "  <attribute name=\"oslanguage\" type=\"string\">en_GB</attribute>\n" ++
  "  <attribute name=\"isuser\" type=\"bool\">1</attribute>\n" ++
  "  <relationship name=\"command\" type=\"1/1\" destination=\"COMMAND\" idrefs=" ++ show commandId ++ "></relationship>\n" ++
  "  <relationship name=\"currentcommand\" type=\"1/1\" destination=\"COMMAND\"></relationship>\n" ++
  "</object>\n"



---------
-- compiler phases
---------

compile input =
  parsing input >>=
  semanticAnalysis >>=
  variableUnrolling >>=
  loopUnrolling >>=
  contextNormalization >>=
  lowIR >>=
  checkKeystrokeLanguage >>=
  contextInternalName >>=
  generateUniqueId >>=
  checkSameCommand >>=
  checkSameId >>=
  codeGeneration


---------
-- simple error handling

type Error = String

reportErrors :: a -> [Error] -> Either Error a
reportErrors a [] = Right a
reportErrors a errorList = Left $ intercalate "\n" errorList

compileErrors :: [Either String a] -> Either String [a]
compileErrors list = case [error | Left error <- list] of
  [] -> sequence list
  errors -> Left $ intercalate "\n" errors

---------


dumpAST program = "\n\nAST\n" ++ show program


--test :: String -> Either ParseError Context
--test input = parse command "(unknown)" input
--test input =
--  parsing input >>=
--  semanticAnalysis >>=
--  variableUnrolling >>=
--  loopUnrolling >>=
--  contextNormalization >>=
--  triggerAndActionContraction >>=
--  codeGeneration

fileExtension = ".commandstext"

writingXMLFile :: XMLFile -> IO ()
writingXMLFile (XMLFile name content) = writeFile name content

run wrappedContent = do
  content <- wrappedContent
  case compile content of
    Left e -> putStrLn e
    Right list -> mapM_ writingXMLFile list

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-":[] ->
      run getContents
    file:[] ->
      run $ readFile file
    _ -> do
      programName <- getProgName
      putStrLn $ "usage: " ++ programName ++ " (file.will | -)"

