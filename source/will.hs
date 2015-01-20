import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative ((<*), (*>), (<*>), (<$>))
import Data.List
import Data.Int
import Data.Hashable
import System.Random
import System.Process
import System.IO.Unsafe -- :D

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
data ActionType = Keystroke | Text | Open | Run deriving Show
data ActionElement = Keys String
  | S String
  | VariableUse String
  | Repeat RepeatNumber [ActionElement] deriving Show
data RepeatNumber = RVariable String | RNumber Int deriving Show




---------
-- Parser
---------

parsing :: String -> Either String Program
parsing input = case parse program "(unknown)" input of
  Right program -> Right program
  Left problem -> Left $ "Parsing error:\n" ++ (show problem)


program :: Parser Program
program = do
  spaces
  set <- sepEndBy1 commandSet spaces
  eof
  return $ Program set

commandSet = do
  c <- context
  -- commands <- many1 command
  commands <- sepEndBy1 command eol
  return $ CommandSet c commands

context :: Parser Context
context = do
  c <- everywhere <|> programList
  punctuation ":"
  eol
  return c

everywhere = do
  string "Everywhere" <|> string "everywhere"
  return Global

programList = do
  string "in" <|> string "In"
  ws
  l <- list
  return $ In l
  where
  list = sepBy1 (many1 (noneOf ",:")) (punctuation ",")

command :: Parser Command
command = do
  string "saying"
  ws
  t <- trigger
  punctuation ":"
  a <- action
  ws
  return $ Command t a

action :: Parser Action
action = do
  actionType <- try typeKeystroke <|> typeText
  elements <- actions
  return $ Action actionType elements

actions = sepEndBy1 actionElement ws

typeText = do
  try (punctuation "types") <|> punctuation "type"
  return Text

typeKeystroke = do
  try (punctuation "presses") <|> punctuation "press"
  return Keystroke

--typeOpen = do
--  try (punctuation "opens") <|> punctuation "open"
--  return Open
--
--typeRun = do
--  try (punctuation "runs") <|> punctuation "run"
--  return Run

actionElement = try repetition
  <|> try text
  <|> variableUse

--  <|> try keystroke

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
  content <- many1 (noneOf "\"")
  char '"'
  return $ S content

variableUse = fmap VariableUse variableName


--Keystroke = Key | ML "-" Key | ML "-" Variable
--ML = M | M "-" ML
--M = "Command" | "Control" | "Shift" | "Option"
--Key = Char | Digit |
--  "DownArrow" | "LeftArrow" | "RightArrow" | "UpArrow" | "Delete" | "End" | "Esc" | 
--  "F1" | "F2" | "F3" | "F4" | "F5" | "F6" | "F7" | "F8" | "F9" | "F10" | "F11" | "F12" | 
--  "Home" | "PageDown" | "PageUp" | "Return" | "Space" | "Tab"
--


-- modifier =  try (string "Command")
--   <|> try (string "Control")
--   <|> try (string "Shift")
--   <|> string "Option"
-- 
-- keystroke = try (string "DownArrow") 
--   <|> try (string "LeftArrow")
--   <|> try (string "RightArrow")
--   <|> try (string "UpArrow")
--   <|> try (string "Delete")
--   <|> try (string "End")
--   <|> try (string "Esc")
--   <|> try (string "F1")
--   <|> try (string "F2")
--   <|> try (string "F3")
--   <|> try (string "F4")
--   <|> try (string "F5")
--   <|> try (string "F6")
--   <|> try (string "F7")
--   <|> try (string "F8")
--   <|> try (string "F9")
--   <|> try (string "F10")
--   <|> try (string "F11")
--   <|> try (string "F12")
--   <|> try (string "Home")
--   <|> try (string "PageDown")
--   <|> try (string "PageUp")
--   <|> try (string "Return")
--   <|> try (string "Space")
--   <|> string "Tab"

trigger :: Parser Trigger
trigger = fmap Trigger (sepBy1 triggerElement ws)

triggerElement = try variableDeclaration
  <|> fmap Word (many1 letter)
  -- <|> try optionalWord 

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
-- Semantic analysis
---------

semanticAnalysis :: Program -> Either String Program
semanticAnalysis program = isVariableDeclared program >>= rangeValidation

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

rangeValidation program @ (Program commandSetList) =
  let errorList = do
        CommandSet _ commands <- commandSetList
        Command (Trigger triggerElements) _ <- commands
        Variable name (Range begin end) <- triggerElements
        guard $ begin >= end
        return $ "Variable '" ++ name ++ "' with inconsistent range"
  in reportErrors program errorList

reportErrors program [] = Right program
reportErrors program errorList = Left $ intercalate "\n" errorList


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

variableUnrolling :: Program -> Either String Program
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

loopUnrolling :: Program -> Either String Program
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
-- Trigger and action contraction
---------

triggerAndActionContraction (Program commandSetList) = Right $ Program (do
  CommandSet context commands <- commandSetList
  return $ CommandSet context [Command (triggerContraction trigger) (actionContraction action) | Command trigger action <- commands])

triggerContraction (Trigger list) = Trigger [Word $ intercalate " " [word | Word word <- list]]

actionContraction (Action t elements) = case t of
  Keystroke -> Action Keystroke [S $ trim words]
  _ -> Action t [S words]
  where words = intercalate "" [word | S word <- elements]

trim = f . f where f = reverse . dropWhile  (== ' ')


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

contextNormalization :: Program -> Either String Program
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
groupCommandsByContext = (map $ foldl1 combine) . (groupBy $ f (==)) . (sortBy $ f compare)
  where
  f g (CommandSet c1 _) (CommandSet c2 _) = g c1 c2
  combine (CommandSet context cmds1) (CommandSet _ cmds2) = CommandSet context (cmds1++cmds2)


---------
-- Code generation
---------

data Application = Application String Int deriving Show

data XMLFile = XMLFile FileName Content deriving Show
type FileName = String
type Content = String

codeGeneration :: Program -> Either String [XMLFile]
codeGeneration program @ (Program list) = case mapM generateCommandSet list of
  Right okay -> Right okay
  Left problem -> Left $ "Code generation error:\n" ++ problem ++ dumpAST program

generateCommandSet :: CommandSet -> Either String XMLFile
generateCommandSet (CommandSet context commands) = do
  c <- normalizeContext context
  return $ generateXMLFile (makeApplication c) commands
  where makeApplication c = do {name <- c; return $ Application (getBundleId name) 1} -- hardcoded version

normalizeContext :: Context -> Either String (Maybe String)
normalizeContext Global = Right Nothing
normalizeContext (In (name:[])) = Right $ Just name
normalizeContext context @ _ = Left $ "context should have been normalized: " ++ show context

generateXMLFile :: Maybe Application -> [Command] -> XMLFile
generateXMLFile app commands = case app of
  Nothing -> XMLFile ("global" ++ fileExtension) (xml app)
  Just (Application name _) -> XMLFile (name ++ fileExtension) (xml app)
  where xml app = fullXML $ generateCommandListXML app commands

-- Generate XML for an application and a command list
generateCommandListXML :: Maybe Application -> [Command] -> String
generateCommandListXML application commandList =
  foldl function "" (zip [0..] commandList)
  where
  function accumulator (index, command) =
    let z i = "z" ++ show (3*index + i)
        ids = (z 1, z 2, z 3)
        uniqueId = generateUniqueId application command
    in accumulator ++ (generateCommandXML application command ids uniqueId)

generateUniqueId :: Maybe Application -> Command -> Int32
generateUniqueId application command =
  let (Command (Trigger ((Word trigger):[])) _) = command
      applicationName =
        case application of
          Just (Application name _) -> name
          _ -> ""
      (uniqueId, _) = random (mkStdGen (hash (applicationName ++ trigger))) :: (Int32, StdGen)
  in uniqueId

generateCommandXML app (Command trigger action) (actionId, triggerId, commandId) uniqueId =
  let vendor = "igormoreno"
      triggerDescription = ""
      Trigger ((Word triggerContent):[]) = trigger
      Action actionType ((S actionContent):[]) = action
  in (commandXML app actionType vendor commandId actionId triggerId uniqueId) ++
     (triggerXML triggerContent triggerDescription triggerId commandId) ++
     (actionXML (xmlify actionContent) actionId commandId)

xmlify :: String -> String
xmlify string =
  concatMap replacer string
  where
  encodings = [('"', "&quot;"), ('&', "&amp;"), ('\'', "&apos;"), ('<', "&lt;"), ('>', "&gt;")] --, ('\n', "&#xD;&#xA;")]
  replacer x = case lookup x encodings of
    Just b -> b
    Nothing -> [x]

-- TODO: Add a nice error message in case things went wrong
-- TODO: What about the version
-- TODO: get rid of unsafeIO
getBundleId :: String -> String
getBundleId name =
  let path1 = unsafePerformIO $ readProcess "find" (["/Applications", "-name", name++".app"]) ""
      dropLast l = take (length l - 1) l
      path = dropLast path1
      output = unsafePerformIO $ readProcess "/usr/libexec/PlistBuddy" ["-c", "Print CFBundleIdentifier", path ++ "/Contents/Info.plist"] ""
  in dropLast output


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
  triggerAndActionContraction >>=
  codeGeneration

---------


dumpAST program = "\n\nAST\n" ++ show program


--test :: String -> Either ParseError Context
--test input = parse command "(unknown)" input
test input =
  parsing input >>=
  semanticAnalysis >>=
  variableUnrolling >>=
  loopUnrolling >>=
  contextNormalization >>=
  triggerAndActionContraction >>=
  codeGeneration

fileExtension = ".commandstext"

writingXMLFile :: XMLFile -> IO ()
writingXMLFile (XMLFile name content) = writeFile name content

main = do
  content <- getContents
  case compile content of
    Left e -> putStrLn e
    Right list -> mapM_ writingXMLFile list

