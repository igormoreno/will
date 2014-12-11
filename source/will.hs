import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import System.Random
import Data.List

data Program = Program [CommandSet] deriving Show
data CommandSet = CommandSet Context [Command] deriving Show

data Context = Global | In [String] deriving Show

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
	| Repetition Int Action deriving Show




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
	set <- many1 commandSet
	eof
	return $ Program set

commandSet = do
	c <- context
	commands <- many1 command
	--commands <- sepEndBy1 command (string "\n")
	return $ CommandSet c commands

context :: Parser Context
context = do
	c <- everywhere <|> programList
	punctuation ":"
	--eol
	return c

everywhere = do
	string "Everywhere" <|> string "everywhere"
	--spaces
	return Global

programList = do
	string "in" <|> string "In"
	spaces
	l <- list
	return $ In l
	where
	list = sepBy1 (many1 (noneOf ",:")) (punctuation ",")

command :: Parser Command
command = do
	string "saying"
	spaces
	t <- trigger
	punctuation ":"
	a <- action
	spaces
	return $ Command t a

action :: Parser Action
action = do
	actionType <- try typeKeystroke <|> try typeOpen <|> try typeRun
	--sepBy1 actionElement spaces
	element <- text
	return $ Action actionType [element]

typeKeystroke = do
	try (punctuation "types") <|> punctuation "type"
	return Text

typeOpen = do
	try (punctuation "opens") <|> punctuation "open"
	return Open

typeRun = do
	try (punctuation "runs") <|> punctuation "run"
	return Run


--actionElement = try repetition
--	<|> try text
--	<|> try keystroke
--	<|> variable
--
text = do
	char '"'
	content <- many1 (noneOf "\"")
	char '"'
	return $ S content

--Keystroke = Key | ML "-" Key | ML "-" Variable
--ML = M | M "-" ML
--M = "Command" | "Control" | "Shift" | "Option"
--Key = Char | Digit |
--	"DownArrow" | "LeftArrow" | "RightArrow" | "UpArrow" | "Delete" | "End" | "Esc" | 
--	"F1" | "F2" | "F3" | "F4" | "F5" | "F6" | "F7" | "F8" | "F9" | "F10" | "F11" | "F12" | 
--	"Home" | "PageDown" | "PageUp" | "Return" | "Space" | "Tab"
--


modifier =  try (string "Command")
	<|> try (string "Control")
	<|> try (string "Shift")
	<|> string "Option"

keystroke = try (string "DownArrow") 
	<|> try (string "LeftArrow")
	<|> try (string "RightArrow")
	<|> try (string "UpArrow")
	<|> try (string "Delete")
	<|> try (string "End")
	<|> try (string "Esc")
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
	<|> string "Tab"

trigger :: Parser Trigger
trigger = fmap Trigger (sepBy1 triggerElement spaces)

triggerElement = try variableDeclaration 
	<|> try optionalWord 
	<|> fmap Word (many1 letter)

optionalWord = do
	punctuation "["
	word <- many1 (noneOf "]")
	punctuation "]"
	return $ Optional word

variableName = many1 letter

variableDeclaration = do
	name <- variableName
	punctuation "="
	start <- many1 digit
	punctuation "-"
	end <- many1 digit
	return $ Variable name (Range (read start) (read end))




punctuation s = do
	spaces
	p <- string s
	spaces
	return p

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"




---------
-- Semantic analysis
---------

---------
-- Option expansion
---------

---------
-- Loop unrolling
---------

---------
-- Variable unrolling
---------

---------
-- Trigger and action contraction
---------

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

contextNormalization :: Program -> Program
contextNormalization (Program list) = Program $ (groupCommandsByContext . expandContext) list

expandContext :: [CommandSet] -> [CommandSet]
expandContext = concatMap expand1
	where
	expand1 c @ (CommandSet Global _) = [c] 
	expand1 c @ (CommandSet (In []) _) = [c] 
	expand1 c @ (CommandSet (In (name:[])) _) = [c] 
	expand1 c @ (CommandSet (In (name:names)) commands) = 
		(CommandSet (In (name:[])) commands):(expand1 $ CommandSet (In names) commands)

groupCommandsByContext :: [CommandSet] -> [CommandSet]
groupCommandsByContext = (map $ foldl1 combine) . (groupBy sameContext) . (sortBy compareContext)
	where
	compareContext (CommandSet (In (p:[])) _) (CommandSet (In (q:[])) _) = compare p q
	sameContext (CommandSet (In (p:[])) _) (CommandSet (In (q:[])) _) = p == q
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
generateCommandSet (CommandSet context commands) = generateXMLFile (getApplication context) commands

getApplication :: Context -> Either String (Maybe Application)
getApplication Global = Right $ Nothing
getApplication (In (name:[])) = Right $ Just (Application name 1)
getApplication context @ _ = Left $ "context should have been normalized: " ++ show context

generateXMLFile :: Either String (Maybe Application) -> [Command] -> Either String XMLFile
generateXMLFile a commands = do
	app <- a
	case app of
		Nothing -> return $ XMLFile ("global" ++ fileExtension) (xml app)
		Just (Application name _) -> return $ XMLFile (name ++ fileExtension) (xml app)
	where xml app = fullXML $ generateCommandList app commands

-- Generate XML for an application and a command list
generateCommandList :: Maybe Application -> [Command] -> String
generateCommandList application commandList =
	foldl function "" (zip [0..] commandList)
	where
	function accumulator (index, command) =
		let z i = "z" ++ show (3*index + i)
		    ids = (z 1, z 2, z 3)
		    (randomId, _) = random (mkStdGen 1) :: (Int, StdGen)  
		in (generateCommand application command ids randomId) ++ accumulator

generateCommand app (Command trigger action) (commandId, actionId, triggerId) randomId =
	let vendor = "igormoreno" 
	    triggerDescription = ""
	    Trigger ((Word triggerContent):[]) = trigger
	    Action actionType ((S actionContent):[]) = action
	in (commandXML app actionType vendor commandId actionId triggerId randomId) ++
	   (triggerXML triggerContent triggerDescription triggerId commandId) ++
	   (actionXML actionContent actionId commandId)


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

commandXML app commandType vendor commandId actionId triggerId randomId
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
  "  <attribute name=\"commandid\" type=\"int32\">" ++ show randomId ++ "</attribute>\n" ++
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

compile something = do
	result1 <- parsing something
	result2 <- return $ contextNormalization result1
	codeGeneration result2

---------


dumpAST program = "\n\nAST\n" ++ show program


--test :: String -> Either ParseError Context
test input = parse command "(unknown)" input

fileExtension = ".commandstext"

writingXMLFile :: XMLFile -> IO ()
writingXMLFile (XMLFile name content) = writeFile name content

main =
    do content <- getContents
       case compile content of
            Left e -> putStrLn e
            Right list -> mapM_ writingXMLFile list 
            --Right list -> sequence $ map writingXMLFile list 
            --Right list -> do map writingXMLFile list 
	    --		    return ()
            --Right list -> writingXMLFile (head list)

	
--    do c <- getContents
--       case parse program "(stdin)" c of
--            Left e -> do putStrLn "Error parsing input:"
--                         print e
--            Right r -> putStrLn $ show r

