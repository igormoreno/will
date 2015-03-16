Language specification
======================

Program example
---------------

everywhere:
saying page down: press "PageDown"
saying page number = 2-5: press repeat number ("PageDown")
saying page up: press "PageUp"
saying number = 1-5 down: press repeat number ("DownArrow")
saying number = 1-5 up: press repeat number ("UpArrow")
saying menu number=1-9: press "Control-F2" repeat number ("RightArrow") "DownArrow"

in Aquamacs:
saying hello: types "hello world"
saying matrix something = 1-3 else = 1-3: types repeat something (repeat else ("-") "*")

in Firefox, Google Chrome:
saying create new tab: presses "Command-t"
saying close tab: presses "Command-w"
saying next tab: presses "Control-Tab"
saying previous tab: presses "Control-Shift-Tab"
saying tab number = 1-9: presses "Command-" number
saying jump back: press "Command-LeftArrow"

in iTerm, Terminal:
saying scratch number = 1-4: presses repeat number ("Control-w")
saying scratch line: presses "Control-u"
saying paste: press "Control-y"
saying do again: presses "UpArrow Return"
saying complete: presses "Tab Tab"
saying search: press "Control-r"


Syntax (Grammar in BNF form)
----------------------------

Program = CCL
CCL = CC | CC CCL
CC = Context CL ":"

Context = Everywhere | In PL
Everywhere = "Everywhere" | "everywhere"
In = "In" | "in"
PL = Program-Name | Program-Name ", " PL

CL = Command | Command CL
Command = "saying" Trigger ":" Action

Trigger = WL
WL = W | W WL
W = VarDeclaration | Word
VarDeclaration = Word "=" Range
Range = Number "-" Number

Action = Verb ActionContent
Verb = "type" | "types" | "press" | "presses" | "run" | "runs"
ActionContent = AL
AL = A | A AL
A = Repeat | Variable | StringLit
Repeat = "repeat" Count "(" ActionContent ")"
Count = Number | Variable
Variable = Word
StringLit = """ String """


### Keystroke language

The use of the Keystroke language is explained in the Semantics section.

Keystroke = KL
KL = K | K KL
K = Key | ML "-" Key
ML = KeyModifier | KeyModifier "-" ML
KeyModifier = "Command" | "Control" | "Shift" | "Option"
Key = LowChar | Digit |
  "DownArrow" | "LeftArrow" | "RightArrow" | "UpArrow" | "Delete" | "End" | "Escape" |
  "F1" | "F2" | "F3" | "F4" | "F5" | "F6" | "F7" | "F8 | "F9" | "F10" | "F11" | "F12" |
  "Home" | "PageDown" | "PageUp" | "Return" | "Space" | "Tab" |
  "`" | "," | "." | "/" | "!" | "~" | "+" | "*" | ":"


### On terminal symbols

Word: represents the word that can be dictated and recognized by the dictation software.
String: represents informally a string. A more precise definition (including escaping rules) will be
described in a further iteration of this document.
LowChar: [a-z]
Digit: [0-9]
Number: [1-9][0-9]*


Semantics
---------

A program in this language consists of a list of commands preceded by the context in which these
commands can be executed. The context can be global (in which case the commands can be executed on
any program) or can be one or more programs. A command contains 2 parts: a trigger and an action.
The trigger is the text the user should dictate and the action is what should be executed once the
dictation is detected.

### Trigger

The trigger may contain variable declarations. Variables in this language are very limited. They can
only consist of a range of numbers (range with letters will be added later). This limitation arises
from the fact that for each possible value in the range, a Dragon Dictate command will be created.

### Action

The action consists of a verb describing the type of action and the content of the action.
The types of actions currently implemented are:
- Text: using the verb `types` or `type` the action will consist of inserting the content of the
  action as a text. This is typically used for text snippets.
- Keystroke: using the verb `presses` or `press` the action will consist of typing a set of
  keystrokes. This is typically used to mix text with special keys (e.g. DownArrow). When using this
  action type, the action content must have the format specified by the *Keystroke language*.
- ShellScript: using the verb `runs` or `run` the action will consist of running the content of the
  action as a shell script using bash (bash expected to be at /bin/bash).

The content of the action may contain repeat blocks and variables. Variables declared in the trigger
may be used to determine the number of repetitions in a repeat block or simply as part of the
content the action. A repeat block will repeat its content a given number of times and may also be
nested inside other repeat blocks. For example:

    saying scroll a bit: presses repeat 5 ("DownArrow")

is transformed into

    saying scroll a bit: presses "DownArrowDownArrowDownArrowDownArrowDownArrow"

A repeat block can also be used with a variable. For example:

    saying down number=1-5: presses repeat number ("DownArrow")

is transformed into

    saying down 1: presses repeat 1 ("DownArrow")
    saying down 2: presses repeat 2 ("DownArrow")
    saying down 3: presses repeat 3 ("DownArrow")
    saying down 4: presses repeat 4 ("DownArrow")
    saying down 5: presses repeat 5 ("DownArrow")

Each repeat block that now contains a number will be then replaced as described before. Variables
can also be used to modify keystrokes. For example:

    saying tab variable=1-3: presses "Tab-variable"

is transformed into

    saying tab 1: presses "Tab-1"
    saying tab 2: presses "Tab-2"
    saying tab 3: presses "Tab-3"

The program will be compiled into an XML file that can be used imported into Dragon Dictate. Because
using this format one cannot define real variables the commands that contain variables have to be
expanded internally creating one command for each possible variable value.

### Keystroke language

When using this action type, the action content must have the format specified by the keystroke
language. In this language spaces are optional so `abc` is the same as `a b c` or `a bc` or `ab c`.
The same is valid for key names, so `DownArrow DownArrow` is the same as `DownArrowDownArrow`.
Capital letters cannot be used directly to write text, so to produce "A" the keystroke should be
`Shift-a`.

### Language features

- Multiple action types (Keystroke, Text, ShellScript).
- Commands may contain simple variable ranges.
- Repeatable actions.


How the language solves the problem
-----------------------------------

Because the creation of commands in Dragon Dictate is typically done using the user interface,
having a high-level language that can be compiled into commands increases dramatically the
usefulness of this tool. Moreover, having a concise syntax to express optional words in the commands
and commands containing variables simplifies the creation of commands and reduces the amount of
repetition when creating commands.
