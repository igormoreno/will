Problem
-------

The use of dictation softwares is increasingly popular and indeed they worked remarkably well with
natural text. But unfortunately they don't work so well if one wishes to write code or just interact
with the computer in general. After suffering from RSI for more than a year, I decided to try to use
a dictation software as a way to interact with the computer without using my hands. After using
Dragon Dictate for two months it was still very frustrating to do many basic things on the computer.

Dragon Dictate allows the user to create personalized commands as a way to make it faster and easier
to perform common tasks. The user can then say a set of words and have Dragon perform a specific
task. This task can be for example press a set of keystrokes or execute a shell script. That can
make interacting with the computer by voice much more productive. Unfortunately, the user can only
create commands using Dragon's UI so for each command several steps have to be performed and they
required the use of the mouse which is something particularly slow to do with dictation.


Proposed solution
-----------------

Create a DSL to add commands to Dragon Dictate.


Key challenges
--------------

The Mac version of Dragon does not provide an API like the Windows version does. The only way to add
commands is via the "import/export commands" functionality accessible via Dragon's UI. It imports
files in a very opaque XML format. There seems to be no documentation about this format available to
users so a good deal of "reverse engineering" and trial and error was required in order to
understand what the parts of this XML mean and how to manipulate it.


Related work
------------

The Windows version of Dragon provides an API that relies on a Windows-specific interprocess
communication API in C/C++. There are a lot of projects that programmatically interact with the
Windows version of Dragon. The most popular of them seems to be dragonfly, that provides a high-level
interface to personalize Dragon using Python. Dragonfly uses Natlink, that creates a Python API that
calls the low-level interprocess communication API. Unfortunately the Mac version of Dragon does not
provide an API so none of these projects apply to it.

There is a project in GitHub of a tool to create Dragon Dictate command files using Scala
(https://github.com/siderakis/dragon-fire). Some of the limitations of this project are:
- Neither the project nor the source code is documented.
- It works only to create commands that execute keystrokes.
- It creates a DSL embedded in scala to do it but the syntax uses identifiers such as ">>" that are
neither very explanatory nor easy to write using a dictation software.

There is a commercial close-source software that interfaces with Dragon Dictate to provide a faster
and more productive way to code by voice.  The product costs $300 and the price does not include
Dragon Dictate, which has to be purchased separately. That's a considerable amount of money
specially considering that Dragon Dictate itself costs already $200.
