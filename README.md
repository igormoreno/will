How to use Will
---------------

The Will compiler is implemented in Haskell and right now all its source code is in the file will.hs.
So compile it with, for example:
ghc will.hs

The Will source file is right now read by the compiler from the standard input. So if the source code is in a file called hello.will, compile it with:
./will < hello.will

The compiler will output one XML file for each context in the Will file. For example, if the source file contains:

    in Firefox, Google Chrome:
    command 1
    command 2
    
    in Terminal:
    command 3
    
    in Firefox:
    command 4

3 files will be generated, one with the commands to be executed in the context "Firefox" and the other 2 for the contexts "Google Chrome" and "Terminal".
These files have extension ".commandstext" and can be directly imported into Dragon (for Mac).


Implementation
--------------

The functions that implement each compiler phase are grouped together and separated by a set of comments naming each compiler phase. They are chained together in the function "compile" in the bottom of the file.


To do
-----

### Known limitations
- The "vendor" should be the username of the current loged in user
- Look for ID collisions
- Properly interpret \"

### Code improvements
- Add a lower AST
- Use heredocs or another approach to avoid overly concat strings
- Re-factor code to avoid using unsafe I/O
- Break the code in multiple modules

### New features
- Read input from a file instead of the standard input
- Add syntax for comments
- Parse key names (instead of just reading them as strings)
- Read multiple input files from the command line
- Optional words in command trigger
- Improve the readme file

