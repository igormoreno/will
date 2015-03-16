How to use Will
---------------

The Will compiler is implemented in Haskell and right now all its source code is in the file will.hs.
So compile it with, for example:
`ghc will.hs`

To compile a Will source file:
`./will hello.will`

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


Project overview
----------------

Please refer to document docs/overview.md


Language specification
----------------------

Please refer to document docs/language-specification.md


Implementation
--------------

The functions that implement each compiler phase are grouped together and separated by a set of comments naming each compiler phase. They are chained together in the function "compile" in the bottom of the file.


To do
-----

### Known limitations
- The "vendor" should be the username of the current loged in user

### Improvements
- Better testing
- Use heredocs or another approach to avoid overly concat strings
- Re-factor code to avoid using unsafe I/O
- Break the code in multiple modules
- Better parser error messages with <?>
- Improve documentation

### New features
- Read multiple input files from the command line
- Optional words in command trigger

ok for ID collisions

