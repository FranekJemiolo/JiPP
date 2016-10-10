-----------------------------README---------------------------------------------
This is my interpreter program for my language defined in LBNF Jam.cf.

The files that should be inside directory:
1. Interpret.hs -> The file with all the interpreting functions
2. Interpreter.hs -> Just parsing and then executing program
3. Jam.cf -> Grammar in LBNF for BNFC usage
4. Makefile -> standard makefile, make and make clean defined
5. README.txt - this readme

About compiling the interpreter:
1. Be sure that every file is in place, and if you compiled earlier version of 
   the interpreter than make sure you typed make clean in terminal.
2. Type make in terminal - it creates new Directory called "Interpreter" and
   using bnfc and ghc it creates the interpreter called "Interpreter"

Usage:
Interpreter (inside Interpreter directory) can be called with or without 
parameter. Without it just reads the program from stdin (so the program starts 
to execute after ctrl+d), with the parameter it tries to read the file 
you have given it to read, and then executes program.

About my language:
WHAT IS NOT DEFINED BUT PARSES:
1. Lists
2. Dictionaries
3. Everything that does with lists, dictionaries - all the calls (like first())
   and compare functions plus declarations
4. Some explicit functions for converting strings to ints and ints to strings.
5. Some more exceptions (concerning lists and dictionaries)
6. Passing value by reference into functions!!!!

OK, SO WHAT WORKS:
Well, pretty much everything else, so we got:
1. Three types of values - int, bool, string.
2. Operations on them.
3. Declarations of those.
4. Functions that take multiparam args and return some value (of these types),
   also recursion works.
5. If, While - that take only bool inside ()
6. Standard arithmetic for int, boolean &&, ||, comparing of these objects of 
   these types. Also addition of strings, and implicite conversion of int to 
   string (when adding "someString" + 123 -> converts to string)
7. For - Standard for, throws Error when changing the value on which you iterate
8. Some increase assignments for ints (+=, -=, ++, --)
9. Print - printing values to stdout, works for int, bool, string
10. Try {} Catch {}, Exception () - throwing and catching various exceptions
    also you can throw exceptions of your own kind (passing a string to the
    Exception)
11. Variables also are statically bound and local to their blocks
    (just like functions)
    
NEW in interpreter:
A little bit of help in debugging - now printing statement that threw exception
Covered guarding variable in for
New examples for handling user's own exceptions.
Reduced number of lifts in code using helper functions.