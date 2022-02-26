# ca65hl
Macros for pseudo high-level language structures for the ca65 assembler.

This is a macro package for use with the ca65 assembler (from the cc65 cross development package) that adds 
high-level functionality to the assembler. 
There is no library or 6502 code used, it only extends the syntax and functionality of the assembler. 
At this point it is targeted for compatibility with the NMOS 6502, so some code generated will not use more 
advanced features of the 65c02 or higher. All code generated corresponds directly to underlying assembly and
with an understanding of the macro code, all assembly code can still be controlled directly in almost every 
case.

The main feature of the macros is to add IF..ELSEIF..ENDIF flow control.
To use:

In all ca65 source modules that use these macros:

`.include "ca65hl.h"`

The file `ca65hl.h` also includes the other `*.h` files in this repository. Put them in the same directory, or 
somewhere the ca65 assembler can find them.

Please see the wiki entries for more information.