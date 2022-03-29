# ca65hl
Macros for pseudo high-level language structures for the ca65 assembler.

This is a macro package for use with the ca65 assembler (from the cc65 cross development package) that adds 
high-level functionality to the assembler. 
There is no library or 6502 code used, it only extends the syntax and functionality of the assembler. 
At this point it is targeted for compatibility with the NMOS 6502, so code generated will not use more 
advanced features of the 65C02 or higher. All assembly code generated corresponds directly to the
macro code, and with some understanding of the macro code, all assembly can still be controlled 
directly in almost every case.

The main feature of the macros is to add IF..ELSEIF..ENDIF flow control.

To use:

In all ca65 source modules that use these macros:

`.include "ca65hl.h"`

The file `ca65hl.h` also includes the other `*.h` files in this repository. Put them in the same directory, or 
somewhere the ca65 assembler can find them.

Please see manual.md entries for more information.

Please see https://gitlab.com/ca65/ca65hl for the most recent code, as thie github repo may not be updated.