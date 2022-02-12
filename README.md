# ca65hl
Macros for pseudo high-level language structures for the ca65 assembler.
Overview

    This is a macro package for use with the ca65 assembler (from the cc65 cross development package) that adds high-level functionality to the assembler. There is no library or 6502 code used, it only extends the syntax and functionality of the assembler. At this point it is targeted for compatibility with the NMOS 6502, so some code generated will not use more advanced features of the 65c02 or higher. All code generated corresponds directly to underlying assembly and with an understanding of the macro code, all assembly code can still be controlled directly in almost every case. 
    The main feature of the macros is to add IF..ELSEIF..ENDIF flow control.
    
    Please see the wiki entries for more information.
