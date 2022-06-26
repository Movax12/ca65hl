# CA65HL

### Overview

This is a macro package for use with the ca65 assembler (from the cc65
cross development package) that adds high-level like code structures
to the assembler. There is no library or 6502 code used, it only extends the
syntax and functionality of the assembler. At this point it is targeted
for compatibility with the NMOS 6502; code output will not use more
advanced features of the 65c02 or higher. All macro code corresponds
directly to underlying assembly and all assembly code can still be
controlled directly in almost every case. The macros have no ability to
optimize based on the previous code used; the programmer/developer must
be aware of what optimizations are available.

The main feature is **if** flow control. The **if** statement logic is
based on 6502 CPU flags and branch statements, not an evaluation of a
Boolean expressions. Loop statements are essentially the same thing and
use the same macro code.

## Conditional Expressions

The **if** statement and looping macros generate branch instructions
based on the condition passed. In the simplest form they expect an
expression of both a 6502 flag name as a single uppercase character: **C
Z N V** or **G**. Followed by **set** or **clear**. Due to this, one
should avoid using **C Z N V G** or **set** or **clear** as identifiers.
(**G** represents 'greater than', but this should be avoided where
possible due to it requiring two branch instructions to evaluate.) The
**set** or **clear** are not required. If omitted, the flag will be
processed as if **set** was used. If **clear** is used the branch for
the flag will be inverted.

In the simplest use in an if statement:

    if (C set)
    ; do code here if C flag is set
    endif

Conditions are required to be enclosed in parenthesis.

Also accepted: Any number of negate signs in front of the condition:

    if (!C set)
    ; do code here if C flag not set
    endif

Using the [C-style macros](https://cc65.github.io/doc/ca65.html#ss12.7)
in ca65, it’s trivial to setup (the included) alternative flag names. When in
use in a statement, these defines can be followed by **set** or **clear**:

    .define less               !C
    .define greaterORequal      C
    .define carry               C
    .define zero                Z
    .define equal               Z
    .define plus               !N
    .define positive           !N
    .define minus               N
    .define negative            N
    .define bit7                N
    .define overflow            V
    .define bit6                V
    .define bitset             !Z
    .define greater             G
    .define lessORequal        !G

Example:

    if (carry set || equal)
    ; do code here if C flag or Z flag is set
    endif

### Macro "Functions" and Inline Code Expressions

If the expression does not match a flag definition as described above,
and is not an identifier (variable), the macro will attempt to execute
the passed value as a macro or instruction. If you have defined a macro
to be called in this way, it can invoke the macro **setBranch** which
should be followed by a valid flag definition.

Example:

    .macro regNegative reg
      .if .xmatch(reg,a)
        cmp #0
     .elseif .xmatch(reg,x)
        cpy #0
     .elseif .xmatch(reg,y)
        cpx #0
     .else
       .error "No register passed."
     .endif
      setBranch N set        ; tell the conditional statement to test the N flag.
    .endmacro                ; when using this macro, 'N set' could be thought of as true, N clear as false.

The macro defines which CPU flag to test and can then be used in the **if** statement:

    if (regNegative a)
        ;code
    endif

As well, assembly code can be used to determine the condition, with any
number of assembly statements and macros separated by colons. If **setBranch** is 
not used to define the CPU flag, a double equal (==) or not equal (!=) followed 
by the flag to be tested can be used. The latter will invert the flag to be tested. 

Example:

    if ( lda foo : tay : dey == zero )
    ; do code if foo is equal to 1
    endif

Another Example:

    if ( jsr inRange == C set )
     ; do stuff if in range
    endif

Using this method of defining a branch, could be thought of as "If
this *results* in this flag being set/clear then the expression is
TRUE."

### Logical AND/OR Support

All conditional statements support logical AND and OR in the the
expression, with the default operators **&&** and **||** (which will
also match **.and** and **.or** tokens in ca65.)

Example:

    if ( C set || N set && V set)
        ; code
    endif

### Parentheses

Parentheses can be used to generate more complex branching logic:

    if ((( lda foo == negative && ldx bar == zero) || lda foo == zero) && (ror bar == C set || ldx baz : inx == zero)) goto myLabel

Logical AND/OR with parentheses can be used in any order to create
branching logic.

Parentheses should also be used to have the macro code ignore an
enclosed section if required. For example, if your inline macro or code
uses characters in its parameters that would normally be recognized as
part of a conditional expression, you can enclose the parameters in
parentheses and it will be ignored. This is especially useful with some
on the integrated macro support.

Example:

    if (mySuperCoolMacro foo && bar) goto myLabel

In this example, the macro, **mySuperCoolMacro**, requires the '&&' to
be passed. This will be parsed by the **if** and cause an error. To work
around this:

    if (mySuperCoolMacro(foo && bar)) goto myLabel

The macro will have to check for, and remove any parentheses, but the &&
will be ignored by the **if** macro.

### Braces (Curly Brackets)

Curly braces should be used to enclose the entire parameter line if
including any commas for any reason, such as an inline index
instruction:

    if {( lda list,y == negative )}
    ; some code
    endif

### Implied Register Loading and Expression Evaluation

If a condition references an identifier alone, the macro code will
default to using the accumulator to load the identifier via a **lda**
instruction. As well, if no flag is specified with either the double
equal (==), not equal (!=) or **setBranch**, and the macro has found
what seems to be valid assembly code or an identifier, it will default
to using **Z clear** to simulate a non-zero result as true.

Example:

    if (myFlag)        ; myFlag is a variable, it gets loaded into reg A, and is evaluated as true if it is non-zero
    ; myFlag code
    endif

    ; this will generate the same code as above:
    if (lda myFlag != zero)
    ; my Flag code
    endif

### Inverting logic

The 'not' operator is **.not** or **!**. An individual condition, or 
an entire parentheses set can be negated:

    if (!myFlag)
    ; Don't do this unless the flag is clear/false
    endif

    ; this will generate the same code as above:
    if (lda myFlag == zero)
    ; my Flag code
    endif

These two **if** statements generate equivalent code:

    if ( C set || N set || V set)
        ; code
    endif

    if (!( !C set && !N set && !V set))
        ; code
    endif

## If Statement

There are two kinds of **if** statements: An **if** statement starting a 
block of code and a stand alone **if** statement.

#### If Statement with Code Block

This is very similar to most high level programming syntax. The keywords
to create a block are: **if**, **else**, **elseif**, **endif**.

    if <condition>
        ; execute here if true
    else
        ; execute here if false
    endif

The **if** statement will generate appropriate branches depending on the
condition(s). The **else** or **elseif** statement generates a **JMP**
instruction to the end of the if block. If a CPU flag can be known to
always be set/clear when the **else** or **elseif** is encountered you
can tell it to branch on that condition instead, using that known state:

    if <condition>
        ; execute here if true
        lda #1    ; Z flag will not ever be set after this instruction
    else Z clear
        ;execute here if false
    endif

With **elseif**:

    if <condition>
        ; execute here if true
        lda #1    ; Z flag will not ever be set after this instruction
    elseif <condition>, Z clear
        ;execute here if second condition true
    endif

For greatest compatibility, only pass the single letter flag, with an 
optional 'set or 'clear' with **else** or **elseif**.

## Long branches:

By default, the if macro will generate appropriate branch opcodes. If
the branch is too far away ca65 will generate an error. The macro
command **setLongBranch** can be used:

    setLongBranch +     ; use JMP instruction to branch
    setLongBranch -     ; use branch instructions only

Or:


    setLongBranch on    ; use JMP instruction to branch
    setLongBranch off   ; use branch instructions only


There is also a feature to indicate at link if the long branch was not
needed:

    setLongBranch +, +    ; if a code block is less than 127 bytes, the linker will say that a long branch is not needed here
    setLongBranch +, -    ; don't warn about code blocks less than 127 bytes

The **if** macro can also accept an optional **long** or **short** parameter:

    if <condition>, long  ; use JMP instruction to branch, regardless of setLongBranch setting
    if <condition>, short ; use branch instruction to branch, regardless of setLongBranch setting

#### If Statement with **goto** or **break**

    if <condition> goto userLabel
    if <condition> break

If the statement ends with a **break** or **goto** it will be evaluated
as a statement on its own and there is no corresponding **endif**.

When using **goto**, a label name should immediately follow. The macro will
generate a branch to this label. Long branching works here as well.

When using **break**, the current loop will be exited if the condition
passes. (If not inside a loop, it will generate an error.)

## Integrated Macros

There are two macros that are included as a part of this package that
allow some more features. One is an comparison macro. The other is
primarily for assignment, or moving a byte value through some steps
including loading and storing.

### Comparison Macro

Designed to be used inline with a conditional expression. For example:

    lda height
    if ( a >= #$F0 )
       ; too high code
    endif

Here, the a macro is called (expanded). It generates a small amount of
code for the comparison of the accumulator to the constant. (In this
case **cmp \#$F0**). It then sets the flag condition to C set. If a
recognized identifier is found, so this also will work:

    if ( height >= #$F0 )
        ; too high code
    endif

Valid comparison operators: **= &lt;&gt;, &gt;, &lt;, &gt;=, <=**

If you wish to use another register:

    if ( x < #$60 )
        ;..
    endif

    if ( y = foo )
        ;..
    endif

    if ( ldy height >= #$F0 )
        ; too high code
    endif

### Macro **mb**

The **mb** macro is designed to make moving byte values and performing
byte operations easier, with a bit of higher level syntax. Used alone,
it requires an assignment operator:

    mb a := foo

This would output the expected: **lda foo**

The macro tries to determines what instructions to generate for the
commands/values is on the right side of an assignment and assign it to
whatever is on the left.

    mb foo := bar

So, foo and bar can both be either a 6502 register or memory address
(variable). If for example:

    mb x := a

It will output a **tax** instruction. The left side is limited to a
memory address or register, but the right side can also be a simple
expression, using a single register.

    mb x := CurrentWorld + #1 & #%00000111

The macro will determine right side is going to have to use the
accumulator due to the operators, output the correct code ending with a
**tax**. If the assignment was the accumulator, there would be no output
for the assignment (since the accumulator is already holding the
result). **Evaluation is limited to simply scanning from left to
right**, there is no implied or explicit precedence.

If you have two variable names and no indication of the register to use,
the default is to use the accumulator. This can be overridden as:

    mb x, var1 := var2
    ; output:
    ; ldx var2
    ; stx var1

An error will be generated if you try to force an index register with
functionality that requires the accumulator.

Operators supported:

    &     bit and
    |     bit or
    ^     bit eor
    +     add, clear carry first
    +c    add with carry
    -     sub, set carry first
    -c    sbc with carry
    <<    shift reg a left        (followed by a constant value)
    >>    shift reg a right       (followed by a constant value)

If you want to use math at build to generate a constant, enclose the
constant in parentheses and the macro code will skip it and pass it to
the assembler:

    mb a := #(FOO * 4 + 2)      ; just generate: lda #(FOO * 4 + 2)

### Extended Syntax

In the file **ca65hl.h** there is an check for the global identifier
**_CA65HL_USE_CUSTOM_SYNTAX_** If it is not defined or defined as a non-zero 
value, **customSyntax.h** will be included in the source. This file enables
an optional syntax for 6502 assembly that allows offsets and indexed opcodes 
to be written like arrays.

For example:

    ; traditional assembly:
    lda foo + 3
    ; mb macro:
    mb a := foo[ 3 ]

You can also index with x, or y as allowed by the 6502 instruction set:

    ; traditional assembly:
    lda foo + 3, y
    ; mb macro:
    mb a := foo[ 3 + y ]

### Integration Into Conditional Expression Evaluation

When processing a conditional statement, if the statement doesn’t match
an instruction, macro and any of the operators supported by the **mb** macro are found,
they will be evaluated.

Examples:

    if ( foo >> 1 == carry set )
        ; bit zero of foo is set
    endif

    if ( foo & #%00000001 )
        ; bit zero of foo is set, since the default condition is non-zero is true
    endif

    if ( foo & #%00100101 = #%00100101 )
        ; if all bits match do code
    endif

The array syntax can also be used:

    if ( foo[ x + 3 ] >= #1 )
        ; 
    endif

    if ( foo[ 3 ] + #$13 <> #$16 )
        ;
    endif

## Loop Structures

#### The do…while loop

Loop while the condition is true:

    ldx #$00
    do                    ; loop from x = 0 to 15
        lda fromhere, x
        sta puthere, x
        inx
        cpx #$10
    while (!equal)

This can also use a slightly different syntax with the keywords
**repeat** and **until**. These are the same, except that the **until**
negates the condition at the end:

    ldx #$00
    repeat                ; loop from x = 0 to 15
        lda fromhere, x
        sta puthere, x
        inx
        cpx #$10
    until (equal)

The syntax can be expanded, the same code will be generated by this
example:

    ldx #$00
    repeat                ; loop from x = 0 to 15
        lda fromhere, x
        sta puthere, x
    until (inx = #$10)

#### The while - do...endwhile

This is a loop that is started with a condition.
A **JMP** instruction is used to loop at **endwhile**.

    ldx #$00
    while (inx : x < #$10) do       ; loop from x = 1 to 15
        lda fromhere, x
        sta puthere, x
    endwhile

#### For Loop

A C-style for loop.

Usage:
`for ( <init>, <condition>, <increment> ), strict`

This macro requires brackets around a comma separated list of for init, condition and increment values. 
Values for <init> and <increment> can be any amount of instructions separated by ':' and are both optional. 
The <condition> can be anything that follows conditional expression syntax. The end of the code block for the 
loop is defined by `next`

Note: Code for <init> will always be executed. If any value is passed for <strict> the loop will only be 
executed after <condition> is checked. If <strict> is not used, the loop will always be executed at least once. 
If it is intended that the loop will be executed at least once, do not use strict - it avoids the generation of a JMP command.

Example:

    for ( ldy #15, !negative, dey )
       lda (palPtr),y
       sta backgroundPalette, y
    next

#### Switch Statement

Macro **switch** works with macros **case**, and **endswitch** to build a list of constants and corresponding address table 
to use as a jump table. Example:

    switch a	; switch on register a
    
        case #0
        
            ; case 0 code
            ; ...
            break
            
        case #12
        
            ; case 12 code
            ; ...
            break
            
        case #34
        
            ; case 34 code
            ; ...
            ; no break, fall through to case #9
            
        case #9
        
            ; case 9 code
            ; ...
            break
            
        case #4 
        case #5
        case #6
        case #7
        
            ; case 4,5,6,7 code
            ; ...
            break
            
        case default
        
            ; default code
    
    endswitch

If the macro setSwitchStatementDataSeg is used first, the data table will be placed in the defined segment and will allow the macro to not have to include 
a JMP command to skip the data tables. Example:

    setSwitchStatementDataSeg "RODATA"
    switch a
    
        case #0
        
            ; case 0 code
            ; ...
            break
            
        case #1
        
            ; case 1 code
            ; ...
            break
            
        case #2
        
            ; case 2 code
            ; ...
            break
            
        case #3
        
            ; case 3 code
            ; ...
            break
            
        case #4 
        case #5
        case #6
        case #7
        
            ; case 4,5,6,7 code
            ; ...
            break
    
    endswitch

The previous example has ordered cases starting at zero. In this case, add 
the **goto** option to jump to the matching case without searching for a match:

    setSwitchStatementDataSeg "RODATA"
    switch index, goto
    
        case #0
        
            ; case 0 code
            ; ...
            break
            
        case #1
        
            ; case 1 code
            ; ...
            break
            
        case #2
        
            ; case 2 code
            ; ...
            break
            
        case #3
        
            ; case 3 code
            ; ...
            break
            
        case #4 
        case #5
        case #6
        case #7
        
            ; case 4,5,6,7 code
            ; ...
            break
    
    endswitch
    
## Other Optimizations

In the examples for **else** and **elseif**, it was show that an known flag status can be passed by the
programmer to optimize the branch from a **JMP** instruction to a branch. The **if**, **else** and **elseif** 
macros can also be optimized slightly further with two more optional annotations:

### Chaining the **endif** Statement - chain

When using **if** the option **chain** can be added as a parameter to branch to the **endif** of an
enclosing **if**  in a nested **if** statement. Example:

    if ( controller & #BUTTON_UP )
        dec cursorIndex
        if ( negative ), chain              ; When positive, branch to the endif of the enclosing if statement
            inc cursorIndex
        endif
    elseif ( controller & #BUTTON_DOWN )
        if ( cursorIndex < #7 ) ,chain      ; When value is 7 or greater, branch to the endif of the enclosing if statement
            inc cursorIndex
        endif
    elseif ( controller & #BUTTON_START ) 
        jmp doPause
    endif
    
The macro code will verify that this option is used correctly with an **.assert**. (Can only be checked at link time.)
If **\_\_CA65HL_WARNING_LEVEL__** is non-zero the macro code will suggest to use this feature.

### Optimizing tail call JMP before an **else** or **elseif** - jmp
    
When using **else** or **elseif** the option **jmp** can be added as a parameter if the last instruction before the 
**else** or **elseif** is a **JMP** and it is known that the implied **JMP** or branch before the
**else** or **elseif** will never be executed.
Example:

    if ( controller & #BUTTON_UP )
        jmp doControllerUp
    elseif ( controller & #BUTTON_DOWN ), jmp       ; suppress output of the JMP to the endif
        jmp doControllerDown
    elseif ( controller & #BUTTON_START ), jmp      ; suppress output of the JMP to the endif
        jmp doPause
    endif

If **\_CUSTOM_SYNTAX_** is non-zero, the macro code will verify that this option is used correctly with an **.assert**. (Can only be checked at link time.)
If **\_CUSTOM_SYNTAX_** is non-zero, and **\_\_CA65HL_WARNING_LEVEL__** is non-zero the macro code will suggest to use this feature.
If **\_CUSTOM_SYNTAX_** is zero, this option can be used, but the macro code will not be able to verify its use.

## Warnings

The macros in this package attempt to give helpful error messages and warnings about how to use them. 
Warnings will remind about register overwrites or other code generation that might not be obvious and could possibly 
cause bugs in some cases. The identifier **\_\_CA65HL_WARNING_LEVEL__** can be set from 0 to 2 to offer warnings about 
some changes to registers or other changes to the state of the CPU that may not be obvious firsthand.
Important errors or warnings will not be suppressed.


END