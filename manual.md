::: {#header}
# CA65HL
:::

::: {#content}
::: sect2
### Overview {#_overview}

::: paragraph
This is a macro package for use with the ca65 assembler (from the cc65
cross development package) that adds high-level functionality to the
assembler. There is no library or 6502 code used, it only extends the
syntax and functionality of the assembler. At this point it is targeted
for compatibility with the NMOS 6502; code output will not use more
advanced features of the 65c02 or higher. All macro code corresponds
directly to underlying assembly and all assembly code can still be
controlled directly in almost every case. The macros have no ability to
optimize based on the previous code used; the programmer/developer must
be aware of what optimizations are available.
:::

::: paragraph
The main feature is **if** flow control. The **if** statement logic is
based on 6502 CPU flags and branch statements, not an evaluation of a
Boolean expressions. Loop statements are essentially the same thing and
use the same code.
:::
:::

::: sect1
## Conditional Expressions {#_conditional_expressions}

::: sectionbody
::: paragraph
The **if** statement and looping macros generate branch instructions
based on the condition passed. In the simplest form they expect an
expression of both a 6502 flag name as a single uppercase character: **C
Z N V** or **G**. Followed by **set** or **clear**. Due to this, one
should avoid using **C Z N V G** or **clear** or **set** as identifiers.
(**G** represents \'greater than\', but this should be avoided where
possible due to it requiring two branch instructions to evaluate.) The
**set** or **clear** are not required. If omitted, the flag will be
processed as if **set** was used. If **clear** is used the branch for
the flag will be inverted.
:::

::: paragraph
In the simplest use in an if statement:
:::

::: literalblock
::: content
    if (C set)
    ; do code here if C flag is set
    endif
:::
:::

::: paragraph
Conditions are required to be enclosed in parenthesis.
:::

::: paragraph
Also accepted: Any number of negate signs in front of the condition:
:::

::: literalblock
::: content
    if (!C set)
    ; do code here if C flag not set
    endif
:::
:::

::: paragraph
Using the [C-style macros](https://cc65.github.io/doc/ca65.html#ss12.7)
in ca65, it's trivial to setup (the included) alternative flag names. In
use, these defines can be followed by **set** or **clear**:
:::

::: literalblock
::: content
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
:::
:::
:::
:::

::: sect1
## Macro \"Functions\" and Inline Code Expressions {#_macro_functions_and_inline_code_expressions}

::: sectionbody
::: paragraph
If the expression does not match a flag definition as described above,
and is not an identifier (variable), the macro will attempt to execute
the passed value as a macro or instruction. If you have defined a macro
to be called in this way, it could use the macro **setBranch** which
should be followed by a valid flag definition. The flag setting defined
this way is what is being treated as \'true\' when compared to Boolean
logic.
:::

::: paragraph
Example:
:::

::: literalblock
::: content
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
      setBranch N set        ; tell the conditional statement to test the N flag, could also use 'negative'
    .endmacro                ; when using this macro, 'N set' could be thought of as true, N clear is false.
:::
:::

::: paragraph
The macro defines which CPU flags to test and can then be used in the
**if** statement:
:::

::: literalblock
::: content
    if (regNegative a)
        ;code
    endif
:::
:::

::: paragraph
As well, assembly code can be used to determine the condition, with any
number of assembly statements and macros separated by colons. In this
way, if **setBranch** is not used to define the CPU flag, the double
equal can be used (==) or the not equal (!=) followed by the flag to be
tested. The latter will invert the flag to be tested. Example:
:::

::: literalblock
::: content
    if ( lda MyVariable : tay : dey == zero )
    ; do code if MyVariable is equal to 1
    endif
:::
:::

::: paragraph
Another Example:
:::

::: literalblock
::: content
    if ( jsr inRange == C set )
     ; do stuff if in range
    endif
:::
:::

::: paragraph
**Note:** Using the **==** or **!=** symbol is a way of defining which
flag to test and is not truly equality. It could be thought of as \"If
this *results* in this flag being set/clear then the expression is
TRUE.\"
:::
:::
:::

::: sect1
## Logical AND/OR Support {#_logical_and_or_support}

::: sectionbody
::: paragraph
All conditional statements support logical AND and OR in the the
expression, with the default operators **&&** and **\|\|** (which will
also match **.and** and **.or** tokens in ca65.)
:::

::: paragraph
Example:
:::

::: literalblock
::: content
    if ( C set || N set && V set)
        ; code
    endif
:::
:::

::: sect2
### Parentheses {#_parentheses}

::: paragraph
Parentheses can be used to generate more complex branching logic:
:::

::: literalblock
::: content
    if ((( lda foo == negative && ldx bar == zero) || lda foo == zero) && (ror bar == C set || ldx baz : inx == zero)) goto myLabel
:::
:::

::: paragraph
Logical AND/OR with parentheses can be used in any order to help create
complex branching logic.
:::

::: paragraph
Parentheses should also be used to have the macro code ignore an
enclosed section if required. For example, if your inline macro or code
uses characters in its parameters that would normally be recognized as
part of a conditional expression, you can enclose the parameters in
parentheses and it will be ignored. This is especially useful with some
on the integrated macro support.
:::

::: paragraph
Example:
:::

::: literalblock
::: content
    if (mySuperCoolMacro foo && bar) goto myLabel
:::
:::

::: paragraph
In this example, the macro, **mySuperCoolMacro**, requires the \'&&\' to
be passed. This will be parsed by the **if** and cause an error. To work
around this:
:::

::: literalblock
::: content
    if (mySuperCoolMacro(foo && bar)) goto myLabel
:::
:::

::: paragraph
The macro will have to check for, and remove any parentheses, but the &&
will be ignored by the **if** macro.
:::
:::

::: sect2
### Braces (Curly Brackets) {#_braces_curly_brackets}

::: paragraph
Curly braces should be used to enclose the entire parameter line if
including any commas for any reason, such as an inline index
instruction:
:::

::: literalblock
::: content
    if {( ldx list,y == negative )}
    ; some code
    endif
:::
:::
:::

::: sect2
### Implied Register Loading and Expression Evaluation {#_implied_register_loading_and_expression_evaluation}

::: paragraph
If a condition references an identifier alone, the macro code will
default to using the accumulator to load the identifier via a **lda**
instruction. As well, if no flag is specified with either the double
equal (==), not equal (!=) or **setBranch**, and the macro has found
what seems to be valid assembly code or an identifier, it will default
to using **Z clear** to simulate a non-zero result as true.
:::

::: paragraph
Example:
:::

::: literalblock
::: content
    if (myFlag)        ; myFlag is a variable, it gets loaded into reg A, and is evaluated as true if it is non-zero
    ; myFlag code
    endif

    ; this will generate the same code as above:
    if (lda myFlag != zero)
    ; my Flag code
    endif
:::
:::
:::

::: sect2
### Inverting logic {#_inverting_logic}

::: paragraph
By default, the logical \'boolean not\' operator is **.not** or **!**.
You can negate an individual condition, or an entire parentheses set:
:::

::: literalblock
::: content
    if (!myFlag)
    ; Don't do this unless the flag is clear/false
    endif

    ; this will generate the same code as above:
    if (lda myFlag == zero)
    ; my Flag code
    endif
:::
:::

::: paragraph
These two **if** statements generate equivalent code:
:::

::: literalblock
::: content
    if ( C set || N set || V set)
        ; code
    endif

    if (!( !C set && !N set && !V set))
        ; code
    endif
:::
:::
:::
:::
:::

# If Statement {#_if_statement .sect0}

::: paragraph
The **if** statement is quite flexible. There are two kinds: An **if**
statement starting a block of code and a stand alone **if** statement.
:::

::: paragraph
If Statement Code Block
:::

::: paragraph
This is very similar to most high level programming syntax. The keywords
to create a block are: **if**, **else**, **elseif**, **endif**. The
conditional expression follows the syntax
[here.](https://github.com/Movax12/ca65hl/wiki/Conditional-Expressions)
:::

::: literalblock
::: content
    if <condition>
        ; execute here if true
    else
        ; execute here if false
    endif
:::
:::

::: paragraph
The **if** statement will generate appropriate branches depending on the
condition(s). The **else** or **elseif** statement generates a **JMP**
instruction to the end of the if block. If a CPU flag can be known to
always be set/clear when the **else** or **elseif** is encountered you
can tell it to branch on that condition instead, using that known state:
:::

::: literalblock
::: content
    if <condition>
        ; execute here if true
        lda #1    ; Z flag will not ever be set after this instruction
    else Z clear
        ;execute here if false
    endif
:::
:::

::: paragraph
With **elseif**:
:::

::: literalblock
::: content
    if <condition>
        ; execute here if true
        lda #1    ; Z flag will not ever be set after this instruction
    elseif <condition>, Z clear
        ;execute here if false
    endif
:::
:::

::: paragraph
Long branches
:::

::: paragraph
By default, the if macro will generate appropriate branch opcodes. If
the branch is too far away ca65 will generate an error. The macro
command setLongBranch can be used:
:::

::: literalblock
::: content
    setLongBranch +     ; branch to jmp instruction
    setLongBranch -     ; use branch instructions.
:::
:::

::: paragraph
There is also a feature to indicate at link if the long branch was not
needed: (not implemented yet)
:::

::: literalblock
::: content
    setLongBranch +, +    ; if a code block is less than 127 bytes, the linker will say that a long branch is not needed here
    setLongBranch +, -    ; don't warn about code blocks less than 127 bytes
:::
:::

::: paragraph
If statement with **goto** or **break**
:::

::: literalblock
::: content
    if <conditional expression> goto userLabel
    if <conditional expression> break
:::
:::

::: paragraph
If the statement ends with a **break** or **goto** it will be evaluated
as a statement on its own and there is no corresponding **endif**.
:::

::: paragraph
When using **goto**, a label should immediately follow. The macro will
generate a branch to this label. Long branching works here as well.
:::

::: paragraph
When using **break**, the current loop will be exited if the condition
passes. (If not inside a loop, it will generate an error.)
:::

# Integrated Macros {#_integrated_macros .sect0}

::: paragraph
There are two macros that are included as a part of this package that
allow some more features. One is an comparison macro. The other is
primary for assignment, or moving a byte value through some steps
including loading and storing.
:::

::: sect2
### Comparison Macro {#_comparison_macro}

::: paragraph
Designed to be used inline with a conditional expression. For example:
:::

::: literalblock
::: content
    lda height
    if ( a >= #$F0 )
       ; too high code
    endif
:::
:::

::: paragraph
Here, the a macro is called (expanded). It generates a small amount of
code for the comparison of the accumulator to the constant. (In this
case **cmp #\$F0**). It then sets the flag condition to C set. If a
recognized identifier is found, so this also will work:
:::

::: literalblock
::: content
    if ( height >= #$F0 )
        ; too high code
    endif
:::
:::

::: paragraph
Valid comparison operators: **= \<\> \> \< \>= ⇐**
:::

::: paragraph
If you wish to use another register, it will recognize and use the
appropriate comparison instruction:
:::

::: literalblock
::: content
    if ( x < #$60 )
        ;..
    endif

    if ( y = foo )
        ;..
    endif

    if ( ldy height >= #$F0 )
        ; too high code
    endif
:::
:::
:::

::: sect2
### Macro **mb** {#_macro_strong_mb_strong}

::: paragraph
The **mb** macro is designed to make moving byte values and performing
byte operations easier, with a bit of higher level syntax. Used alone,
it requires an assignment operator:
:::

::: literalblock
::: content
    mb a := foo
:::
:::

::: paragraph
This would output the expected: **lda foo**
:::

::: paragraph
The macro tries to determines what instructions to generate for the
commands/values is on the right side of an assignment and assign it to
whatever is on the left.
:::

::: literalblock
::: content
    mb foo := bar
:::
:::

::: paragraph
So, foo and bar can both be either a 6502 register or memory address
(variable). If for example:
:::

::: literalblock
::: content
    mb x := a
:::
:::

::: paragraph
It will output a **tax** instruction. The left side is limited to a
memory address or register, but the right side can also be a simple
expression, using a single register.
:::

::: literalblock
::: content
    mb x := CurrentWorld + #1 & #%00000111
:::
:::

::: paragraph
It will figure out the right side is going to have to use the
accumulator due to the operators, output the correct code ending with a
**tax**. If the assignment was the accumulator, there would be no output
for the assignment (since the accumulator is already holding the
result). **Evaluation is limited to simply scanning from left to
right**, there is no implied or explicit precedence.
:::

::: paragraph
If you have two variable names and no indication of the register to use,
the default is to use the accumulator. This can be overridden as:
:::

::: literalblock
::: content
    mb x, var1 := var2
    ; output:
    ; ldx var2
    ; stx var1
:::
:::

::: paragraph
An error will be generated if you try to force an index register with
functionality that requires the accumulator.
:::

::: paragraph
Operators supported:
:::

::: literalblock
::: content
    &     bit and
    |     bit or
    ^     bit eor
    +     add, clear carry 1st
    +c    add with carry
    -     sub, set carry 1st
    -c    sbc with carry
    <<    shift reg a left        (followed by a constant value)
    >>    shift reg a right       (followed by a constant value)
:::
:::

::: paragraph
If you want to use math at build to generate a constant, enclose the
constant in parentheses and the macro code will skip it and pass it to
the assembler:
:::

::: literalblock
::: content
    mb a := #(FOO * 4 + 2)      ; just generate: lda #(FOO * 4 + 2)
:::
:::
:::

::: sect2
### Extended Syntax {#_extended_syntax}

::: paragraph
As well, when using **mb**, you can use an alternate syntax for indexing
values. For example:
:::

::: literalblock
::: content
    ; traditional assembly:
    lda foo + 3
    ; mb macro:
    mb a := foo[ 3 ]
:::
:::

::: paragraph
You can also index with x, or y as allowed by the 6502 instruction set:
:::

::: literalblock
::: content
    ; traditional assembly:
    lda foo + 3, y
    ; mb macro:
    mb a := foo[ 3 + y ]
:::
:::
:::

::: sect2
### Integration Into Conditional Expression Evaluation {#_integration_into_conditional_expression_evaluation}

::: paragraph
When processing a conditional statement, if the statement doesn't match
an instruction, macro and any of the supported operators are found, the
will be evaluated.
:::

::: paragraph
Examples:
:::

::: literalblock
::: content
    if ( foo >> 1 == carry set )
        ; bit zero of foo is set
    endif

    if ( foo & #%00000001 )
        ; bit zero of foo is set, since the default condition is non-zero is true
    endif

    if ( foo & #%00100101 = #%00100101 )
        ; if all bits match do code
    endif
:::
:::

::: paragraph
Note: in these examples, the accumulator will be changed, but \'foo\'
will not.
:::
:::

# Loop Structures {#_loop_structures .sect0}

::: paragraph
There are two main kinds of loops: **do**...​**while \<condition\>** and
**while \<condition\> do**...​**endwhile.**
:::

::: paragraph
**do**...​**while \<condition\>**
:::

::: paragraph
Loop while the condition is true:
:::

::: literalblock
::: content
    ldx #$00
    do                    ; loop from x = 0 to 15
        lda fromhere, x
        sta puthere, x
        inx
        cpx #$10
    while (!equal)
:::
:::

::: paragraph
This can also use a slightly different syntax with the keywords
**repeat** and **until**. These are the same, except that the **until**
negates the condition at the end:
:::

::: literalblock
::: content
    ldx #$00
    repeat                ; loop from x = 0 to 15
        lda fromhere, x
        sta puthere, x
        inx
        cpx #$10
    until (equal)
:::
:::

::: paragraph
The syntax can be expanded, the same code will be generated by this
example:
:::

::: literalblock
::: content
    ldx #$00
    repeat                ; loop from x = 0 to 15
        lda fromhere, x
        sta puthere, x
    until (inx = #$10)
:::
:::

::: paragraph
**while \<condition\> do**...​**endwhile**
:::

::: paragraph
This is a loop that is started with a condition. (Actual
implementation:Code following **while** will be output at the position
of the **endwhile**, and JMP instruction at the start of the loop.)
:::

::: literalblock
::: content
    ldx #$00
    while (inx : x < #$10) do       ; loop from x = 1 to 15
        lda fromhere, x
        sta puthere, x
    endwhile
:::
:::

::: paragraph
For more information see:
[https://github.com/Movax12/ca65hl](https://github.com/Movax12/ca65hl){.bare}
:::
:::
