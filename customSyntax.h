; --------------------------------------------------------------------------------------------
; https://mit-license.org/
; Copyright © 2022 Julian Terrell big.JT@protonmail.com
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated 
; documentation files (the “Software”), to deal in the Software without restriction, including without limitation 
; the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
; and to permit persons to whom the Software is furnished to do so, subject to the following conditions: The above 
; copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
; TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
; DEALINGS IN THE SOFTWARE.

; --------------------------------------------------------------------------------------------
; File: customSyntax.h
;
; --------------------------------------------------------------------------------------------
; Allow custom control over instructions, but still use ca65 built in functions to evaluate
; the instructions. This was a part of ca65hl, and the custom syntax only worked with ca65hl macros, but
; it has been separated into this file and will work everywhere by using the ca65 feature:
;
; .feature ubiquitous_idents
;
; This feature allows instructions to be used as macro names. The macros will toggle .feature ubiquitous_idents
; to allow ca65 to process the instruction normally. Be careful to not declare your own macros with instruction
; names accidentally. ( This shouldn't be possible unless these macros are inadvertently undefined. )
;
; --------------------------------------------------------------------------------------------
; Two features offered with these macros:
;
; 1) More than one instruction or macro per line separated by ':'
;
;   In this case, commas cannot be used with instructions or macros. If a comma is desired, use a single 
;   instruction. Macros cannot start a multi-instruction line.
;
; 2) The instruction macros will process their operands through the ___arraySyntax macro that allows
; an alternative syntax with array like indexing with constant expressions and the x or y registers.
;
;   Example:
; > lda foo[ 4 ] ; becomes: lda foo + 4
;
;   As well, the index can contain 'x' or 'y', to indicate to use the 6502's
;   indexed addressing modes.
;
;   Example:
; > lda foo[ 4 + x ] ; becomes: lda foo + 4, x
;
;   The index can be anywhere in the expression:
;
;   Example:
; > lda [ y - 4 ]foo
;
; The macro will allow any combination of constant values with either a Y or X included, but the index register
; must be first in the square brackets, or the index register must be preceded by a plus (+). The macro then converts 
; it to standard syntax and passes it to ca65 to assemble the instruction as normal.

.ifndef ::_CUSTOM_SYNTAX_H
::_CUSTOM_SYNTAX_H = 1

.feature ubiquitous_idents +    ; allow overloading mnemonics

.scope CUSTOM_SYNTAX
    JMP_INSTRUCTION_COUNTER     .set 0  ; count
    OUTPUT_CUSTOM_SYNTAX        .set 0
    RTS_FOUND                   .set 0
.endscope

; RTS tracking is for future use
.macro customSyntaxCountRTS
    CUSTOM_SYNTAX::RTS_FOUND .set 0
.endmacro

.define customSyntaxFoundRTS () (CUSTOM_SYNTAX::RTS_FOUND)

.macro customSyntax_Output opt1
    .if .xmatch( opt1, on )
        CUSTOM_SYNTAX::OUTPUT_CUSTOM_SYNTAX .set 1
    .elseif .xmatch( opt1, off )
        CUSTOM_SYNTAX::OUTPUT_CUSTOM_SYNTAX .set 0
    .else
        .error "Invalid option. Should be: 'on' or 'off'."
    .endif
.endmacro
; --------------------------------------------------------------------------------------------
; Function: ___findToken param, tok, position
;
; Parameters:
;
;   param - Token list to search through.
;   tok - Token to find.
;   position - Passed identifier to store found position in.
;              Should be initialized to zero to search the entire token list.
;
; Note:
; It won't find the token in the very first (0) position!
; This macro is defined elsewhere, so check if defined first.

.if !.definedmacro( ___findToken )
.macro ___findToken param, tok, position
    .repeat .tcount( {param} ), c
        .if .xmatch( {.mid(c, 1, {param}) }, {tok})
            .if !position ; do not remove this check
                position .set c
                .exitmacro
            .endif
        .endif
    .endrepeat
.endmacro
.endif

; --------------------------------------------------------------------------------------------
; Function: ___arraySyntax instr, op, index
;
; Parameters:
;
;   instr - CPU instruction to output.
;   op - Operand for instruction.
;
;   Look for '[]' set and allow as an array, possibly with x or y indexed:
;
;   This macro will output the instruction in passed in instr but it will 
;   process the operand, allowing an index defined by square braces: '[]'.
;   In the square braces can be any constant expression, which will be 
;   extracted and added onto the end of the operand as in normal assembly.
;
;   Example:
; > lda foo[ 4 ] ; becomes: lda foo+4
;
;   As well, the index can contain 'x' or 'y', to indicate to use the 6502's
;   indexed addressing modes.
;
;   Example:
; > lda foo[ 4 + x ] ; becomes: lda foo+4, x

.macro ___arraySyntax instr, op, index

    .local open
    .local close
    .local reg
    .local regPos
    .local REGX, REGY
    REGX = 1
    REGY = 2
    open    .set 0
    close   .set 0
    reg     .set 0
    regPos  .set 0
    
    ; Only use custom syntax if there was no comma
    ; if index was passed, treat as normal 6502 syntax and quit, since this syntax does not use commas
    .ifblank index
        ___findToken {op}, [, open
        ; also check if position 0 is valid for open, since ___findToken won't find the first position
        .if open || .xmatch ( .left(1, {op}), [ )
            ___findToken {op}, ], close
            .if close
                ; look for '[ x +' type pattern
                .if .xmatch( {.mid(open + 1, 1 ,{op})}, x)
                    reg .set REGX
                .elseif .xmatch( {.mid(open + 1, 1 ,{op})}, y)
                    reg .set REGY
                .endif
                .if reg
                    ; require a +/-, but allow '[y]' or '[x]' if nothing else: 
                    ; if only a 'y' or 'x', define the constant as nothing
                    .if open + 2 = close
                        .define _CONST 
                        ; next has to be a + or -, or it is an error
                    .elseif .xmatch( {.mid(open + 2, 1 ,{op})}, +)
                        .define _CONST + (.mid(open + 3, close - open - 3 ,{op}))
                    .elseif .xmatch( {.mid(open + 2, 1 ,{op})}, -)
                        .define _CONST - (.mid(open + 3, close - open - 3 ,{op}))
                    .else
                        .error "Expected: '+' or '-'"
                    .endif
                .else
                    ; No reg found yet. Check for '+x' anywhere in the brackets
                    ___findToken {op}, x, regPos
                    .if regPos && regPos < close
                        reg .set REGX
                    .else
                        ___findToken {op}, y, regPos
                        .if regPos && regPos < close
                            reg .set REGY
                        .endif
                    .endif
                    ; found a valid register? to the left must be a +
                    .if reg
                        .if !.xmatch( {.mid(regPos - 1, 1 ,{op})}, +) 
                            .error "Expected: '+' before x or y."
                        .endif
                        ; lda foo[ 3 + x - 5 ]
                        ;      0 1 2 3 4 5 6 7
                        .define _CONST + (.mid(open + 1, regPos - open - 2, {op}) .mid(regPos + 1, close - regPos - 1, {op}))
                    .else
                        ; no registers, constant is whatever is in the '[]'
                        .define _CONST + (.mid(open + 1, close - open - 1 ,{op}))
                    .endif
                .endif
            .else
                .error "Expected: ']'"
            .endif
        .else
            ; no '[]'
            ; set value to make the expression below for .mid() become .tcount() only
            open .set .tcount({op})
            .define _CONST
        .endif
    
        ; anything after the '[]' pair? this allows index to be anywhere in the expression, rather than only at the end
        .if close && .tcount({op}) > close + 1
            .define _AFTER () .mid(close + 1, .tcount({op}) - close - 1 , {op})
        .else
            .define _AFTER
        .endif
        .if reg = REGX
            .define _OUT .left(1,instr) .mid(0, open, {op}) _AFTER _CONST, x
        .elseif reg = REGY
            .define _OUT .left(1,instr) .mid(0, open, {op}) _AFTER _CONST, y
        .else                                                                 
            .define _OUT .left(1,instr) .mid(0, open, {op}) _AFTER _CONST
        .endif
        .undefine _CONST
        .undefine _AFTER
    .else
        ; normal instruction syntax
        .define _OUT instr op, index
    .endif
    ; --------------------------------------------------------------------------------------------
    ; Output instruction:
    .feature ubiquitous_idents -    ; allow normal instruction table look ups
    _OUT                            ; output instruction as standard ca65 6502 syntax
    .feature ubiquitous_idents +    ; allow overloading mnemonics again
    ; --------------------------------------------------------------------------------------------
    .if CUSTOM_SYNTAX::OUTPUT_CUSTOM_SYNTAX 
        printTokenList {_OUT}
    .endif
    .undefine _OUT
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___EvalInstrList statement, index
;
; Parameters:
;
;   statement - on first call, will be a CPU mnemonic and operand. After could also be a macro name
;   index - indexed mode for the instruction - x or y
;
; This macro is called by the overloaded instruction macros and allows for one or more instructions
; per line, separated by a colon ':'. A macro cannot be first on the line when using a multiple instructions.
; It will send instructions to ___arraySyntax to support extended syntax as described in this file.

.macro ___EvalInstrList statement, index

    ; split macro calls at colons:
    .local colonPos
    colonPos .set 0
    ___findToken {statement}, :, colonPos
    .if colonPos
        .ifnblank index
            .error "Commas not supported for multiple instructions on one line."
            .fatal ""
        .endif
        ; split at colon, and exit:
        ___EvalInstrList .mid(0, colonPos, {statement})
        ___EvalInstrList .mid( colonPos + 1 , .tcount({statement}) - colonPos - 1, {statement} )
    .else
        ; no colon, output macros or instructions:

        ; turn of instructions as macros temporarily to allow .ismnemonic to match instructions:
        .local isInstruction
        .feature ubiquitous_idents -
        isInstruction = .ismnemonic(.left (1,{statement})) || .xmatch( .left (1,{statement}), adc) ; ca65 bug? .ismnemonic doesn't match 'adc'
        .feature ubiquitous_idents +

        ; if found an instruction, send it to ___arraySyntax to process any extended syntax, otherwise, let ca65 deal with it
        .if isInstruction
            
            ; For instruction jmp: Save addresses of jmp instructions. This is to allow an .assert that checks ELSE or ELSEIF 
            ; was not proceeded by a jmp instruction. If it was, ca65hl will suggest to use 'jmp' option with the ELSE/ELSEIF 
            ; macro that will suppress the macro's normal generation of a jmp instruction to skip to the ENDIF. If 'jmp' 
            ; option was used, it will verify that the usage is correct.
            .if .xmatch(.left (1,{statement}), jmp )
                .ifdef ::CA65HL_H
                    ::.ident( .sprintf( "LAST_JMP_INSTRUCTION_END_ADDRESS_%04X", CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER)) = * + 3
                    CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER .set CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER + 1
                .endif
            .endif

            ; tracking rts for future use
            .if .xmatch(.left (1,{statement}), rts )
                CUSTOM_SYNTAX::RTS_FOUND .set 1
            .endif
            
            ___arraySyntax .left (1,{statement}), { .mid (1, .tcount({statement}) - 1, {statement} ) }, index
        .else
            statement
        .endif
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Overload mnemonics to allow custom syntax for all instructions

.macro lda operand, index
    ___EvalInstrList lda operand, index
.endmacro
.macro sta operand, index
    ___EvalInstrList sta operand, index
.endmacro
.macro ldx operand, index
    ___EvalInstrList ldx operand, index
.endmacro
.macro stx operand, index
    ___EvalInstrList stx operand, index
.endmacro
.macro ldy operand, index
    ___EvalInstrList ldy operand, index
.endmacro
.macro sty operand, index
    ___EvalInstrList sty operand, index
.endmacro
.macro adc operand, index
    ___EvalInstrList adc operand, index
.endmacro
.macro and operand, index
    ___EvalInstrList and operand, index
.endmacro
.macro asl operand, index
    ___EvalInstrList asl operand, index
.endmacro
.macro cmp operand, index
    ___EvalInstrList cmp operand, index
.endmacro
.macro dec operand, index
    ___EvalInstrList dec operand, index
.endmacro
.macro eor operand, index
    ___EvalInstrList eor operand, index
.endmacro
.macro inc operand, index
    ___EvalInstrList inc operand, index
.endmacro
.macro lsr operand, index
    ___EvalInstrList lsr operand, index
.endmacro
.macro ora operand, index
    ___EvalInstrList ora operand, index
.endmacro
.macro rol operand, index
    ___EvalInstrList rol operand, index
.endmacro
.macro ror operand, index
    ___EvalInstrList ror operand, index
.endmacro
.macro sbc operand, index
    ___EvalInstrList sbc operand, index
.endmacro
.macro bit operand, index
    ___EvalInstrList bit operand, index
.endmacro
.macro cpx operand, index
    ___EvalInstrList cpx operand, index
.endmacro
.macro cpy operand, index
    ___EvalInstrList cpy operand, index
.endmacro
.macro inx operand, index
    ___EvalInstrList inx operand, index
.endmacro
.macro iny operand, index
    ___EvalInstrList iny operand, index
.endmacro
.macro bcc operand, index
    ___EvalInstrList bcc operand, index
.endmacro
.macro bcs operand, index
    ___EvalInstrList bcs operand, index
.endmacro
.macro beq operand, index
    ___EvalInstrList beq operand, index
.endmacro
.macro bne operand, index
    ___EvalInstrList bne operand, index
.endmacro
.macro bpl operand, index
    ___EvalInstrList bpl operand, index
.endmacro
.macro bmi operand, index
    ___EvalInstrList bmi operand, index
.endmacro
.macro brk operand, index
    ___EvalInstrList brk operand, index
.endmacro
.macro bvc operand, index
    ___EvalInstrList bvc operand, index
.endmacro
.macro bvs operand, index
    ___EvalInstrList bvs operand, index
.endmacro
.macro clc operand, index
    ___EvalInstrList clc operand, index
.endmacro
.macro cld operand, index
    ___EvalInstrList cld operand, index
.endmacro
.macro cli operand, index
    ___EvalInstrList cli operand, index
.endmacro
.macro clv operand, index
    ___EvalInstrList clv operand, index
.endmacro
.macro dex operand, index
    ___EvalInstrList dex operand, index
.endmacro
.macro dey operand, index
    ___EvalInstrList dey operand, index
.endmacro
.macro jmp operand, index
    ___EvalInstrList jmp operand, index
.endmacro
.macro jsr operand, index
    ___EvalInstrList jsr operand, index
.endmacro
.macro nop operand, index
    ___EvalInstrList nop operand, index
.endmacro
.macro pha operand, index
    ___EvalInstrList pha operand, index
.endmacro
.macro php operand, index
    ___EvalInstrList php operand, index
.endmacro
.macro pla operand, index
    ___EvalInstrList pla operand, index
.endmacro
.macro plp operand, index
    ___EvalInstrList plp operand, index
.endmacro
.macro rti operand, index
    ___EvalInstrList rti operand, index
.endmacro
.macro rts operand, index
    ___EvalInstrList rts operand, index
.endmacro
.macro sec operand, index
    ___EvalInstrList sec operand, index
.endmacro
.macro sed operand, index
    ___EvalInstrList sed operand, index
.endmacro
.macro sei operand, index
    ___EvalInstrList sei operand, index
.endmacro
.macro tax operand, index
    ___EvalInstrList tax operand, index
.endmacro
.macro tay operand, index
    ___EvalInstrList tay operand, index
.endmacro
.macro tsx operand, index
    ___EvalInstrList tsx operand, index
.endmacro
.macro txa operand, index
    ___EvalInstrList txa operand, index
.endmacro
.macro txs operand, index
    ___EvalInstrList txs operand, index
.endmacro
.macro tya operand, index
    ___EvalInstrList tya operand, index
.endmacro

.endif ; .ifndef ::_CUSTOM_SYNTAX_H
