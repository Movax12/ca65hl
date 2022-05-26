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
; Custom instruction syntax section.
; Allow custom controls for instructions, but still use ca65 built in functions to evaluate
; the instructions. This was a part of ca65hl, and the custom syntax only worked with ca65hl macros, but
; it has been separated into this file and will work everywhere by using the ca65 feature:
;
; .feature ubiquitous_idents
;
; This feature allows instructions to be used as macro names. The macros then temporarily undefine themselves to
; allow ca65 to process the instruction normally. Be careful to not declare your own macros with instruction
; names accidentally.
;
; The instruction macros will process their operands through the ___arraySyntax macro that results in allowing
; an alternative syntax that allows array like offsets an indexing.
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
; The macro will allow any combination of constant values with either a y or x included, but the index register
; must be first in the square brackets, or the index register must be preceded by a plus (+). The macro then converts it
; to standard syntax and passes it to ca65 to verify the addressing mode. 

.ifndef ::_CUSTOM_SYNTAX_
::_CUSTOM_SYNTAX_ = 1

.feature ubiquitous_idents ; allow overloading mnemonics

.scope _CUSTOM_
    JMP_INSTRUCTION_COUNTER .set 0  ; count 
.endscope

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
; This macro is defined elsewhere in my source, so check if defined first.

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
; Function: ___arraySyntax instr, op
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

.macro ___arraySyntax instr, op

    .local open
    .local close
    .local reg
    .local regPos
    .local REGX, REGY
    REGX = 1
    REGY = 2
    open   .set 0
    close  .set 0
    reg    .set 0
    regPos  .set 0
    
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
    .if (.tcount({op}) > close + 1) && (close > 0 )
        .define _AFTER () .mid(close + 1, .tcount({op}) - close - 1 , {op})
    .else
        .define _AFTER
    .endif
    .if reg = REGX
        .left(1,instr) .mid(0, open, {op}) _AFTER _CONST, x
    .elseif reg = REGY 
        .left(1,instr) .mid(0, open, {op}) _AFTER _CONST, y
    .else                                                                 
        .left(1,instr) .mid(0, open, {op}) _AFTER _CONST
    .endif
    .undefine _CONST
    .undefine _AFTER
.endmacro


.macro macro_jmp operand
    .undefine jmp
    jmp operand
    
    ; Use this code with ca65hl.h to track jump commands. This is to .assert that an ELSE or ELSEIF was not proceeded by a jmp instruction.
    ; If it was, ca65hl will suggest to use 'jmp' option with the ELSE/ELSEIF macro that will suppress the macro's normal generation of a 
    ; jmp instruction to skip to the ENDIF. If 'jmp' option was used, it will verify that the usage is correct.
    .ifdef ::_CA65HL_H_
        ::.ident( .sprintf( "JMP_INSTRUCTION_END_%04X", _CUSTOM_::JMP_INSTRUCTION_COUNTER)):
        _CUSTOM_::JMP_INSTRUCTION_COUNTER .set _CUSTOM_::JMP_INSTRUCTION_COUNTER + 1
    .endif
    
    .define jmp macro_jmp
.endmacro

; --------------------------------------------------------------------------------------------
; Allow custom syntax for all modes that use operands:

.macro macro_lda operand, index
    .undefine lda
    .if .paramcount > 1
        lda operand, index
    .else
        ___arraySyntax lda, operand
    .endif
    .define lda macro_lda
.endmacro

.macro macro_sta operand, index
    .undefine sta
    .if .paramcount > 1
        sta operand, index
    .else
        ___arraySyntax sta, operand
    .endif
    .define sta macro_sta
.endmacro

.macro macro_ldx operand, index
    .undefine ldx
    .if .paramcount > 1
        ldx operand, index
    .else
        ___arraySyntax ldx, operand
    .endif
    .define ldx macro_ldx
.endmacro

.macro macro_stx operand, index
    .undefine stx
    .if .paramcount > 1
        stx operand, index
    .else
        ___arraySyntax stx, operand
    .endif
    .define stx macro_stx
.endmacro

.macro macro_ldy operand, index
    .undefine ldy
    .if .paramcount > 1
        ldy operand, index
    .else
        ___arraySyntax ldy, operand
    .endif
    .define ldy macro_ldy
.endmacro

.macro macro_sty operand, index
    .undefine sty
    .if .paramcount > 1
        sty operand, index
    .else
        ___arraySyntax sty, operand
    .endif
    .define sty macro_sty
.endmacro

.macro macro_adc operand, index
    .undefine adc
    .if .paramcount > 1
        adc operand, index
    .else
        ___arraySyntax adc, operand
    .endif
    .define adc macro_adc
.endmacro

.macro macro_and operand, index
    .undefine and
    .if .paramcount > 1
        and operand, index
    .else
        ___arraySyntax and, operand
    .endif
    .define and macro_and
.endmacro

.macro macro_asl operand, index
    .undefine asl
    .if .paramcount > 1
        asl operand, index
    .else
        ___arraySyntax asl, operand
    .endif
    .define asl macro_asl
.endmacro

.macro macro_cmp operand, index
    .undefine cmp
    .if .paramcount > 1
        cmp operand, index
    .else
        ___arraySyntax cmp, operand
    .endif
    .define cmp macro_cmp
.endmacro

.macro macro_dec operand, index
    .undefine dec
    .if .paramcount > 1
        dec operand, index
    .else
        ___arraySyntax dec, operand
    .endif
    .define dec macro_dec
.endmacro

.macro macro_eor operand, index
    .undefine eor
    .if .paramcount > 1
        eor operand, index
    .else
        ___arraySyntax eor, operand
    .endif
    .define eor macro_eor
.endmacro

.macro macro_inc operand, index
    .undefine inc
    .if .paramcount > 1
        inc operand, index
    .else
        ___arraySyntax inc, operand
    .endif
    .define inc macro_inc
.endmacro

.macro macro_lsr operand, index
    .undefine lsr
    .if .paramcount > 1
        lsr operand, index
    .else
        ___arraySyntax lsr, operand
    .endif
    .define lsr macro_lsr
.endmacro

.macro macro_ora operand, index
    .undefine ora
    .if .paramcount > 1
        ora operand, index
    .else
        ___arraySyntax ora, operand
    .endif
    .define ora macro_ora
.endmacro

.macro macro_rol operand, index
    .undefine rol
    .if .paramcount > 1
        rol operand, index
    .else
        ___arraySyntax rol, operand
    .endif
    .define rol macro_rol
.endmacro

.macro macro_ror operand, index
    .undefine ror
    .if .paramcount > 1
        ror operand, index
    .else
        ___arraySyntax ror, operand
    .endif
    .define ror macro_ror
.endmacro

.macro macro_sbc operand, index
    .undefine sbc
    .if .paramcount > 1
        sbc operand, index
    .else
        ___arraySyntax sbc, operand
    .endif
    .define sbc macro_sbc
.endmacro

.macro macro_bit operand, index
    .undefine bit
    .if .paramcount > 1
        bit operand, index
    .else
        ___arraySyntax bit, operand
    .endif
    .define bit macro_bit
.endmacro

.macro macro_cpx operand, index
    .undefine cpx
    .if .paramcount > 1
        cpx operand, index
    .else
        ___arraySyntax cpx, operand
    .endif
    .define cpx macro_cpx
.endmacro

.macro macro_cpy operand, index
    .undefine cpy
    .if .paramcount > 1
        cpy operand, index
    .else
        ___arraySyntax cpy, operand
    .endif
    .define cpy macro_cpy
.endmacro

; OVERLOAD

.define jmp macro_jmp
.define lda macro_lda
.define sta macro_sta
.define ldx macro_ldx
.define stx macro_stx
.define ldy macro_ldy
.define sty macro_sty
.define adc macro_adc
.define and macro_and
.define asl macro_asl
.define cmp macro_cmp
.define dec macro_dec
.define eor macro_eor
.define inc macro_inc
.define lsr macro_lsr
.define ora macro_ora
.define rol macro_rol
.define ror macro_ror
.define sbc macro_sbc
.define bit macro_bit
.define cpx macro_cpx
.define cpy macro_cpy

.endif
