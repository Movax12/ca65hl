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
; This feature allows instructions to be used as macro names. The macros then temporarily undefine themselves to
; allow ca65 to process the instruction normally. Be careful to not declare your own macros with instruction
; names accidentally.
;
; The instruction macros will process their operands through the ___arraySyntax macro that allows
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
; The macro will allow any combination of constant values with either a y or x included, but the index register
; must be first in the square brackets, or the index register must be preceded by a plus (+). The macro then converts 
; it to standard syntax and passes it to ca65 to verify the addressing mode and assemble the instruction.

.ifndef ::_CUSTOM_SYNTAX_
::_CUSTOM_SYNTAX_ = 1

.feature ubiquitous_idents ; allow overloading mnemonics

.scope _CUSTOM_
    JMP_INSTRUCTION_COUNTER .set 0  ; count
    FOUND_COLON             .set 0
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

.macro ___arraySyntax instr, op, index

    .local open
    .local close
    .local reg
    .local regPos
    .local REGX, REGY
    .local colonPos
    REGX = 1
    REGY = 2
    open   .set 0
    close  .set 0
    reg    .set 0
    regPos  .set 0
    colonPos .set 0
    
    ; if index was passed, treat as normal 6502 syntax and quit, since this syntax does not use commas
    .ifnblank index
        .left(1,instr) op, index
        .exitmacro
    .endif
    
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

; --------------------------------------------------------------------------------------------

.macro macro_jmp operand
    seperateColonList jmp, operand
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
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

.macro seperateColonList instr, op, index

    .local colonPos
    colonPos .set 0
    _CUSTOM_::FOUND_COLON .set 0
    ___findToken {op}, :, colonPos
    .if colonPos
        .left(1, instr) .mid(0, colonPos, {op})
        .mid(colonPos + 1, .tcount({op}) -  colonPos - 1, {op})
        _CUSTOM_::FOUND_COLON .set 1
    .elseif .xmatch ( { .left(1, {op}) }, : )
        .left(1, instr)
        .mid(1, .tcount({op}) - 1, {op})
        _CUSTOM_::FOUND_COLON .set 1
    .endif
    
.endmacro
; --------------------------------------------------------------------------------------------
; Allow custom syntax for all modes that use operands:

.macro macro_lda operand, index
    seperateColonList lda, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine lda
    ___arraySyntax lda, operand, index
    .define lda macro_lda
.endmacro

.macro macro_sta operand, index
    seperateColonList sta, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine sta
    ___arraySyntax sta, operand, index
    .define sta macro_sta
.endmacro

.macro macro_ldx operand, index
    seperateColonList ldx, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine ldx
    ___arraySyntax ldx, operand, index
    .define ldx macro_ldx
.endmacro

.macro macro_stx operand, index
    seperateColonList stx, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine stx
    ___arraySyntax stx, operand, index
    .define stx macro_stx
.endmacro

.macro macro_ldy operand, index
    seperateColonList ldy, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine ldy
    ___arraySyntax ldy, operand, index
    .define ldy macro_ldy
.endmacro

.macro macro_sty operand, index
    seperateColonList sty, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine sty
    ___arraySyntax sty, operand, index
    .define sty macro_sty
.endmacro

.macro macro_adc operand, index
    seperateColonList adc, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine adc
    ___arraySyntax adc, operand, index
    .define adc macro_adc
.endmacro

.macro macro_and operand, index
    seperateColonList and, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine and
    ___arraySyntax and, operand, index
    .define and macro_and
.endmacro

.macro macro_asl operand, index
    seperateColonList asl, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine asl
    ___arraySyntax asl, operand, index
    .define asl macro_asl
.endmacro

.macro macro_cmp operand, index
    seperateColonList cmp, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine cmp
    ___arraySyntax cmp, operand, index
    .define cmp macro_cmp
.endmacro

.macro macro_dec operand, index
    seperateColonList dec, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine dec
    ___arraySyntax dec, operand, index
    .define dec macro_dec
.endmacro

.macro macro_eor operand, index
    seperateColonList eor, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine eor
    ___arraySyntax eor, operand, index
    .define eor macro_eor
.endmacro

.macro macro_inc operand, index
    seperateColonList inc, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine inc
    ___arraySyntax inc, operand, index
    .define inc macro_inc
.endmacro

.macro macro_lsr operand, index
    seperateColonList lsr, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine lsr
    ___arraySyntax lsr, operand, index
    .define lsr macro_lsr
.endmacro

.macro macro_ora operand, index
    seperateColonList ora, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine ora
    ___arraySyntax ora, operand, index
    .define ora macro_ora
.endmacro

.macro macro_rol operand, index
    seperateColonList rol, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine rol
    ___arraySyntax rol, operand, index
    .define rol macro_rol
.endmacro

.macro macro_ror operand, index
    seperateColonList ror, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine ror
    ___arraySyntax ror, operand, index
    .define ror macro_ror
.endmacro

.macro macro_sbc operand, index
    seperateColonList sbc, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine sbc
    ___arraySyntax sbc, operand, index
    .define sbc macro_sbc
.endmacro

.macro macro_bit operand, index
    seperateColonList bit, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bit
    ___arraySyntax bit, operand, index
    .define bit macro_bit
.endmacro

.macro macro_cpx operand, index
    seperateColonList cpx, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine cpx
    ___arraySyntax cpx, operand, index
    .define cpx macro_cpx
.endmacro

.macro macro_cpy operand, index
    seperateColonList cpy, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine cpy
    ___arraySyntax cpy, operand, index
    .define cpy macro_cpy
.endmacro

.macro macro_inx operand, index
    seperateColonList inx, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine inx
    ___arraySyntax inx, operand, index
    .define inx macro_inx
.endmacro

.macro macro_iny operand, index
    seperateColonList iny, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine iny
    ___arraySyntax iny, operand, index
    .define iny macro_iny
.endmacro

.macro macro_bcc operand, index
    seperateColonList bcc, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bcc
    ___arraySyntax bcc, operand, index
    .define bcc macro_bcc
.endmacro

.macro macro_bcs operand, index
    seperateColonList bcs, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bcs
    ___arraySyntax bcs, operand, index
    .define bcs macro_bcs
.endmacro

.macro macro_beq operand, index
    seperateColonList beq, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine beq
    ___arraySyntax beq, operand, index
    .define beq macro_beq
.endmacro

.macro macro_bne operand, index
    seperateColonList bne, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bne
    ___arraySyntax bne, operand, index
    .define bne macro_bne
.endmacro

.macro macro_bpl operand, index
    seperateColonList bpl, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bpl
    ___arraySyntax bpl, operand, index
    .define bpl macro_bpl
.endmacro

.macro macro_bmi operand, index
    seperateColonList bmi, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bmi
    ___arraySyntax bmi, operand, index
    .define bmi macro_bmi
.endmacro

.macro macro_brk operand, index
    seperateColonList brk, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine brk
    ___arraySyntax brk, operand, index
    .define brk macro_brk
.endmacro

.macro macro_bvc operand, index
    seperateColonList bvc, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bvc
    ___arraySyntax bvc, operand, index
    .define bvc macro_bvc
.endmacro

.macro macro_bvs operand, index
    seperateColonList bvs, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine bvs
    ___arraySyntax bvs, operand, index
    .define bvs macro_bvs
.endmacro

.macro macro_clc operand, index
    seperateColonList clc, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine clc
    ___arraySyntax clc, operand, index
    .define clc macro_clc
.endmacro

.macro macro_cld operand, index
    seperateColonList cld, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine cld
    ___arraySyntax cld, operand, index
    .define cld macro_cld
.endmacro

.macro macro_cli operand, index
    seperateColonList cli, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine cli
    ___arraySyntax cli, operand, index
    .define cli macro_cli
.endmacro

.macro macro_clv operand, index
    seperateColonList clv, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine clv
    ___arraySyntax clv, operand, index
    .define clv macro_clv
.endmacro

.macro macro_dex operand, index
    seperateColonList dex, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine dex
    ___arraySyntax dex, operand, index
    .define dex macro_dex
.endmacro

.macro macro_dey operand, index
    seperateColonList dey, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine dey
    ___arraySyntax dey, operand, index
    .define dey macro_dey
.endmacro

.macro macro_jsr operand, index
    seperateColonList jsr, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine jsr
    ___arraySyntax jsr, operand, index
    .define jsr macro_jsr
.endmacro

.macro macro_nop operand, index
    seperateColonList nop, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine nop
    ___arraySyntax nop, operand, index
    .define nop macro_nop
.endmacro

.macro macro_pha operand, index
    seperateColonList pha, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine pha
    ___arraySyntax pha, operand, index
    .define pha macro_pha
.endmacro

.macro macro_php operand, index
    seperateColonList php, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine php
    ___arraySyntax php, operand, index
    .define php macro_php
.endmacro

.macro macro_pla operand, index
    seperateColonList pla, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine pla
    ___arraySyntax pla, operand, index
    .define pla macro_pla
.endmacro

.macro macro_plp operand, index
    seperateColonList plp, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine plp
    ___arraySyntax plp, operand, index
    .define plp macro_plp
.endmacro

.macro macro_rti operand, index
    seperateColonList rti, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine rti
    ___arraySyntax rti, operand, index
    .define rti macro_rti
.endmacro

.macro macro_rts operand, index
    seperateColonList rts, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine rts
    ___arraySyntax rts, operand, index
    .define rts macro_rts
.endmacro

.macro macro_sec operand, index
    seperateColonList sec, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine sec
    ___arraySyntax sec, operand, index
    .define sec macro_sec
.endmacro

.macro macro_sed operand, index
    seperateColonList sed, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine sed
    ___arraySyntax sed, operand, index
    .define sed macro_sed
.endmacro

.macro macro_sei operand, index
    seperateColonList sei, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine sei
    ___arraySyntax sei, operand, index
    .define sei macro_sei
.endmacro

.macro macro_tax operand, index
    seperateColonList tax, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine tax
    ___arraySyntax tax, operand, index
    .define tax macro_tax
.endmacro

.macro macro_tay operand, index
    seperateColonList tay, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine tay
    ___arraySyntax tay, operand, index
    .define tay macro_tay
.endmacro

.macro macro_tsx operand, index
    seperateColonList tsx, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine tsx
    ___arraySyntax tsx, operand, index
    .define tsx macro_tsx
.endmacro

.macro macro_txa operand, index
    seperateColonList txa, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine txa
    ___arraySyntax txa, operand, index
    .define txa macro_txa
.endmacro

.macro macro_txs operand, index
    seperateColonList txs, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine txs
    ___arraySyntax txs, operand, index
    .define txs macro_txs
.endmacro

.macro macro_tya operand, index
    seperateColonList tya, operand, index
    .if _CUSTOM_::FOUND_COLON
        .exitmacro
    .endif
    .undefine tya
    ___arraySyntax tya, operand, index
    .define tya macro_tya
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
.define inx macro_inx
.define iny macro_iny

.define bcc macro_bcc
.define bcs macro_bcs
.define beq macro_beq
.define bne macro_bne
.define bpl macro_bpl
.define bmi macro_bmi
.define brk macro_brk
.define bvc macro_bvc
.define bvs macro_bvs
.define clc macro_clc
.define cld macro_cld
.define cli macro_cli
.define clv macro_clv
.define dex macro_dex
.define dey macro_dey
.define jsr macro_jsr
.define nop macro_nop
.define pha macro_pha
.define php macro_php
.define pla macro_pla
.define plp macro_plp
.define rti macro_rti
.define rts macro_rts
.define sec macro_sec
.define sed macro_sed
.define sei macro_sei
.define tax macro_tax
.define tay macro_tay
.define tsx macro_tsx
.define txa macro_txa
.define txs macro_txs
.define tya macro_tya



.endif
