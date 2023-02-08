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
; names accidentally. This shouldn't be possible unless these macros are inadvertently undefined. 
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

.ifndef ::_CUSTOM_SYNTAX_
::_CUSTOM_SYNTAX_ = 1

.feature ubiquitous_idents ; allow overloading mnemonics

.scope _CUSTOM_
    JMP_INSTRUCTION_COUNTER     .set 0  ; count
    EVALINSTRLISTCOUNT          .set 0  
    OUTPUT_CUSTOM_SYNTAX        .set 0
.endscope

.macro customSyntax_Output opt1
    .if .xmatch( opt1, on )
        _CUSTOM_::OUTPUT_CUSTOM_SYNTAX .set 1
    .elseif .xmatch( opt1, off )
        _CUSTOM_::OUTPUT_CUSTOM_SYNTAX .set 0
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
    _OUT ; output instruction as standard ca65 6502
    .if _CUSTOM_::OUTPUT_CUSTOM_SYNTAX 
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
; It will send what it found to ___arraySyntax, to support extended syntax as described in this file.

.macro ___EvalInstrList statement, index
    ; for a multiple instruction line: if first call to this macro, save who called it with CALLING_MACRO
    ; Example: if called by macro_ldx
    ; CALLING_MACRO will be defined as: macro_ldx ldx
    .if !_CUSTOM_::EVALINSTRLISTCOUNT
        .define CALLING_MACRO .ident(.concat("macro_", .string(.left(1,{statement})))) .left(1,{statement})
    .endif
    _CUSTOM_::EVALINSTRLISTCOUNT .set _CUSTOM_::EVALINSTRLISTCOUNT + 1
    ; split macro calls at colons:
    .local colonPos
    colonPos .set 0
    ___findToken {statement}, :, colonPos
    .if colonPos
        .ifnblank index
            .error "Commas not supported for multiple instructions on one line."
            .fatal ""
        .endif
        ___EvalInstrList .mid(0, colonPos, {statement})
        ___EvalInstrList .mid( colonPos + 1 , .tcount({statement}) - colonPos - 1, {statement} )
    .else
        ; no colon, output macros or instructions
        .if .xmatch (.left(1,{statement}), .left(1, CALLING_MACRO) )    ; is this the calling macro? don't call the macro, call the instruction
            ___arraySyntax .right(1, CALLING_MACRO), { .mid (1, .tcount({statement}) - 1, {statement} ) }, index
        .elseif .definedmacro ( .left(1,{statement})) 
            statement
        .elseif .ismnemonic(.left (1,{statement})) || .xmatch( .left (1,{statement}), adc) ; ca65 bug? .ismnemonic doesn't match 'adc'
            ___arraySyntax .left (1,{statement}), { .mid (1, .tcount({statement}) - 1, {statement} ) }, index
        .else
            .error "Unknown instruction or macro."
        .endif
    .endif
    _CUSTOM_::EVALINSTRLISTCOUNT .set _CUSTOM_::EVALINSTRLISTCOUNT - 1
    .if !_CUSTOM_::EVALINSTRLISTCOUNT
        .undefine CALLING_MACRO
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; special case for JMP:

.macro macro_jmp operand
    .undefine jmp
    jmp operand
    .if _CUSTOM_::OUTPUT_CUSTOM_SYNTAX 
        printTokenList {jmp operand}
    .endif
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
; Allow custom syntax for all instructions

.macro macro_lda operand, index
    .undefine lda
    ___EvalInstrList lda operand, index
    .define lda macro_lda
.endmacro
.macro macro_sta operand, index
    .undefine sta
    ___EvalInstrList sta operand, index
    .define sta macro_sta
.endmacro
.macro macro_ldx operand, index
    .undefine ldx
    ___EvalInstrList ldx operand, index
    .define ldx macro_ldx
.endmacro
.macro macro_stx operand, index
    .undefine stx
    ___EvalInstrList stx operand, index
    .define stx macro_stx
.endmacro
.macro macro_ldy operand, index
    .undefine ldy
    ___EvalInstrList ldy operand, index
    .define ldy macro_ldy
.endmacro
.macro macro_sty operand, index
    .undefine sty
    ___EvalInstrList sty operand, index
    .define sty macro_sty
.endmacro
.macro macro_adc operand, index
    .undefine adc
    ___EvalInstrList adc operand, index
    .define adc macro_adc
.endmacro
.macro macro_and operand, index
    .undefine and
    ___EvalInstrList and operand, index
    .define and macro_and
.endmacro
.macro macro_asl operand, index
    .undefine asl
    ___EvalInstrList asl operand, index
    .define asl macro_asl
.endmacro
.macro macro_cmp operand, index
    .undefine cmp
    ___EvalInstrList cmp operand, index
    .define cmp macro_cmp
.endmacro
.macro macro_dec operand, index
    .undefine dec
    ___EvalInstrList dec operand, index
    .define dec macro_dec
.endmacro
.macro macro_eor operand, index
    .undefine eor
    ___EvalInstrList eor operand, index
    .define eor macro_eor
.endmacro
.macro macro_inc operand, index
    .undefine inc
    ___EvalInstrList inc operand, index
    .define inc macro_inc
.endmacro
.macro macro_lsr operand, index
    .undefine lsr
    ___EvalInstrList lsr operand, index
    .define lsr macro_lsr
.endmacro
.macro macro_ora operand, index
    .undefine ora
    ___EvalInstrList ora operand, index
    .define ora macro_ora
.endmacro
.macro macro_rol operand, index
    .undefine rol
    ___EvalInstrList rol operand, index
    .define rol macro_rol
.endmacro
.macro macro_ror operand, index
    .undefine ror
    ___EvalInstrList ror operand, index
    .define ror macro_ror
.endmacro
.macro macro_sbc operand, index
    .undefine sbc
    ___EvalInstrList sbc operand, index
    .define sbc macro_sbc
.endmacro
.macro macro_bit operand, index
    .undefine bit
    ___EvalInstrList bit operand, index
    .define bit macro_bit
.endmacro
.macro macro_cpx operand, index
    .undefine cpx
    ___EvalInstrList cpx operand, index
    .define cpx macro_cpx
.endmacro
.macro macro_cpy operand, index
    .undefine cpy
    ___EvalInstrList cpy operand, index
    .define cpy macro_cpy
.endmacro
.macro macro_inx operand, index
    .undefine inx
    ___EvalInstrList inx operand, index
    .define inx macro_inx
.endmacro
.macro macro_iny operand, index
    .undefine iny
    ___EvalInstrList iny operand, index
    .define iny macro_iny
.endmacro
.macro macro_bcc operand, index
    .undefine bcc
    ___EvalInstrList bcc operand, index
    .define bcc macro_bcc
.endmacro
.macro macro_bcs operand, index
    .undefine bcs
    ___EvalInstrList bcs operand, index
    .define bcs macro_bcs
.endmacro
.macro macro_beq operand, index
    .undefine beq
    ___EvalInstrList beq operand, index
    .define beq macro_beq
.endmacro
.macro macro_bne operand, index
    .undefine bne
    ___EvalInstrList bne operand, index
    .define bne macro_bne
.endmacro
.macro macro_bpl operand, index
    .undefine bpl
    ___EvalInstrList bpl operand, index
    .define bpl macro_bpl
.endmacro
.macro macro_bmi operand, index
    .undefine bmi
    ___EvalInstrList bmi operand, index
    .define bmi macro_bmi
.endmacro
.macro macro_brk operand, index
    .undefine brk
    ___EvalInstrList brk operand, index
    .define brk macro_brk
.endmacro
.macro macro_bvc operand, index
    .undefine bvc
    ___EvalInstrList bvc operand, index
    .define bvc macro_bvc
.endmacro
.macro macro_bvs operand, index
    .undefine bvs
    ___EvalInstrList bvs operand, index
    .define bvs macro_bvs
.endmacro
.macro macro_clc operand, index
    .undefine clc
    ___EvalInstrList clc operand, index
    .define clc macro_clc
.endmacro
.macro macro_cld operand, index
    .undefine cld
    ___EvalInstrList cld operand, index
    .define cld macro_cld
.endmacro
.macro macro_cli operand, index
    .undefine cli
    ___EvalInstrList cli operand, index
    .define cli macro_cli
.endmacro
.macro macro_clv operand, index
    .undefine clv
    ___EvalInstrList clv operand, index
    .define clv macro_clv
.endmacro
.macro macro_dex operand, index
    .undefine dex
    ___EvalInstrList dex operand, index
    .define dex macro_dex
.endmacro
.macro macro_dey operand, index
    .undefine dey
    ___EvalInstrList dey operand, index
    .define dey macro_dey
.endmacro
.macro macro_jsr operand, index
    .undefine jsr
    ___EvalInstrList jsr operand, index
    .define jsr macro_jsr
.endmacro
.macro macro_nop operand, index
    .undefine nop
    ___EvalInstrList nop operand, index
    .define nop macro_nop
.endmacro
.macro macro_pha operand, index
    .undefine pha
    ___EvalInstrList pha operand, index
    .define pha macro_pha
.endmacro
.macro macro_php operand, index
    .undefine php
    ___EvalInstrList php operand, index
    .define php macro_php
.endmacro
.macro macro_pla operand, index
    .undefine pla
    ___EvalInstrList pla operand, index
    .define pla macro_pla
.endmacro
.macro macro_plp operand, index
    .undefine plp
    ___EvalInstrList plp operand, index
    .define plp macro_plp
.endmacro
.macro macro_rti operand, index
    .undefine rti
    ___EvalInstrList rti operand, index
    .define rti macro_rti
.endmacro
.macro macro_rts operand, index
    .undefine rts
    ___EvalInstrList rts operand, index
    .define rts macro_rts
.endmacro
.macro macro_sec operand, index
    .undefine sec
    ___EvalInstrList sec operand, index
    .define sec macro_sec
.endmacro
.macro macro_sed operand, index
    .undefine sed
    ___EvalInstrList sed operand, index
    .define sed macro_sed
.endmacro
.macro macro_sei operand, index
    .undefine sei
    ___EvalInstrList sei operand, index
    .define sei macro_sei
.endmacro
.macro macro_tax operand, index
    .undefine tax
    ___EvalInstrList tax operand, index
    .define tax macro_tax
.endmacro
.macro macro_tay operand, index
    .undefine tay
    ___EvalInstrList tay operand, index
    .define tay macro_tay
.endmacro
.macro macro_tsx operand, index
    .undefine tsx
    ___EvalInstrList tsx operand, index
    .define tsx macro_tsx
.endmacro
.macro macro_txa operand, index
    .undefine txa
    ___EvalInstrList txa operand, index
    .define txa macro_txa
.endmacro
.macro macro_txs operand, index
    .undefine txs
    ___EvalInstrList txs operand, index
    .define txs macro_txs
.endmacro
.macro macro_tya operand, index
    .undefine tya
    ___EvalInstrList tya operand, index
    .define tya macro_tya
.endmacro

; --------------------------------------------------------------------------------------------
; OVERLOAD
; --------------------------------------------------------------------------------------------

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
