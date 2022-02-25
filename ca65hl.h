; --------------------------------------------------------------------------------------------
; https://mit-license.org/
; Copyright © 2022 big.JT@protonmail.com
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
; Section: ca65hl
; 
; This is a recreation of ca65 macros to allow for some high-level like structured code for branches and loops.
; This code started (rewritten) around Feb 2022
;
; --------------------------------------------------------------------------------------------

; This file is only macro code, intended to add functionality. No memory is used and no
; supporting 6502 code needed.
;
; Some macros are intended to be 'private' to this file. They are are prefixed with a triple underscore.
; 
; Macros for use outside of this file:
;
; setBranch
; setLongBranch
; mb
; if
; else
; elseif
; endif
; do
; while
; repeat
; until
; while <> do
; endwhile
; break

.ifndef _NCA65HL_
_NCA65HL_ = 1

; temporarily define this here. This should be defined in config.h/make file/command line for the project 
DEBUG_H_ON = 0

.include "stacks.h"     ; macros that allow for named stacks 
.include "tokeneval.h"  ; some macros to make token evaluation easier
.include "debug.h"      ; macros to help with debugging

; --------------------------------------------------------------------------------------------
; substitutes for branch mnemonics:
; 'set' or 'clear' can be added after keywords with no 'set' or 'clear'

.define less                C clear
.define greaterORequal      C 
.define carry               C 
.define zero                Z
.define equal               Z
.define plus                N clear
.define positive            N clear
.define minus               N
.define negative            N 
.define bit7                N 
.define overflow            V 
.define bit6                V 
.define bitset              Z clear

.define greater             G        ; Use greater and less/equal macros to simulate
.define lessORequal         G clear

; --------------------------------------------------------------------------------------------
; Branch instructions:
; C, Z, N, V, G flag can be followed by the keyword 'set' or 'clear'.

.define BranchOn_C_set   BCS
.define BranchOn_C_clear BCC
.define BranchOn_Z_set   BEQ
.define BranchOn_Z_clear BNE
.define BranchOn_N_set   BMI
.define BranchOn_N_clear BPL
.define BranchOn_V_set   BVS
.define BranchOn_V_clear BVC
.define BranchOn_G_set   branchOnGreater
.define BranchOn_G_clear branchOnLessOrEqual

.macro branchOnGreater label
    BEQ :+
    BCS label
    :
.endmacro

.macro branchOnLessOrEqual label
    BEQ label
    BCC label
.endmacro

; --------------------------------------------------------------------------------------------
; --------------------------------------------------------------------------------------------
; Branch macros - to define the next branch instruction to be generated

.scope branchSet
    branchDefined .set 0
.endscope

; branch instruction output
; ( use .left to turn .ident into a token list to be recognized as an above .define)

.define Branch (F, S, label) .left(1, .ident( .sprintf("BranchOn_%s_%s", .string(F), .string(S)))) label

; --------------------------------------------------------------------------------------------
; Set the flag to be tested for next branch output, but no checks for previous define
; used in this file: will output an error if define/undefine are not matched

.macro ___setBranch branch
    branchSet::branchDefined .set 1
    setBranchFlag {.left(1,branch)}
    setBranchCondition {.right(1,branch)}
.endmacro

; --------------------------------------------------------------------------------------------
; Set the flag to be tested for next branch output, but ignore if branch is already defined.
; this allows overriding user macro calls to setBranch with inline branch definition via '==', '!='

.macro setBranch branch
    .if .not branchSet::branchDefined
        branchSet::branchDefined .set 1
        setBranchFlag {.left(1,branch)}
        setBranchCondition {.right(1,branch)}
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; individually define branch flag and flag status for next branch

.macro setBranchFlag f
    branchSet::branchDefined .set 1
    .define branchFlag f
.endmacro

.macro setBranchCondition s
    .define branchCondition s
.endmacro

; --------------------------------------------------------------------------------------------
; invert current branch setting

.macro invertBranchCondition 
    .define tempCondition branchCondition
    .undefine branchCondition
    .if .xmatch (tempCondition, set)
        .define branchCondition clear
    .else
        .define branchCondition set
    .endif
    .undefine tempCondition
.endmacro

; --------------------------------------------------------------------------------------------
; clear defines after a branch is output as code

.macro clearBranchSet
    branchSet::branchDefined .set 0
    .undefine branchFlag
    .undefine branchCondition
.endmacro

; --------------------------------------------------------------------------------------------
; symbols to track some values

.scope FLOW_CONTROL_VALUES
    IF_STATEMENT_COUNT                  .set 0  ; if statement label counter - always incremented after every 'if'
    BREAK_STATEMENT_COUNT               .set 0  ; break statement counter
    DO_WHILE_STATEMENT_COUNT            .set 0  ; while loop counter 
    WHILE_DO_ENDWHILE_STATEMENT_COUNT   .set 0  ; while..do endwhile counter
    NEGATE_CONDITION                    .set 0  ; flag: if on, conditions are inverted
    IF_STATEMENT_ACTIVE                 .set 0  ; flag: if executing an 'if' macro (no calling an 'if' while a condition is being processed)
    LONG_JUMP_ACTIVE                    .set 0  ; flag: use JMP to branch
.endscope

; --------------------------------------------------------------------------------------------
; Local macro to match the next token in the token list passed
; it won't find the token in the very first (0) position!
; this macro is used elsewhere, so check if defined first

.if .not .definedmacro( ___findToken )
.macro ___findToken param, tok, position
    .repeat .tcount( {param} ), c
        .if .xmatch( {.mid(c, 1, {param}) }, {tok})
            .if .not position ; do not remove this check
                position .set c
                .exitmacro
            .endif
        .endif
    .endrepeat
.endmacro
.endif

; --------------------------------------------------------------------------------------------
; set the flag for long jumps
; TODO.. warnings for unnecessary long branches 

.macro setLongBranch l, v
    .if .xmatch(l, on) || .xmatch(l, +)
        FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE .set 1
    .elseif .xmatch(l, off) || .xmatch(l, -)
        FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE .set 0
    .else
        .error "Unknown long branch setting."
    .endif
.endmacro 

; --------------------------------------------------------------------------------------------
; Look for '[]' set and allow as an array, possibly with x or y indexed

.macro arraySyntax instr, op
    .local open
    .local close
    .local reg
    .local regPos
    open   .set 0
    close  .set 0
    reg    .set 0
    regPos  .set 0
    printTokenList {Instruction: instr op}
    
    ___findToken {op}, [, open
    .if open
        ___findToken {op}, ], close
        .if close
            ; look for '[ x +' type pattern
            .if .xmatch( {.mid(open + 1, 1 ,{op})}, x)
                reg .set ___math::REGX
            .elseif .xmatch( {.mid(open + 1, 1 ,{op})}, y)
                reg .set ___math::REGY
            .endif
            .if reg
                ; require a +/-, but allow '[y]' or '[x]' if nothing else: 
                ; if only a 'y' or 'x', define the constant as nothing
                .if open + 2 = close
                    .define _CONST 
                    ; next has to be a + or -, or it is an error
                .elseif .xmatch( {.mid(open + 2, 1 ,{op})}, +)
                    .define _CONST + .mid(open + 3, close - open - 3 ,{op})
                .elseif .xmatch( {.mid(open + 2, 1 ,{op})}, -)
                    .define _CONST - .mid(open + 3, close - open - 3 ,{op})
                .else
                    .error "Expected: '+' or '-'"
                .endif
            .else
                ; No reg found yet. Check for '+x' anywhere in the brackets
                ___findToken {op}, x, regPos
                .if regPos && regPos < close
                    reg .set ___math::REGX
                .else
                    ___findToken {op}, y, regPos
                    .if regPos && regPos < close
                        reg .set ___math::REGY
                    .endif
                .endif
                ; found a valid register? to the left must be a +
                .if reg
                    .if ! .xmatch( {.mid(regPos - 1, 1 ,{op})}, +) 
                        .error "Expected: '+'"
                    .endif
                    .define _CONST + .mid(open + 1, regPos - open - 2, {op}) .mid(regPos + 1, close - regPos - 1, {op})
                .else
                    ; no registers, constant is whatever is in the '[]'
                    .define _CONST + .mid(open + 1, close - open - 1 ,{op})
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
    
    ; anything after the '[]' pair? this allows index to be anywhere in the expression, rather than only a the end
    .if (.tcount({op}) > close + 1) && (close > 0 )
        .define _AFTER () .mid(close + 1, .tcount({op}) - close - 1 , {op})
    .else
        .define _AFTER
    .endif
    .if reg = ___math::REGX
        .left(1,instr) .mid(0, open, {op}) _AFTER _CONST, x
    .elseif reg = ___math::REGY                                           
        .left(1,instr) .mid(0, open, {op}) _AFTER _CONST, y
    .else                                                                 
        .left(1,instr) .mid(0, open, {op}) _AFTER _CONST
    .endif
    .undefine _CONST
    .undefine _AFTER
.endmacro

; --------------------------------------------------------------------------------------------
; SECTION: Compare

.scope ___compare
    found       .set 0
    operator    .set 0
    position    .set 0
    .enum
        EQUAL = 1
        NEQUAL
        GREATEREQUAL
        LESSEQUAL
        GREATER
        LESS
    .endenum
.endscope

; --------------------------------------------------------------------------------------------

.macro ___findCompareOperator exp
    
    .local pos
    .local op
    .local bracketCount
    
    ;default:
    pos .set 0
    op .set 0
    bracketCount .set 0
    ___compare::found .set 0
    ___compare::operator .set 0
    ___compare::position .set 0
    .repeat .tcount({exp}), i
    .if ! op
        .if .xmatch ( {.mid(i,1,{exp})}, {(} )
            bracketCount .set bracketCount + 1
        .elseif .xmatch ( {.mid(i,1,{exp})}, {)} ) 
            bracketCount .set bracketCount - 1
        .endif
        .if bracketCount = 0
            .if .xmatch ( {.mid(i,1,{exp})}, = )
                op .set ___compare::EQUAL
            .elseif .xmatch ( {.mid(i,1,{exp})}, <> )
                op .set ___compare::NEQUAL
            .elseif .xmatch ( {.mid(i,1,{exp})}, >= )
                op .set ___compare::GREATEREQUAL
            .elseif .xmatch ( {.mid(i,1,{exp})}, <= )
                op .set ___compare::LESSEQUAL
            .elseif .xmatch ( {.mid(i,1,{exp})}, > )
                op .set ___compare::GREATER
            .elseif .xmatch ( {.mid(i,1,{exp})}, < )
                op .set ___compare::LESS
            .endif
        .endif
        pos .set i
    .endif
    .endrepeat
    .if op
        ___compare::found .set 1
        ___compare::operator .set op
        ___compare::position .set pos
    .endif

.endmacro

; --------------------------------------------------------------------------------------------
; Section : Math

.scope ___math
    regFound .set 0
    .enum
        ADD = 1
        SUB
        ADDC
        SUBC
        MULT
        DIVIDE
        SHR
        SHL
        AND_
        OR 
        EOR_
    .endenum
    .enum
        REGA = 1
        REGX
        REGY
    .endenum
.endscope

; --------------------------------------------------------------------------------------------

.macro ___findMathOperator exp, pos, op

    .local bracketCount
    .local sqBracketCount
    
    ;default:
    bracketCount .set 0
    sqBracketCount .set 0

    .repeat .tcount({exp}), i
    .if .not op
        .if .xmatch ( {.mid(i,1,{exp})}, {[} )
            sqBracketCount .set sqBracketCount + 1
        .elseif .xmatch ( {.mid(i,1,{exp})}, {]} ) 
            sqBracketCount .set sqBracketCount - 1
        .endif
        .if sqBracketCount = 0
            .if .xmatch ( {.mid(i,1,{exp})}, {(} )
                bracketCount .set bracketCount + 1
            .elseif .xmatch ( {.mid(i,1,{exp})}, {)} ) 
                bracketCount .set bracketCount - 1
            .endif
            .if bracketCount = 0
                .if .xmatch ( {.mid(i,1,{exp})}, + ) && ( .xmatch ( {.mid(i + 1, 1, {exp})}, c ) || .xmatch ( {.mid(i + 1, 1, {exp})}, C ) )
                    op .set ___math::ADDC
                .elseif .xmatch ( {.mid(i,1,{exp})}, + )
                    op .set ___math::ADD
                .elseif .xmatch ( {.mid(i,1,{exp})}, - ) && ( .xmatch ( {.mid(i + 1, 1, {exp})}, c ) || .xmatch ( {.mid(i + 1, 1, {exp})}, C ) )
                    op .set ___math::SUBC
                .elseif .xmatch ( {.mid(i,1,{exp})}, - )
                    op .set ___math::SUB
                .elseif .xmatch ( {.mid(i,1,{exp})}, * )
                    op .set ___math::MULT
                .elseif .xmatch ( {.mid(i,1,{exp})}, / )
                    op .set ___math::DIVIDE
                .elseif .xmatch ( {.mid(i,1,{exp})}, >> )
                    op .set ___math::SHR
                .elseif .xmatch ( {.mid(i,1,{exp})}, << )
                    op .set ___math::SHL
                .elseif .xmatch ( {.mid(i,1,{exp})}, & )
                    op .set ___math::AND_
                .elseif .xmatch ( {.mid(i,1,{exp})}, | )
                    op .set ___math::OR
                .elseif .xmatch ( {.mid(i,1,{exp})}, ^ )
                    op .set ___math::EOR_
                .endif
            .endif
        .endif
        pos .set i
    .endif
    .endrepeat
    .if ! op
        pos .set 0
    .endif
    
.endmacro

; --------------------------------------------------------------------------------------------

.macro ___evalMathOp exp, register

    .local op1
    .local op2
    .local pos1
    .local pos2
    .local carryOp
    .local reg
    op1      .set 0
    op2      .set 0
    pos1     .set 0
    pos2     .set 0
    carryOp  .set 0
    reg      .set 0
    
    ___math::regFound .set 0
    printTokenList {Math eval: exp}
    ; default:
    reg .set ___math::REGA
    .ifnblank register
        reg .set register
    .endif
   
    ___findMathOperator {exp}, pos1, op1
    carryOp .set ((op1 = ___math::ADDC) || (op1 = ___math::SUBC)) 
    .if op1
        ___findMathOperator { .mid(pos1 + 1 + carryOp, .tcount({exp}) - pos1 - 1 - carryOp, {exp} ) }, pos2, op2
    .endif
    
    ; if another op found, adjust pos2, set token count for this evaluation to stop at pos2
    .if op2
        pos2 .set pos1 + pos2 + 1 + carryOp
        tcount .set pos2
    .else 
        tcount .set .tcount({exp})
    .endif
    
    .define _RIGHT_EXP .mid(pos1 + 1 + carryOp, tcount - pos1 - 1 - carryOp, {exp})
    
      ; override register to A if the following:
    .if tcount > 1 && ( reg = ___math::REGX || reg = ___math::REGY ) && ( op1 > 2 || (!.match(_RIGHT_EXP, 1)) )  ; ADD and SUB are 1, 2
        reg .set ___math::REGA
    .endif
    
     .if reg = ___math::REGA
        .define _LOAD lda
    .elseif reg = ___math::REGX
        .define _LOAD ldx
    .elseif reg = ___math::REGY
        .define _LOAD ldy
    .endif
    
    ; if no registers explicitly defined before the op and something to load:
    .if ! (.xmatch ({ .left(1,{exp}) }, a) || .xmatch ({ .left(1,{exp}) }, x) || .xmatch ({ .left(1,{exp}) }, y) )
        .if (op1 && ( pos1 > 0))
            arraySyntax _LOAD, {.mid(0, pos1, {exp})}
        .elseif ! op1
            ; if no operation, then something to load
            arraySyntax _LOAD, {exp}
        .endif
    .endif
    
    .if reg = ___math::REGX
        .if op1 = ___math::ADD
            .repeat _RIGHT_EXP
                inx
                printTokenList inx
            .endrepeat
        .elseif op1 = ___math::SUB
            .repeat _RIGHT_EXP
                dex
                printTokenList dex
            .endrepeat            
        .endif
    .elseif reg = ___math::REGY
        .if op1 = ___math::ADD
            .repeat _RIGHT_EXP
                iny
                printTokenList iny
            .endrepeat
        .elseif op1 = ___math::SUB
            .repeat _RIGHT_EXP
                dey
                printTokenList dey
            .endrepeat
        .endif       
    ; if reg = ___math::REGA :
    .elseif op1 = ___math::ADDC
        .if pos1 + carryOp + 1 = .tcount({exp})
            adc #0
            printTokenList adc #0
        .else
            arraySyntax adc, {_RIGHT_EXP}
        .endif
    .elseif op1 = ___math::SUBC 
        .if pos1 + carryOp + 1 = .tcount({exp})
            sbc #0
            printTokenList sbc #0
        .else
            arraySyntax sbc, {_RIGHT_EXP}
        .endif
    .elseif op1 = ___math::SUB
        sec
        arraySyntax sbc, {_RIGHT_EXP}
    .elseif op1 = ___math::ADD
        clc
        arraySyntax adc, {_RIGHT_EXP}
    .elseif op1 = ___math::SHL
        .repeat _RIGHT_EXP
            asl a
            printTokenList asl
        .endrepeat
    .elseif op1 = ___math::SHR
        .repeat _RIGHT_EXP
            lsr a
            printTokenList lsr
        .endrepeat
    .elseif op1 = ___math::AND_
        arraySyntax and, {_RIGHT_EXP}
    .elseif op1 = ___math::OR
        arraySyntax ora, {_RIGHT_EXP}
    .elseif op1 = ___math::EOR_
        arraySyntax eor, {_RIGHT_EXP}
    .endif
    .undefine _RIGHT_EXP
    .undefine _LOAD
    
    .if op2
        ___evalMathOp { .mid(pos2, .tcount({exp}) - pos2, {exp}) }, register
    .else
       ___math::regFound .set reg ; signal back which register was used for any operation
    .endif
    
.endmacro

; --------------------------------------------------------------------------------------------;
; called by IF when compare operator found in a statement
; compare::operator and compare::position must be set first
; 
.macro ___compareM exp

    .local left
    .local right
    .define _LEFT ()  .mid (0, ___compare::position, {exp})
    .define _RIGHT () .mid (___compare::position + 1, .tcount({exp}) - ___compare::position - 1, {exp})
    
    ; first look for a register 
    ; check if register on the right side. Left is always the register.
    left .set 0
    .if .xmatch (_RIGHT, a)
        left .set ___math::REGA
    .elseif .xmatch (_RIGHT, x)
        left .set ___math::REGX
    .elseif .xmatch (_RIGHT, y)
        left .set ___math::REGY
    .endif

    ; if we found a register on the right side, pretend it is on the left by switching the compare and the defines
    .if left        
        .if ___compare::operator = ___compare::GREATER
            ___compare::operator .set ___compare::LESS
        .elseif ___compare::operator = ___compare::GREATEREQUAL
            ___compare::operator .set ___compare::LESSEQUAL
        .endif
        ; switch sides
        .define _TEMPRIGHT _RIGHT
        .undefine _RIGHT
        .define _RIGHT _LEFT
        .undefine _LEFT
        .define _LEFT _TEMPRIGHT
        .undefine _TEMPRIGHT
    .else
        .if .xmatch (_LEFT, a)
            left .set ___math::REGA
        .elseif .xmatch (_LEFT, x)
            left .set ___math::REGX
        .elseif .xmatch (_LEFT, y)
            left .set ___math::REGY
        .endif
    .endif
    ; no registers found, so first we will load a register, or eval an expression
    .if ! left
        .define _LEFT1 .left (1, {_LEFT})
        .if .match( _LEFT1, abc) 
            .if .ismnemonic(_LEFT1)
                arraySyntax _LEFT1, {.right( .tcount({_LEFT}) - 1, {_LEFT} )}
                .if .xmatch(_LEFT1,lda)
                    left .set ___math::REGA
                .elseif .xmatch(_LEFT1,ldx)
                    left .set ___math::REGX
                .elseif .xmatch(_LEFT1,ldy)
                    left .set ___math::REGY
                .elseif .xmatch(_LEFT1,inx)
                    left .set ___math::REGX
                .elseif .xmatch(_LEFT1,dex)
                    left .set ___math::REGX
                .elseif .xmatch(_LEFT1,iny)
                    left .set ___math::REGY
                .elseif .xmatch(_LEFT1,dey)
                    left .set ___math::REGY
                .endif
            .endif
        .endif
        .if ! left
            ___evalMathOp {_LEFT}
            left .set ___math::regFound
        .endif
        .undefine _LEFT1
    .endif
    
    .if .not left
       .error "Unknown register to use in comparison macro."
       .exitmacro
    .endif
    
    .if left = ___math::REGA
        arraySyntax cmp, {_RIGHT}
    .elseif left = ___math::REGX
        arraySyntax cpx, {_RIGHT}
    .elseif left = ___math::REGY
        arraySyntax CPY, {_RIGHT}
    .endif
    .if ___compare::operator     = ___compare::GREATER
        ___setBranch G set
    .elseif ___compare::operator = ___compare::LESS
        ___setBranch C clear
    .elseif ___compare::operator = ___compare::EQUAL
        ___setBranch Z set
    .elseif ___compare::operator = ___compare::GREATEREQUAL
        ___setBranch C set
    .elseif ___compare::operator = ___compare::LESSEQUAL
        ___setBranch G clear
    .elseif ___compare::operator = ___compare::NEQUAL
        ___setBranch Z clear
    .endif
    
    .undefine _LEFT 
    .undefine _RIGHT
.endmacro

; --------------------------------------------------------------------------------------------
; mb - Move Byte
; move a byte, possibly through some other instructions for basic add/sub or bit-wise operations
; register is optional - it indicates which register to use. If it is omitted, 
; register to use will be searched for, or register A will be used
; by default. (The macro will adjust arguments if exp is empty).

.macro mb register, exp

    .local rightReg
    .local leftReg
    .local pos
    rightReg    .set 0
    leftReg     .set 0    
    pos .set 0
    .ifblank exp
        .define _EXP () register
    .else
        ; register passed 'register':
        .define _EXP () exp
        .if .xmatch(register, a)
            rightReg .set ___math::REGA
        .elseif .xmatch(register, x)
            rightReg .set ___math::REGX
        .elseif .xmatch(register, y)
            rightReg .set ___math::REGY
        .else
            .error "Unknown register."
        .endif
    .endif
    printTokenList {MB_:REG_: register | EXP_:exp}	

    ; find assignment token:
    ___findToken {_EXP}, =, pos
    .if ! pos
        ___findToken {_EXP}, :=, pos
    .endif
    .if ! pos
        .error "No assignment."
    .endif
    
    .define _LEFT .mid(0, pos, {_EXP})
    .define _RIGHT .mid(pos + 1, .tcount({_EXP}) - pos - 1, {_EXP})
    
    ; find left register
    .if .xmatch(.left(1, {_LEFT}), a) || .xmatch(.left(1, {_LEFT}), a:)
        leftReg .set ___math::REGA
    .elseif .xmatch(.left(1, {_LEFT}), x)
        leftReg .set ___math::REGX
    .elseif .xmatch(.left(1, {_LEFT}), y)
        leftReg .set ___math::REGY
    .endif
    
    .if ! rightReg ; if no register defined for move yet, see if it is explicitly used on the right side
        .if .xmatch(.left(1, {_RIGHT}), a)
            rightReg .set ___math::REGA
        .elseif .xmatch(.left(1, {_RIGHT}), x)
            rightReg .set ___math::REGX
        .elseif .xmatch(.left(1, {_RIGHT}), y)
            rightReg .set ___math::REGY
        .endif
        ; no match on right, use any register that may have been found on the left
        .if !rightReg
           rightReg .set leftReg
        .endif
    .endif
    
    ; look for any simple math operations on the right side, 
    ; call will output any LOAD first, evaluate any operations
    ; from left to right
    .if rightReg
        ___evalMathOp {_RIGHT}, rightReg
    .else
        ___evalMathOp {_RIGHT}
    .endif
    ; override right side reg if ___evalMathOp changed/set it:
    rightReg .set ___math::regFound
    
    .if ! leftReg ; no register defined? just store: 
        .if rightReg = ___math::REGA
            arraySyntax sta, {_LEFT}
        .elseif rightReg = ___math::REGX
            arraySyntax stx, {_LEFT}
        .elseif rightReg = ___math::REGY
            arraySyntax sty, {_LEFT}
        .endif
    .elseif leftReg = ___math::REGA
        .if rightReg = ___math::REGX
            txa
        .elseif rightReg = ___math::REGY
            tya
        .endif
    .elseif leftReg = ___math::REGX
        .if rightReg = ___math::REGA
            tax
        .elseif rightReg = ___math::REGY
            .error "Cannot do 'x := y' type move."
        .endif
    .elseif leftReg = ___math::REGY
        .if rightReg = ___math::REGA
            tay
        .elseif rightReg = ___math::REGX
            .error "Cannot do 'y := x' type move."
        .endif
    .endif
    
   .undefine _LEFT
   .undefine _RIGHT
   .undefine _EXP
.endmacro

; --------------------------------------------------------------------------------------------
; Recursively evaluate statements: One or more instructions or macros as well as supported comparisions
; and simple match evaluations.

.macro ___evaluateStatementList statement

    ; define and then work with one parameter at a time
    .local colonPos
    colonPos .set 0
    ___findToken {statement}, :, colonPos
    .if colonPos
        .define _S_() .mid(0, colonPos, {statement})
    .else
        .define _S_() statement
    .endif
    printTokenList {Statement: _S_}
    ; -------------------------------------
    ; first: special case - is a register
    .if .xmatch( .left (1,{_S_}), a) || .xmatch( .left (1,{_S_}), x) || .xmatch( .left (1,{_S_}), y)
        ___findCompareOperator {_S_}
        .if ___compare::found
            ___compareM {_S_}
        .else
            ___evalMathOp {_S_}
        .endif
    .else 
        ; if it is a macro, just call the macro:
        .if .definedmacro ( .left(1,{_S_}))
            printTokenList {Macro: _S_}
            .left (1,{_S_})  .mid (1, .tcount({_S_}) - 1, {_S_} ) 
        .else
            ; first check for operators that indicate comparison : > < <> >= <=
            ___findCompareOperator {_S_}
            .if ___compare::found
                ___compareM {_S_}
            .elseif .ismnemonic(.left (1,{_S_})) || .xmatch( .left (1,{_S_}), adc) ; ca65 bug? .ismnemonic doesn't match 'adc'
                arraySyntax .left (1,{_S_}), {.mid (1, .tcount({_S_}) - 1, {_S_} )}
            .else
                ___evalMathOp {_S_}
            .endif
        .endif
    .endif
    ; -------------------------------------
    .undefine _S_
      ; Repeat with next if there was a colon found:
    .if colonPos
        ___evaluateStatementList { .mid ( colonPos + 1 , .tcount({statement}) - colonPos - 1, {statement} ) }
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Establish what branch to generate for one or more instructions/macros.
; call ___evaluateStatementList to execute any instructions or macros, set flags for branch before exit

.macro ___evaluateBranch statement

    .local statementTokenCount
    .local inlineBranchSetPos
    .local negateFlag
    ;defaults:
    statementTokenCount .set .tcount({statement})
    inlineBranchSetPos .set 0
    negateFlag .set -1          ; temporarily use -1 to mean 'not found'
    ; find possible '==', or '!=' for setting a branch
    .if .tcount({statement}) > 2
        ___findToken {statement}, !, inlineBranchSetPos
        .if inlineBranchSetPos
            .if .xmatch ( .mid (inlineBranchSetPos + 1, 1, {statement}) , = )
                negateFlag .set 1
            .endif
        .endif
        .if negateFlag = -1 ; nothing found yet
            inlineBranchSetPos .set 0
            ___findToken {statement}, =, inlineBranchSetPos
            .if inlineBranchSetPos
                .if .xmatch ( .mid (inlineBranchSetPos + 1, 1, {statement}) , = )
                    negateFlag .set 0
                .endif
            .endif    
        .endif
        .if negateFlag > -1
            statementTokenCount .set inlineBranchSetPos
            inlineBranchSetPos .set inlineBranchSetPos + 2 ; position was at first '=' so add 2 to get to the branch condition
        .else
            inlineBranchSetPos .set 0
        .endif
    .endif
    
    ; find branch setting - either first token or after '==', '!=' 
    ; check branch setting is valid: if branch setting with 2 tokens, and no '==', '!=' then done
    .define _flagMatch (f) .xmatch ({.mid(inlineBranchSetPos, 1, {statement})}, {f} ) 
    .if _flagMatch {C} || _flagMatch {Z} || _flagMatch {N} || _flagMatch {V} || _flagMatch {G}
        ; if 2 tokens for flag set:
        .if .tcount({statement}) - inlineBranchSetPos = 2
            .if .xmatch ( .mid(inlineBranchSetPos + 1, 1, {statement}), set) || .xmatch ( .mid(inlineBranchSetPos + 1, 1, {statement}), clear)
                ___setBranch .mid(inlineBranchSetPos, 2, {statement})
            .else
                .error "Expected: 'set or 'clear'."
                .fatal "STOP"
            .endif
        .elseif .tcount({statement}) - inlineBranchSetPos = 1
            setBranchFlag .mid(inlineBranchSetPos, 1, {statement})
            setBranchCondition set
        .else
            .error "Branch parameter error."
            .fatal "STOP"
        .endif
        .if negateFlag = 1
            invertBranchCondition
        .endif
    .endif
    .undefine _flagMatch
    
    ; branch condition possibly defined now. If it is not, or there is a '==', '!=' , that means there is something to execute
    ; (if a macro call tries to set the flags it will not be honored if flags already set via inlineBranchSetPos)
    .if (.not branchSet::branchDefined ) || inlineBranchSetPos
        ___evaluateStatementList {.mid(0, statementTokenCount, {statement})}
    .endif
    ; still no branch? default to non-zero is 'true'
    .if .not branchSet::branchDefined
        ___setBranch Z clear
    .endif
    
.endmacro

; --------------------------------------------------------------------------------------------
; macro for helping to evaluate || and && tokens with a possible negate applied to a bracket set
; from inside .macro 'if'

.macro ___xmatchSpecial token, match, negateBracketSet
    match .set 0        ; set false by default
    .if .xmatch ({token} , {&&})        ; want to try and find an &&
        .if negateBracketSet ; are we inside a negated bracket set?
            .if xmatchToken {||}
                match .set 1
            .endif
        .else 
            .if xmatchToken {&&}
                match .set 1
            .endif
        .endif
    .elseif .xmatch ({token}, {||})
        .if negateBracketSet ; inside a negated bracket set?
            .if xmatchToken {&&}
                match .set 1
            .endif
        .else 
            .if xmatchToken {||}
                match .set 1
            .endif
        .endif
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; The core of functionality for this file. Evaluates a condition and 
; generates branches. Calls other macros to process statements to code output.

.macro if condition
    
    printTokenList {Branch_Statement: condition}
    
    ; compatibility for older code that doesn't have surrounding braces
    ; try to help it out by adding some
    .if .not .xmatch (.left(1,{condition}), {(})
        .local brace
        brace .set 0
        ;.warning "Need ("
        ___findToken {condition}, goto, brace
        .if !brace
            ___findToken {condition}, break, brace
        .endif
        .if brace
            if ( .left(brace, {condition}) ) .mid(brace, .tcount({condition}) - brace, {condition})
        .else 
            if ( condition )
        .endif    

       .exitmacro
    .endif

    .if FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE
        .error "Cannot use 'if' statement in conditional expression"
        .fatal "STOP"
    .endif
    FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE .set 1
    
    .local ifEndIfCodeBlockStart_IfGotoFail ;  branch to this label on failed condition with a goto statement, or pass condition with a code block defined by if..endif
    .local longJumpToEndIf_IfGotoPass       ;  for long jump: branch to this label on failed condition with an if..endif statement, or pass condition with goto
    .local negateNext                       ;  flag: if single branch term to be negated              
    .local negateBracketSet                 ;  flag: if a set of terms in brackets to be negated
    .local bracketLevel                     ;  level of brackets we are in, lowest is 1
    .local branchLabelCounter               ;  count of how many branch labels to additional OR/AND conditions needed
    .local foundTokenPosition               ;  save token position of found && or || tokens when performing look-ahead evaluating correct branch
    .local lastCloseBracketPos              ;  save position of end of test condition ( could be goto/break statement after)
    .local gotoUserLabel                    ;  flag: if 'goto' found, branch to label passed in <condition>
    .local gotoBreakLabel                   ;  flag: branch to break label to exit loop
    .local scanAheadBracketLevel            ;  bracket level we are on when scanning ahead
    .local foundValidAND                    ;  flag: found an && when scanning ahead while considering if bracket set is inverted
    .local foundValidOR                     ;  flag: found an || when scanning ahead while considering if bracket set is inverted
    .local exitedBracketSetLevel            ;  when evaluating look-ahead, save the bracket level if exiting the condition's bracket level
    .local scanAheadNegateBrackets          ;  negate status for brackets when scanning ahead
    .local foundOR_AND                      ;  flag: matched a AND or OR when scanning ahead
    .local statementStartPos                ;  token position for start of found statement
    .local statementTokenCount              ;  token count for found statement
    
    negateBracketSet        .set FLOW_CONTROL_VALUES::NEGATE_CONDITION ; when set this will negate the entire condition
    
    negateNext              .set 0
    bracketLevel            .set 0
    branchLabelCounter      .set 0
    foundTokenPosition      .set 0
    lastCloseBracketPos     .set 0
    gotoUserLabel           .set 0
    gotoBreakLabel          .set 0
    scanAheadBracketLevel   .set 0
    foundValidAND           .set 0
    foundValidOR            .set 0
    negateStackCounter      .set 0
    exitedBracketSetLevel   .set 0
    scanAheadNegateBrackets .set 0
    
    ; array for label locations: (use global to reuse ident)
    .define tokenPositionForBranchLabel(c)  ::.ident(.sprintf("_BRANCH_LABEL_%02X", c))    
    
    startTokenListEval {condition}    ; use token macros to make processing tokens a bit easier
    ; --------------------------------------------------------------------------------------------
    ; verify brackets:
    saveTokenListPosition
    previousToken             ; step back before first token
    verifyNextToken {(}       ; make sure to start with a '('
    nextToken
    allowAllTokens
    .repeat .tcount({condition})
    .if .not EOT
        .if xmatchToken {(}
            bracketLevel .set bracketLevel + 1
        .elseif xmatchToken {)}
            bracketLevel .set bracketLevel - 1
            lastCloseBracketPos .set currentTokenNumber
        .endif
        .if bracketLevel < 0
            .error "Bad brackets."
            .fatal "STOP"
        .endif
        nextToken
    .endif
    .endrepeat
    restoreTokenListPosition
    .if bracketLevel <> 0
        .error "Bad brackets."
        .fatal "STOP"
    .endif
    ; --------------------------------------------------------------------------------------------
    ; find if there is a goto keyword:
    ; if there is, set the successful condition to branch to the passed label
    .if lastCloseBracketPos + 1 < .tcount({condition})
        .if .xmatch( .mid(lastCloseBracketPos + 1, 1, {condition}), goto )
            gotoUserLabel .set 1
            .define conditionFailLabel ifEndIfCodeBlockStart_IfGotoFail
            .if FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE
                .define conditionPassLabel longJumpToEndIf_IfGotoPass
            .else
                .define conditionPassLabel .mid(lastCloseBracketPos + 2, .tcount({condition}) - lastCloseBracketPos - 2, {condition}) ; capture everything after the 'goto'
            .endif
        .elseif .xmatch( .mid(lastCloseBracketPos + 1, 1, {condition}), break )
            ___checkBreakInsideLoop               ; valid break?  
            gotoBreakLabel .set 1
            .define conditionFailLabel ifEndIfCodeBlockStart_IfGotoFail
            .if FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE
                .define conditionPassLabel longJumpToEndIf_IfGotoPass
            .else
                .define conditionPassLabel .ident( .sprintf( "BREAK_STATEMENT_LABEL_%04X", FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT))
            .endif
        .else
            .error "'goto' or 'break' expected."
        .endif
        setTokenCount lastCloseBracketPos + 1 ; set max tokens for EOT to exclude the goto and label
    .else
        .define conditionPassLabel ifEndIfCodeBlockStart_IfGotoFail
        .if FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE
            .define conditionFailLabel longJumpToEndIf_IfGotoPass
        .else
            .define conditionFailLabel .ident( .sprintf( "_IF_STATEMENT_ENDIF_LABEL_%04X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT ))
        .endif
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; main loop: evaluate branches and AND OR conditions
    ; loop over all tokens, but do nothing if at EOT
    .repeat .tcount({condition})
    .if !EOT
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken {!}
            negateNext .set ! negateNext           
            verifyNextToken { ! abc a x y ( :: }
            nextToken
        ; --------------------------------------------------------------------------------------------
        .elseif xmatchToken {(}
            bracketLevel .set bracketLevel + 1
            stackPush "_IF_NEGATE_STACK_", negateBracketSet
            negateBracketSet .set negateNext ^ negateBracketSet
            negateNext .set 0
            verifyNextToken { ! abc a x y  ( :: }
            nextToken
        ; --------------------------------------------------------------------------------------------
        .elseif xmatchToken {)}
            bracketLevel .set bracketLevel - 1
            stackPop "_IF_NEGATE_STACK_", negateBracketSet
            verifyNextToken { ) && || }
            nextToken
        ; --------------------------------------------------------------------------------------------
        .elseif xmatchToken {||} || xmatchToken {&&}
            ; see if we need a label here:
            .repeat branchLabelCounter, i   ; branchLabelCounter starts at 0 and is post incremented for the next (yet to be defined) index
                .if tokenPositionForBranchLabel{i} = currentTokenNumber
                    .ifndef .ident( .sprintf( "IF_LABEL_%04X_BRANCH_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, tokenPositionForBranchLabel{i} ))
                        .ident( .sprintf( "IF_LABEL_%04X_BRANCH_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, tokenPositionForBranchLabel{i} )):
                    .endif
                .endif
            .endrepeat
            verifyNextToken { ! abc a x y ( :: }
            nextToken
        ; --------------------------------------------------------------------------------------------
        .elseif matchToken {abc} || xmatchToken{a} || xmatchToken{x} || xmatchToken{y} || matchToken {::} ; something that could be an identifier, register, or branch setting
        
            ; find statementStartPos and statementTokenCount
            ; find end of statement, but ignore anything in ()
            statementStartPos .set currentTokenNumber
            allowAllTokens
            scanAheadBracketLevel .set 0    ; use as temp
            .repeat .tcount({condition}) - currentTokenNumber
            .if ( !EOT ) && ((!xmatchToken {)}) || ( scanAheadBracketLevel > 0 )) && (!xmatchToken {||}) && (!xmatchToken {&&})
                .if xmatchToken {(}
                    scanAheadBracketLevel .set scanAheadBracketLevel + 1
                .endif
                .if xmatchToken {)} && (scanAheadBracketLevel > 0)
                    scanAheadBracketLevel .set scanAheadBracketLevel - 1
                .endif
                nextToken
            .endif
            .endrepeat
            statementTokenCount .set currentTokenNumber - statementStartPos
            previousToken
            verifyNextToken { ) || && }
            nextToken
            ; evaluate the statement(s) and determine the branch 
            ___evaluateBranch {.mid(statementStartPos, statementTokenCount, {condition})}
    
            .if negateNext^negateBracketSet
                invertBranchCondition
            .endif
            negateNext .set 0
            
            ; save, skip ahead in the token list temporarily:
            allowAllTokens              
            saveTokenListPosition
            saveStackPointer "_IF_NEGATE_STACK_"
            
            foundTokenPosition .set 0
            scanAheadBracketLevel .set bracketLevel
            scanAheadNegateBrackets .set negateBracketSet
            exitedBracketSetLevel .set 0
            foundOR_AND .set 0
            ; skip immediate and repeated closed braces: eg. for 'N set' (marked in quotes): if (( C set || N set')' && V set)
            .repeat .tcount({condition}) - currentTokenNumber
            .if (!EOT) && xmatchToken {)}
                scanAheadBracketLevel .set scanAheadBracketLevel - 1
                exitedBracketSetLevel .set scanAheadBracketLevel
                nextToken
            .endif
            .endrepeat
            ; --------------------------------------------------------------------------------------------
            ; where to branch to depends on next token
            ___xmatchSpecial {&&}, foundValidAND, scanAheadNegateBrackets ; special token match, considers negated bracket set
            .if foundValidAND
                ; Example: C set && V set || Z set
                ;  Pass: C is set: do not branch
                ;  Fail: (branch on inverted condition) branch to next || in this bracket level
                ; If no || then check for lower bracket level:
                ;  Pass: do not branch
                ;  Fail:(invert condition) branch to next || on next lower bracket level
                ; If no || then overall condition is fail, branch to endif or exit test (goto) on the inverted condition
                
                ; look ahead in token list without disturbing the current state:
                nextToken ; skip && token, but not skipped for main loop
                .repeat .tcount({condition}) - currentTokenNumber
                .if (!EOT) && (!foundTokenPosition)
                    .if xmatchToken {(}
                        scanAheadBracketLevel .set scanAheadBracketLevel + 1
                        stackPush "_IF_NEGATE_STACK_", scanAheadNegateBrackets   ; this value doesn't matter, but keeps the stack in order
                    .elseif xmatchToken {)}
                        scanAheadBracketLevel .set scanAheadBracketLevel - 1
                        .if scanAheadBracketLevel < bracketLevel
                            .if !exitedBracketSetLevel
                                exitedBracketSetLevel .set scanAheadBracketLevel
                            .endif
                        .endif
                        stackPop "_IF_NEGATE_STACK_", scanAheadNegateBrackets
                    .else
                        ___xmatchSpecial {||}, foundOR_AND, scanAheadNegateBrackets
                        .if foundOR_AND
                            ; did we exit the brackets? if yes then any valid OR for branch must be in a lower set of brackets
                            .if exitedBracketSetLevel
                                .if scanAheadBracketLevel <= exitedBracketSetLevel
                                    foundTokenPosition .set currentTokenNumber
                                .endif
                            .elseif scanAheadBracketLevel = bracketLevel
                                foundTokenPosition .set currentTokenNumber
                            .endif
                        .endif
                    .endif
                    nextToken
                .endif
                .endrepeat
                invertBranchCondition
            ; --------------------------------------------------------------------------------------------
            ; END .if xmatchToken {&&}
            .else
                ___xmatchSpecial {||}, foundValidOR, scanAheadNegateBrackets ; special token match, considers negated bracket set
                .if foundValidOR
                    ; Example: C set || V set && Z set
                    ;  Pass: C is set: branch to next && in this bracket level:
                    ;       NOTE: This is correct for left to right, to allow && to have priority only branch to && on lower bracket level:
                    ;  Fail: do not branch
                    ; If no && then check for lower bracket level:
                    ;  Pass: C is set: branch to next && on next lower bracket level
                    ;  Fail: do not branch
                    ;  If no && then overall condition is pass, branch to code block start, or goto label for pass condition
                    
                    ; look ahead in token list without disturbing the current state:
                    nextToken ; skip || token, but not skipped for main loop
                    .repeat .tcount({condition}) - currentTokenNumber
                    .if (!EOT) && (!foundTokenPosition)
                        .if xmatchToken {(}
                            scanAheadBracketLevel .set scanAheadBracketLevel + 1
                            stackPush "_IF_NEGATE_STACK_", scanAheadNegateBrackets   ; this value doesn't matter, but keeps the stack in order
                        .elseif xmatchToken {)}
                            scanAheadBracketLevel .set scanAheadBracketLevel - 1
                            .if scanAheadBracketLevel < bracketLevel
                                .if !exitedBracketSetLevel
                                    exitedBracketSetLevel .set scanAheadBracketLevel
                                .endif
                            .endif
                            stackPop "_IF_NEGATE_STACK_", scanAheadNegateBrackets
                        .else
                            ___xmatchSpecial {&&}, foundOR_AND, scanAheadNegateBrackets
                            .if foundOR_AND
                                ; did we exit our brackets? if yes then any valid AND must be in a lower set of brackets
                                .if exitedBracketSetLevel
                                    .if scanAheadBracketLevel <= exitedBracketSetLevel
                                        foundTokenPosition .set currentTokenNumber
                                    .endif
                                ; uncomment for left to right precedence. As commented allows ANDs to have precedence     
                                ;.elseif scanAheadBracketLevel = bracketLevel
                                ;    foundTokenPosition .set currentTokenNumber
                                .endif
                            .endif
                        .endif
                        nextToken
                    .endif
                    .endrepeat
                .endif
            .endif
            ; --------------------------------------------------------------------------------------------
            ; END
            
            .if foundValidAND || foundValidOR
                .if foundTokenPosition
                    ; branch to next appropriate AND/OR statement:
                    .define branchToLabel .ident( .sprintf( "IF_LABEL_%04X_BRANCH_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, foundTokenPosition ))
                    tokenPositionForBranchLabel{branchLabelCounter} .set foundTokenPosition
                    branchLabelCounter .set branchLabelCounter + 1
                .else ; additional || or && that affects this branch, but none to branch to:
                    .if foundValidAND
                        .define branchToLabel conditionFailLabel    ; branch to conditionFailLabel on inverted flag, e.g.: if ( C set && N set)
                    .else ; foundValidOR
                        .define branchToLabel conditionPassLabel    ; branch to conditionPassLabel on flag, e.g.: if ( C set || N set)
                    .endif
                .endif
            .else ; no || or && found that applies to this branch:
                ; branch directly to pass condition on goto/break, invert and branch to ENDIF
                ; for IF..ENDIF statements. Flip this when long jumps are active.
                .if (gotoUserLabel || gotoBreakLabel) ^ FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE
                    .define branchToLabel conditionPassLabel
                .else
                    invertBranchCondition
                    .define branchToLabel conditionFailLabel
                .endif
            .endif
            ; output the branch:
            Branch branchFlag, branchCondition, branchToLabel
            ; clear temporary settings
            clearBranchSet
            restoreTokenListPosition
            restoreStackPointer "_IF_NEGATE_STACK_"
            .undefine branchToLabel
        .endif
        ; --------------------------------------------------------------------------------------------
        ; END .if matchToken {abc} || xmatchToken{a} || xmatchToken{x} || xmatchToken{y} || matchToken {::}
    .endif
    .endrepeat
    
    ; when long jump active, if..endif will branch here on fail, goto/break will branch here on pass:
    .if FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE
        longJumpToEndIf_IfGotoPass:
        .if gotoUserLabel
            .define jmpToLabel .mid(lastCloseBracketPos + 2, .tcount({condition}) - lastCloseBracketPos - 2, {condition})
        .elseif gotoBreakLabel
            .define jmpToLabel .ident( .sprintf( "BREAK_STATEMENT_LABEL_%04X", FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT))
        .else
            .define jmpToLabel .ident( .sprintf( "_IF_STATEMENT_ENDIF_LABEL_%04X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT ))
        .endif
        jmp jmpToLabel
        .undefine jmpToLabel
    .endif
    ; local label for branching into the code block, or failing for goto statement:
    ifEndIfCodeBlockStart_IfGotoFail:   
    
    .if gotoBreakLabel
        stackPush "BREAK_STATEMENT_STACK", FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT
    .elseif !gotoUserLabel
        stackPush "_IF_ENDIF_STATEMENT_STACK_", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT
    .endif
    
    FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT + 1    ; increase if statement count always
    FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE .set 0

    endTokenListEval ; clear token evaluation    
    .undefine conditionFailLabel
    .undefine conditionPassLabel
    .undefine tokenPositionForBranchLabel

.endmacro

.macro else knownFlagStatus
    
    .local IF_STATEMENT_COUNT 
    stackPeek "_IF_ENDIF_STATEMENT_STACK_", IF_STATEMENT_COUNT ; just look, don't touch
    .if IF_STATEMENT_COUNT < 0 
        .error "'else' without 'if'"
        .fatal "Halting"
    .endif
    
    .ifdef .ident( .sprintf( "_IF_STATEMENT_ELSE_%04X_DEFINED", IF_STATEMENT_COUNT ))
        .error "Duplicate 'else'."
        .fatal "STOP"
    .endif
    ; mark this IF as having an ELSE
    .ident( .sprintf( "_IF_STATEMENT_ELSE_%04X_DEFINED", IF_STATEMENT_COUNT )) = 1
    
    ; jump to endif:
    .ifnblank knownFlagStatus
        Branch {.left(1, knownFlagStatus)}, {.right(1, knownFlagStatus)}, .ident( .sprintf( "_IF_STATEMENT_ELSE_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT ))
    .else
        jmp .ident( .sprintf( "_IF_STATEMENT_ELSE_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT ))
    .endif
    
    ; elseif counter for this if statement:
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )    
    ; if ELSE_IF_COUNT is defined, means there are one or more ELSEIF, so create a label for the last one,
    ; otherwise, create a label for the originating IF
    .ifdef ELSE_IF_COUNT
        .ident( .sprintf( "_IF_STATEMENT_%04X_ELSEIF_ENDIF_LABEL_%04X_", IF_STATEMENT_COUNT, ELSE_IF_COUNT )):
        ELSE_IF_COUNT .set -1 ; signal it is not needed for the endif macro
    .else
        .ident( .sprintf( "_IF_STATEMENT_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT )):
    .endif
    
    .undefine ELSE_IF_COUNT
    
.endmacro


.macro elseif condition, knownFlagStatus

    .local IF_STATEMENT_COUNT
    stackPeek "_IF_ENDIF_STATEMENT_STACK_", IF_STATEMENT_COUNT ; just look, don't touch
    .if IF_STATEMENT_COUNT < 0 
        .error "'elseif' without 'if'"
        .fatal "Halting"
    .endif
    
    ; jump to endif
    .ifnblank knownFlagStatus
        Branch {.left(1, knownFlagStatus)}, {.right(1, knownFlagStatus)}, .ident( .sprintf( "_IF_STATEMENT_ELSE_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT ))
    .else
        jmp .ident( .sprintf( "_IF_STATEMENT_ELSE_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT ))
    .endif
    
    ; elseif counter for this if statement:
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )
    ; if not defined, it means there are no previous ELSEIF, so create a label for the originating IF 
    .ifndef ELSE_IF_COUNT
        ; set the endif label for the original IF:
        .ident( .sprintf( "_IF_STATEMENT_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT )):
        ELSE_IF_COUNT .set -1 ; start at -1, will be incremented to 0
    .else
        ; this isn't the first ELSEIF: 
        .ident( .sprintf( "_IF_STATEMENT_%04X_ELSEIF_ENDIF_LABEL_%04X_", IF_STATEMENT_COUNT, ELSE_IF_COUNT )):
    .endif
    
    ELSE_IF_COUNT .set ELSE_IF_COUNT + 1
    ; negate statement to GOTO the next ELSEIF/ELSE/ENDIF on failed condition 
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    if {condition goto .ident( .sprintf( "_IF_STATEMENT_%04X_ELSEIF_ENDIF_LABEL_%04X_", IF_STATEMENT_COUNT, ELSE_IF_COUNT )) }
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    
    .undefine ELSE_IF_COUNT 
    
.endmacro

.macro endif

    .local IF_STATEMENT_COUNT
    ; get LIFO label counter
    stackPop "_IF_ENDIF_STATEMENT_STACK_", IF_STATEMENT_COUNT
    ; check if all okay
    .if IF_STATEMENT_COUNT < 0 
        .error "'endif' without 'if'"
        .fatal "Halting"
    .endif
    
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )
    ; if label was referenced, it means there was an ELSE or ELSEIF, so create the label
    ; otherwise, create a label for the originating IF
    .ifref .ident( .sprintf( "_IF_STATEMENT_ELSE_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT ))
        .ident( .sprintf( "_IF_STATEMENT_ELSE_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT )):
        ; if there was an ELSEIF and last ELSEIF label not handled by an ELSE:
        .ifdef ELSE_IF_COUNT
            .if ELSE_IF_COUNT <> -1
                .ident( .sprintf( "_IF_STATEMENT_%04X_ELSEIF_ENDIF_LABEL_%04X_", IF_STATEMENT_COUNT, ELSE_IF_COUNT )):
            .endif
        .endif
    .else
        .ident( .sprintf( "_IF_STATEMENT_ENDIF_LABEL_%04X", IF_STATEMENT_COUNT )):
    .endif
    
    .undefine ELSE_IF_COUNT 
    
.endmacro

; --------------------------------------------------------------------------------------------

.macro do
    stackPush "DO_WHILE_LOOP_STATEMENT_STACK", FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT
    .ident( .sprintf( "_DO_WHILE_LOOP_LABEL_%04X", FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT)):
    FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT + 1
.endmacro


.macro while exp
    .if .xmatch(.right(1, {exp}), do) ; if match 'do' at end this is a while..do..endwhile statement
        while_do {.mid(0, .tcount({exp}) - 1, {exp}) }
    .else
        .local DO_WHILE_STATEMENT_COUNT
        stackPop "DO_WHILE_LOOP_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
        if {exp goto .ident( .sprintf( "_DO_WHILE_LOOP_LABEL_%04X", DO_WHILE_STATEMENT_COUNT))}
        ___generateBreakLabel
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
.macro repeat
    do
.endmacro

.macro until exp
    .local DO_WHILE_STATEMENT_COUNT
    stackPop "DO_WHILE_LOOP_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    if {exp goto .ident( .sprintf( "_DO_WHILE_LOOP_LABEL_%04X", DO_WHILE_STATEMENT_COUNT))}
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    ___generateBreakLabel
.endmacro

; --------------------------------------------------------------------------------------------
; while <> do .. endwhile
; This macro saves the expression passed and outputs code for it in the <endwhile> statement
; This way, only one JMP is executed and the condition is tested at the end of the code block

.macro while_do exp
    stackPush "WHILE_DO_ENDWHILE_LOOP_STATEMENT_STACK", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT                      ; save counter
    ; output a jmp to send execution to the end of the code block for the test condition:
    jmp .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_START_LABEL_%04X", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT))   
    .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_LABEL_%04X", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT)):            ; loop back here on passed condition
    _pushTokenList "WHILE_DO_ENDWHILE_LOOP_STATEMENT", {exp}                                                                    ; save the expression passed
    FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT + 1      ; increment while-do counter
.endmacro

.macro endwhile
    .local WHILE_DO_ENDWHILE_STATEMENT_COUNT
    stackPop "WHILE_DO_ENDWHILE_LOOP_STATEMENT_STACK", WHILE_DO_ENDWHILE_STATEMENT_COUNT                                        ; get the counter
    .if WHILE_DO_ENDWHILE_STATEMENT_COUNT < 0                                                                                   ; error check
        .error "'endwhile' without 'while-do'"
        .fatal "Halting"
    .endif
    _popTokenList "WHILE_DO_ENDWHILE_LOOP_STATEMENT"                                                                            ; pop the expression into poppedTokenList
    .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_START_LABEL_%04X", WHILE_DO_ENDWHILE_STATEMENT_COUNT)):                           ; label for JMP from while-do macro
    if {poppedTokenList goto .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_LABEL_%04X", WHILE_DO_ENDWHILE_STATEMENT_COUNT))}        ; output code to test the condition
    ___generateBreakLabel
.endmacro

.macro ___checkBreakInsideLoop
    .local doWhileLoop
    .local whileDoLoop
    stackPeek "DO_WHILE_LOOP_STATEMENT_STACK", doWhileLoop
    stackPeek "WHILE_DO_ENDWHILE_LOOP_STATEMENT_STACK", whileDoLoop
    .if doWhileLoop < 0 && whileDoLoop < 0
        .error "No loop for 'break'"
    .endif
.endmacro

.macro break knownFlagStatus
    ___checkBreakInsideLoop
    .ifnblank knownFlagStatus
        Branch {.left(1, knownFlagStatus)}, {.right(1, knownFlagStatus)}, .ident( .sprintf( "BREAK_STATEMENT_LABEL_%04X", FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT))
    .else
        jmp .ident( .sprintf( "BREAK_STATEMENT_LABEL_%04X", FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT))
    .endif
    stackPush "BREAK_STATEMENT_STACK", FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT
.endmacro

.macro ___generateBreakLabel checkMoreBreakStatements
    .local _BREAK_STATEMENT_COUNT
    stackPeek "BREAK_STATEMENT_STACK", _BREAK_STATEMENT_COUNT  
    .ifnblank checkMoreBreakStatements  ; recursive call to pop stack of any matching break statements
        .if _BREAK_STATEMENT_COUNT = checkMoreBreakStatements
            stackPop "BREAK_STATEMENT_STACK", _BREAK_STATEMENT_COUNT
            ___generateBreakLabel _BREAK_STATEMENT_COUNT
        .endif
    .elseif _BREAK_STATEMENT_COUNT >= 0
        stackPop "BREAK_STATEMENT_STACK", _BREAK_STATEMENT_COUNT
        .ident( .sprintf( "BREAK_STATEMENT_LABEL_%04X", _BREAK_STATEMENT_COUNT)):
        ___generateBreakLabel _BREAK_STATEMENT_COUNT
        FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::BREAK_STATEMENT_COUNT + 1
    .endif
.endmacro


; --------------------------------------------------------------------------------------------

.endif
