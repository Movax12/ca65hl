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
; File: ca65hl.h
; Section: ca65hl
; 
; This is a recreation of ca65 macros to allow for some high-level like structured code for branches and loops.
; This code started (rewritten) around Feb 2022
;
; This file is only macro code, intended to add functionality. No memory is used and no
; supporting 6502 code needed.
;
; Some macros are intended to be 'private' to this file. They are are prefixed with a triple underscore.
; 
; Macros for use outside of this file:
;
; > setBranch
; > setLongBranch
; > mb
; > if
; > else
; > elseif
; > endif
; > do..while
; > repeat..until
; > while <> do
; > endwhile
; > break
; > for..next
; > switch..case..endswitch

.ifndef ::_CA65HL_H_
::_CA65HL_H_ = 1

; Include macros to enable custom array syntax for instructions
.ifndef ::_CA65HL_USE_CUSTOM_SYNTAX_
    ::_CA65HL_USE_CUSTOM_SYNTAX_ = 1
.endif

; Warn in some cases if turned on:
.ifndef ::__CA65HL_WARNING_LEVEL__
    ::__CA65HL_WARNING_LEVEL__ = 2
.endif

; --------------------------------------------------------------------------------------------
; debugging for this file only. Change this to '1' for some console output.
::__DEBUG_CA65HL__ = 0
.macro printTokenListDebug parameter
    .if ::__DEBUG_CA65HL__
        printTokenList {parameter}
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
.include "stacks.h"             ; macros that allow for named stacks 
.include "tokeneval.h"          ; macros to make token evaluation easier
.include "debug.h"              ; macros to help with debugging
.if ::_CA65HL_USE_CUSTOM_SYNTAX_
    .include "customSyntax.h"   ; macros to enable allow custom syntax for instructions
.endif

; --------------------------------------------------------------------------------------------
; Substitutes for branch mnemonics. Edit or add to as desired.
; 'set' or 'clear' can be added after keywords when in use.
; 'set' will have no effect, 'clear' will invert the flag.

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
.define greater             G        ; Use greater and less/equal macros
.define lessORequal        !G

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
; Function: ___error
;
; Parameters:
;
;   message - Error message to display.
;
;   This macro will output an error message and halt assembly.
;   (.fatal doesn't output line numbers)

.macro ___error message
    .error message
    .fatal "Halting"
.endmacro

; --------------------------------------------------------------------------------------------
; Function: branchOnGreater
;
; Parameters:
;
;   label - Label to branch to.
;
;   Simulate a branch for 'G' flag set: 'greater'

.macro branchOnGreater label
    beq :+
    bcs label
    :
.endmacro

; --------------------------------------------------------------------------------------------
; Function: branchOnLessOrEqual
;
; Parameters:
;
;   label - Label to branch to.
;
;   Simulate a branch for 'G' flag clear: 'less or equal'

.macro branchOnLessOrEqual label
    beq label
    bcc label
.endmacro

; --------------------------------------------------------------------------------------------
; Branch macros section - to define the next branch instruction to be generated

; --------------------------------------------------------------------------------------------
; keep track if a branch has been defined for the next ___Branch macro

.scope ___branchSet
    branchDefined .set 0
.endscope

; --------------------------------------------------------------------------------------------
; Inline C-style defines that resolve to instructions.
; Used with macro <___Branch>
; Branch instructions:
; C, Z, N, V, G flag can be followed by the keyword 'set' or 'clear'.

.define BranchOn_C_set   bcs
.define BranchOn_C_clear bcc
.define BranchOn_Z_set   beq
.define BranchOn_Z_clear bne
.define BranchOn_N_set   bmi
.define BranchOn_N_clear bpl
.define BranchOn_V_set   bvs
.define BranchOn_V_clear bvc
.define BranchOn_G_set   branchOnGreater
.define BranchOn_G_clear branchOnLessOrEqual

; --------------------------------------------------------------------------------------------
; Function: ___Branch (F, S, label)
;
; Parameters:
;
;   F - Flag to indicate branch instruction: C, Z, N, V, G
;   S - Status of flag: 'set' or 'clear'
;   label - label to branch to
;
; This inline macro will expand to a branch instruction.
; (uses .left to turn .ident into a token list: ca65 will recognized as an above .define)

.define ___Branch (___F, ___S, label) .left(1, .ident( .sprintf("BranchOn_%s_%s", .string(___F), .string(___S)))) label

; --------------------------------------------------------------------------------------------
; Function: setBranchFlag f, setBranchCondition s
;
; Parameters:
;
;   f - Set flag for next invocation of <___Branch>
;   s - Set flag status for next invocation of <___Branch>
;
; Individually define branch flag and flag status for next branch.

.macro setBranchFlag _f
    ___branchSet::branchDefined .set 1
    .define branchFlag _f
.endmacro

.macro setBranchCondition _s
    .define branchCondition _s
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___setBranch branch
;
; Parameters:
;
;   branch - Set flag and status for next invocation of <___Branch>
;
; Define both branch flag and flag status for next branch. Also set 
; ___branchSet::branchDefined flag that they are set. Used internally with no error checking.

.macro ___setBranch branch
    ___branchSet::branchDefined .set 1
    setBranchFlag {.left(1,branch)}
    setBranchCondition {.right(1,branch)}
.endmacro

; --------------------------------------------------------------------------------------------
; Function: setBranch branch
;
; Parameters:
;
;   branch - Set flag and status for next invocation of <___Branch>
;
; Define both branch flag and flag status for next branch. Also set ___branchSet::branchDefined 
; flag that they are set. Ignore if branch is already defined.
; This allows overriding user macro calls to setBranch with inline branch definition via '==', '!='
; Does some error checking for the user. 

.macro setBranch branch
    .if !___branchSet::branchDefined
        ___branchSet::branchDefined .set 1
        .if .xmatch(.left(1,{branch}), !)   ; check for !, skip
            .define ___BRANCH() .mid(1, .tcount({branch}) - 1, {branch})   
        .else
            .define ___BRANCH() branch
        .endif
        ; error check: must be C Z N V G
        .if !(.xmatch( {.left(1,___BRANCH)}, C) || .xmatch( {.left(1,___BRANCH)}, Z) || .xmatch( {.left(1,___BRANCH)}, N) || .xmatch( {.left(1,___BRANCH)}, V) || .xmatch( {.left(1,___BRANCH)}, G))
            ___error "Expected: Valid flag: C, Z, N, V, G"
        .endif
        setBranchFlag {.left(1,___BRANCH)}
        .if .tcount({___BRANCH}) > 1
            .if !(.xmatch( {.right(1,___BRANCH)}, set) || .xmatch( {.right(1,___BRANCH)}, clear))
                ___error "Expected: 'set' or 'clear'"
            .endif
            setBranchCondition {.right(1,___BRANCH)}
        .else
            setBranchCondition set
        .endif
        .if .xmatch(.left(1,{branch}), !)
            ___invertBranchCondition
        .endif
        .undefine ___BRANCH
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___invertBranchCondition
;
; Parameters: none
;
; Invert current branch setting

.macro ___invertBranchCondition 
    .define ___tempCondition branchCondition
    .undefine branchCondition
    .if .xmatch (___tempCondition, set)
        .define branchCondition clear
    .else
        .define branchCondition set
    .endif
    .undefine ___tempCondition
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___clearBranchSet
;
; Parameters: none
;
; Clear defines after a branch is output as code. Clear that branch is defined.

.macro ___clearBranchSet
    ___branchSet::branchDefined .set 0
    .undefine branchFlag
    .undefine branchCondition
.endmacro

; --------------------------------------------------------------------------------------------
; symbols to track some values for if, loops and break

.scope FLOW_CONTROL_VALUES
    IF_STATEMENT_COUNT                  .set 0  ; if statement label counter - always incremented after every 'if'
    LAST_ENDIF_COUNT                    .set 0  ; keep track of the count of last IF-ENDIF block
    DO_WHILE_STATEMENT_COUNT            .set 0  ; while loop counter 
    WHILE_DO_ENDWHILE_STATEMENT_COUNT   .set 0  ; while..do endwhile counter
    FOR_STATEMENT_COUNTER               .set 0  ; for statement counter
    SWITCH_STATEMENT_COUNTER            .set 0  ; switch statement counter
    SWITCH_STATEMENT_DATA_SEG           .set 0  ; flag: if on, use data segment defined by setSwitchStatementDataSeg
    NEGATE_CONDITION                    .set 0  ; flag: if on, conditions are inverted
    IF_STATEMENT_ACTIVE                 .set 0  ; flag: if executing an 'if' macro (no calling an 'if' while a condition is being processed)
    LONG_JUMP_ACTIVE                    .set 0  ; flag: use JMP to branch
    LONG_JUMP_WARNINGS                  .set 1  ; flag: output warnings if long jump not needed
    INTERNAL_CALL                       .set 0  ; flag: if on, 'if' macro being invoked from this file.
    BREAK_COUNT_DOWHILE                 .set 0  ; break statement counter - incremented after break label created
    BREAK_COUNT_WHILEDO                 .set 0  ; break statement counter - incremented after break label created
    BREAK_COUNT_FOR                     .set 0  ; break statement counter - incremented after break label created
    BREAK_COUNT_SWITCH                  .set 0  ; break statement counter - incremented after break label created
    LOOP_TYPE_DEFINED                   .set 0  ; flag: only undefine ___loopType if set
.endscope

; --------------------------------------------------------------------------------------------
; Function: setLongBranch
;
; Parameters:
;
;   l - long branch setting: 'on' or '+' to turn on, 'off' or '-' to turn off
;   v - long branch warnings: 'on' or '+' to turn on, 'off' or '-' to turn off
;
; If long branch turned on, JMP will be output to branch to a user label, break, or endif.
; If warnings turned on an .assert will output a warning for branches that could have
; been reached without a JMP instruction.

.macro setLongBranch l, v
    .if .xmatch(l, on) || .xmatch(l, +)
        FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE .set 1
    .elseif .xmatch(l, off) || .xmatch(l, -)
        FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE .set 0
    .else
        ___error "Unknown long branch setting."
    .endif
    .ifnblank v
        .if .xmatch(v, on) || .xmatch(v, +)
            FLOW_CONTROL_VALUES::LONG_JUMP_WARNINGS .set 1
        .elseif .xmatch(v, off) || .xmatch(v, -)
            FLOW_CONTROL_VALUES::LONG_JUMP_WARNINGS .set 0
        .else
            ___error "Unknown long branch setting."
        .endif
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: depreciateLongBranch
;
; Parameters:
;
;   l - 'long' or 'short' expected
;
; If long or short branch forced, will be ignored for backward branches.
; Warn that this is the case. This will be removed in future versions.

.macro depreciateLongBranch
    .warning "Long branch option depreciated for backward branches. Backward branches automatically generated!"        
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
    .if !op
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

; --------------------------------------------------------------------------------------------;
; called by IF when compare operator found in a statement
; ___compare::operator and ___compare::position must be set first
; 
.macro ___doCompare exp

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
    .if !left
        .define _LEFT1 .left (1, {_LEFT})
        ; look for characters, then see if they match a supported instruction
        .if .match( _LEFT1, abc)                        
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
            ; if match found, output the instruction
            .if left
                _LEFT1 {.right( .tcount({_LEFT}) - 1, {_LEFT} )}
            .endif
        .endif
        .if !left
            ___evalMathOp {_LEFT}
            left .set ___math::regFound
        .endif
        .undefine _LEFT1
    .endif
    
    .if !left
       ___error "Unknown register to use in comparison macro."
       .exitmacro
    .endif
    
    .if left = ___math::REGA
        cmp _RIGHT
    .elseif left = ___math::REGX
        cpx _RIGHT
    .elseif left = ___math::REGY
        cpy _RIGHT
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
    .if !op
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
                ;.elseif .xmatch ( {.mid(i,1,{exp})}, * ) ; not implemented
                ;    op .set ___math::MULT
                ;.elseif .xmatch ( {.mid(i,1,{exp})}, / ) ; not implemented
                ;    op .set ___math::DIVIDE
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
    .if !op
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
    .local tcount
    op1      .set 0
    op2      .set 0
    pos1     .set 0
    pos2     .set 0
    carryOp  .set 0
    reg      .set 0
    
    ___math::regFound .set 0
    printTokenListDebug {Math eval: exp}
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
    .if ( op1 > 2 ) || ( ( op1 = 1 || op1 = 2) && (!.match(.left(1,_RIGHT_EXP), 1)))  ; ADD and SUB are 1, 2
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
    .if !(.xmatch ({ .left(1,{exp}) }, a) || .xmatch ({ .left(1,{exp}) }, x) || .xmatch ({ .left(1,{exp}) }, y) )
        .if (op1 && ( pos1 > 0))
            _LOAD {.mid(0, pos1, {exp})}
        .elseif !op1
            ; if no operation, then something to load
            _LOAD {exp}
        .endif
    .endif
    
    .if reg = ___math::REGX
        .if op1 = ___math::ADD
            .repeat _RIGHT_EXP
                inx
            .endrepeat
        .elseif op1 = ___math::SUB
            .repeat _RIGHT_EXP
                dex
            .endrepeat            
        .endif
    .elseif reg = ___math::REGY
        .if op1 = ___math::ADD
            .repeat _RIGHT_EXP
                iny
            .endrepeat
        .elseif op1 = ___math::SUB
            .repeat _RIGHT_EXP
                dey
            .endrepeat
        .endif       
    ; if reg = ___math::REGA :
    .elseif op1 = ___math::ADDC
        .if pos1 + carryOp + 1 = .tcount({exp})
            adc #0
        .else
            adc _RIGHT_EXP
        .endif
    .elseif op1 = ___math::SUBC 
        .if pos1 + carryOp + 1 = .tcount({exp})
            sbc #0
        .else
            sbc _RIGHT_EXP
        .endif
    .elseif op1 = ___math::SUB
        sec
        sbc _RIGHT_EXP
    .elseif op1 = ___math::ADD
        clc
        adc _RIGHT_EXP
    .elseif op1 = ___math::SHL
        .repeat _RIGHT_EXP
            asl a
        .endrepeat
    .elseif op1 = ___math::SHR
        .repeat _RIGHT_EXP
            lsr a
        .endrepeat
    .elseif op1 = ___math::AND_
        and _RIGHT_EXP
    .elseif op1 = ___math::OR
        ora _RIGHT_EXP
    .elseif op1 = ___math::EOR_
        eor _RIGHT_EXP
    .endif
    .undefine _RIGHT_EXP
    .undefine _LOAD
    
    .if op2
        ___evalMathOp { .mid(pos2, .tcount({exp}) - pos2, {exp}) }, register
    .else
       ___math::regFound .set reg ; signal back which register was used for any operation
    .endif
    
.endmacro

; --------------------------------------------------------------------------------------------
; Function: mb register, exp
;
; Parameters:
;    register - Optional - register to use 
;    exp - expression to process
;
;   Move a byte, possibly through some other instructions for basic add/sub or bit-wise operations.
;   Can accept one parameter. If no register passed, register to use will be searched for, 
;   or register A will be used by default. (The macro will adjust arguments if exp is empty).

.macro mb register, exp

    .local requestReg
    .local leftReg
    .local pos
    requestReg    .set 0
    leftReg     .set 0    
    pos .set 0
    .ifblank exp
        .define _EXP () register
    .else
        ; register passed in 'register':
        .define _EXP () exp
        .if .xmatch(register, a)
            requestReg .set ___math::REGA
        .elseif .xmatch(register, x)
            requestReg .set ___math::REGX
        .elseif .xmatch(register, y)
            requestReg .set ___math::REGY
        .else
            ___error "Unknown register."
        .endif
    .endif
    printTokenListDebug {MB_:REG_: register | EXP_:exp}	

    ; find assignment token: (accept := or =)
    ___findToken {_EXP}, =, pos
    .if !pos
        ___findToken {_EXP}, :=, pos
    .endif
    .if !pos
        ___error "No assignment."
    .endif
    
    .define _LEFT .mid(0, pos, {_EXP})
    .define _RIGHT .mid(pos + 1, .tcount({_EXP}) - pos - 1, {_EXP})
    
    ; find left register. 'a:' is a single token in ca65, so accept it too, assume ':' is part of ':='
    .if .xmatch(.left(1, {_LEFT}), a) || .xmatch(.left(1, {_LEFT}), a:)
        leftReg .set ___math::REGA
    .elseif .xmatch(.left(1, {_LEFT}), x)
        leftReg .set ___math::REGX
    .elseif .xmatch(.left(1, {_LEFT}), y)
        leftReg .set ___math::REGY
    .endif
    
    .if !requestReg ; if no register defined for move yet, see if it is explicitly used on the right side
        .if .xmatch(.left(1, {_RIGHT}), a)
            requestReg .set ___math::REGA
        .elseif .xmatch(.left(1, {_RIGHT}), x)
            requestReg .set ___math::REGX
        .elseif .xmatch(.left(1, {_RIGHT}), y)
            requestReg .set ___math::REGY
        .else
            ; no match on right, use any register that may have been found on the left
           requestReg .set leftReg
        .endif
    .endif
    
    ; look for any simple math operations on the right side, 
    ; call will output any LOAD first, evaluate any operations
    ; from left to right
    .if requestReg
        ___evalMathOp {_RIGHT}, requestReg
    .else
        ___evalMathOp {_RIGHT}
    .endif
    ; override right side reg if ___evalMathOp changed/set it:
    requestReg .set ___math::regFound
    
    .if !leftReg ; no register defined? just store: 
        .if requestReg = ___math::REGA
            sta _LEFT
        .elseif requestReg = ___math::REGX
            stx _LEFT
        .elseif requestReg = ___math::REGY
            sty _LEFT
        .endif
    .elseif leftReg = ___math::REGA
        .if requestReg = ___math::REGX
            txa
        .elseif requestReg = ___math::REGY
            tya
        .endif
    .elseif leftReg = ___math::REGX
        .if requestReg = ___math::REGA
            tax
        .elseif requestReg = ___math::REGY
            ___error "Cannot do 'x := y' type move."
        .endif
    .elseif leftReg = ___math::REGY
        .if requestReg = ___math::REGA
            tay
        .elseif requestReg = ___math::REGX
            ___error "Cannot do 'y := x' type move."
        .endif
    .endif
    
   .undefine _LEFT
   .undefine _RIGHT
   .undefine _EXP
.endmacro

; --------------------------------------------------------------------------------------------
; Macro for copying parameters.
; This will output the appropriate amount of load/store instructions
; Arguments: CPU register to use, source, destination, dest. size, source size, and immediate mode flag
; Source must be the same size or smaller then the dest.
; If the source is smaller, load a zero, skip further loads, and fill the destination with zero
; We could optionally sign extend, not implemented yet.
; Arguments don't need error checking, should be valid from calling macro

.scope ___moveWord
    previousImm .set 0
.endscope

; small macro for ___moveMem_ca65hl macro, to skip loading the same immediate value if possible
.macro ___LOAD_imm_ca65HL value

    .if .const(value)
        .if .not ( value = ___moveWord::previousImm )
            LOAD # ( value )
            ___moveWord::previousImm .set value 
        .endif
    .else
        LOAD # ( value )
    .endif
    
.endmacro

.macro ___moveMem_ca65hl reg, source, dest, destsize, sourcesize, imm

    .local sourcecount
    sourcecount .set sourcesize

    .if .xmatch(reg, a)
        .define LOAD  lda
        .define STORE sta
    .elseif .xmatch(reg, x) 
        .define LOAD  ldx
        .define STORE stx
    .else
        .define LOAD  ldy
        .define STORE sty
    .endif
    
    ; set to a value that couldn't fit into one byte to avoid matching the first byte
    ___moveWord::previousImm .set $0100
    
    .if imm
        .repeat destsize, i
                ; nothing to do for source if sourcecount is -1
                .if sourcecount <> -1 
                    .if sourcecount = 0
                        LOAD #0
                    .else
                        ___LOAD_imm_ca65HL <( ( source ) >> (8 * i) )
                    .endif
                    sourcecount .set sourcecount - 1
                .endif
                STORE i+dest
        .endrepeat
    .else
        .repeat destsize, i
                ; nothing to do for source if sourcecount is -1
                .if sourcecount <> -1 
                    .if sourcecount = 0
                        LOAD #0
                    .else   
                        LOAD i + source
                    .endif
                    sourcecount .set sourcecount - 1
                .endif
                STORE i+dest
        .endrepeat
    .endif
    
    .undefine LOAD
    .undefine STORE
    
.endmacro

; --------------------------------------------------------------------------------------------
; Function: mw register, exp
;
; Parameters:
;    register - Optional - register to use 
;    exp - expression to process
;
;   Simple support for moving 16 bits as address or immediate.
;   Can accept one parameter. If no register passed, register to use will be searched for, 
;   or register A will be used by default. (The macro will adjust arguments if exp is empty).

.macro mw register, exp

    .local parameterDestSize  
    .local parameterSourceSize 
    .local immMode
    .local castbase
    .local requestReg
    .local pos
    requestReg  .set 0
    parameterDestSize   .set 0  ; size of memory for parameter to be loaded
    parameterSourceSize .set 0  ; size of source data
    immMode .set 0              ; if copy should be immediate mode
    destIsReg .set 0            ; if the destination matched a register
    sourceIsReg .set 0          ; if the source matched a register
    pos .set 0
    
    .ifblank exp
        .define _EXP () register
    .else
        ; register passed in 'register':
        .define _EXP () exp
        .if .xmatch(register, a)
            requestReg .set ___math::REGA
        .elseif .xmatch(register, x)
            requestReg .set ___math::REGX
        .elseif .xmatch(register, y)
            requestReg .set ___math::REGY
        .else
            ___error "Unknown register."
        .endif
    .endif
    printTokenListDebug {MB_:REG_: register | EXP_:exp}	

    ; find assignment token: (accept := or =)
    ___findToken {_EXP}, =, pos
    .if !pos
        ___findToken {_EXP}, :=, pos
    .endif
    .if !pos
        ___error "No assignment."
    .endif
    
    .define _T_DEST_ .mid(0, pos, {_EXP})
    .define _T_SOURCE_ .mid(pos + 1, .tcount({_EXP}) - pos - 1, {_EXP})
    ; for now, this will always be two for 16 bit move, keep as 'variable' to allow future changes
    parameterDestSize .set 2
    
    ; --------------------------------------------------------------------------------------------
    ; find destination

    .if .xmatch( { _T_DEST_ }, { ax } ) || .xmatch( { _T_DEST_ }, { ay } ) || .xmatch( { _T_DEST_ }, { xy } )     
        .define _DEST_ _T_DEST_
        destIsReg .set 1
        ; for nicer error messages:
        .if .xmatch(_T_DEST_,ax) 
            .define _DEST_STR_ "register ax"
        .elseif .xmatch(_T_DEST_,ay) 
            .define _DEST_STR_ "register ay"
        .elseif .xmatch(_T_DEST_,xy) 
            .define _DEST_STR_ "register xy"
        .endif
    .else
        .define _DEST_ _T_DEST_
        .define _DEST_STR_ ""
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; find source

    .if .xmatch( {.left(1, {_T_SOURCE_} ) } , {&} ) 
            parameterSourceSize .set 2
            immMode .set 1
            ; remove the '&'
            .define _SOURCE_ .mid(1, .tcount( {_T_SOURCE_} ) - 1 , {_T_SOURCE_})
        ; Check if source is an immediate value:
    .elseif .xmatch( .left(1,{_T_SOURCE_}), # ) ; if immediate
        immMode .set 1
        ; remove the '#'
        .define _SOURCE_ .mid(1, .tcount( {_T_SOURCE_} ) - 1 , {_T_SOURCE_})
        ; check if there is a constant
        .if .const( _SOURCE_ ) 
            .if ( _SOURCE_ & $FFFF0000 ) 
                parameterSourceSize .set 4
            .elseif ( _SOURCE_ & $FF00 )
                parameterSourceSize .set 2
            .else
                parameterSourceSize .set 1
            .endif
        .else ; not a constant, so just match destination size. 
            parameterSourceSize .set parameterDestSize
        .endif
    
    ; Check if source is using a size cast:
    .elseif .xmatch( .left( 1, {_T_SOURCE_}), {(} ) ; size cast
        .if .xmatch( .mid( 1, 1 , {_T_SOURCE_}) ,  .byte )
            castbase .set 1
        .elseif .xmatch( .mid( 1, 1 , {_T_SOURCE_}) ,  .word ) || .xmatch( .mid(1, 1 , {_T_SOURCE_}) ,  .addr )
            castbase .set 2
        .elseif .xmatch( .mid( 1, 1 , {_T_SOURCE_}) ,  .dword )
            castbase .set 4
        .else
            .error "Size expected for cast: .byte, .word, .addr or .dword"
            .fatal "STOP"
        .endif    
        
        .if .match( .mid( 2, 1 , {_T_SOURCE_}), 1) ; number
            castbase .set castbase * .mid(2, 1 , {_T_SOURCE_})
            .if .not .xmatch( .mid( 3, 1 , {_T_SOURCE_}), {)} )
                .error "`)` expected."
                .fatal "STOP"
            .endif
            .define _SOURCE_ .mid( 4, .tcount( {_T_SOURCE_} ) - 4 , {_T_SOURCE_})
        .else ; no number in cast
            .if .not .xmatch( .mid( 2, 1 , {_T_SOURCE_}), {)} )
                .error "`)` expected."
                .fatal "STOP"
            .endif
            .define _SOURCE_ .mid( 3, .tcount( {_T_SOURCE_} ) - 3 , {_T_SOURCE_})
        .endif
        parameterSourceSize .set castbase
    
    .else
        ; Start here: no leading tokens to skip due to not address operator, not immediate, not cast size:
        .define _SOURCE_  _T_SOURCE_
        
        ; is source a register?
        ; if not check for other operators supported:
        ; < for lowbyte
        ; > for high bye
        ; otherwise check for an identifier and find its size if possible
        
        ; default:
        ; size of ident may be difficult to determine, so default to this for ca65 to evaluate later and copy parameterDestSize number of bytes:
        parameterSourceSize .set parameterDestSize
            
        ; see if we can determine the size:
        
        ; Check if register:
        .if .xmatch(_SOURCE_,a) || .xmatch(_SOURCE_,x) || .xmatch(_SOURCE_,y)
            parameterSourceSize .set 1
            sourceIsReg .set 1
        .elseif .xmatch(_SOURCE_,ax) || .xmatch(_SOURCE_,ay) || .xmatch(_SOURCE_,xy)
            parameterSourceSize .set 2
            sourceIsReg .set 1
        ; Check if lowbyte or high byte operator: <, >
        .elseif .xmatch( {.left(1, {_SOURCE_} ) } , {<} ) || .xmatch( {.left(1, {_SOURCE_} ) } , {>} )
            parameterSourceSize .set 1
            
        ; see if we can get the size of an single token ident. ca65 is funny with scoping due to one pass, not much more can easily be done.
        ; could be more strict with checking ident. sizes, but it would require calls to use explicit scoping or casting idents.
        .elseif .tcount( { _SOURCE_ } ) = 1
            .if .match( { _SOURCE_ }, 1234 ) ; number: load from address
                .if _SOURCE_ >= 0 && _SOURCE_ <= $FFFF
                    parameterSourceSize .set parameterDestSize
                .else
                    .error "Address out of range."
                .endif
            .elseif .defined ( _SOURCE_ )
                parameterSourceSize .set .sizeof(  _SOURCE_  )
            .endif
        .endif
    .endif
    
    ; error if source is bigger than dest.
    .if parameterSourceSize > parameterDestSize
        .error .sprintf( "Parameter overflow for '%s'. Parameter size: %d bytes. Passed value size: %d bytes.", _DEST_STR_, parameterDestSize, parameterSourceSize)
        .fatal "STOP"
    .endif
    
    ; check for register parameters not supported:
    .if destIsReg && sourceIsReg && ( parameterSourceSize > 1 || parameterDestSize > 1 )
        ; this is an error, unless register parameters match (copy/move not required)
        .if .not (.xmatch(_SOURCE_, _DEST_)) 
            .error .sprintf("Register parameter %s: Not supported with register source.", .string(_DEST_))
            .fatal "STOP"
        .endif
    .endif
    
    ; if source is a CPU reg. and destination is not a register: (assume it is memory)
    .if (.xmatch( _SOURCE_, a) || .xmatch( _SOURCE_, x) || .xmatch( _SOURCE_, y) ) && (.not destIsReg)
        .if .xmatch( _SOURCE_, a) ; if the source is reg.a:
            sta _DEST_
        .elseif .xmatch( _SOURCE_, x) ; if the source is reg.x:
            stx _DEST_
        .elseif .xmatch( _SOURCE_, y) ; if the source is reg.y
            sty _DEST_
        .endif   
        ; check if _DEST_ memory size is bigger than one byte and warn
        .if parameterDestSize > 1
            .warning "MW: Register source size is 1 byte: High bytes not set."
        .endif
    .elseif (.xmatch( _SOURCE_, ax) || .xmatch( _SOURCE_, ay) || .xmatch( _SOURCE_, xy) ) && (.not destIsReg)
        .if .xmatch( _SOURCE_, ax) ; if the source is ax:
            sta _DEST_
            stx _DEST_ + 1
        .elseif .xmatch( _SOURCE_, ay) ; if the source is ay:
            sta _DEST_
            sty _DEST_ + 1
        .elseif .xmatch( _SOURCE_, xy) ; if the source is xy:
            stx _DEST_
            sty _DEST_ + 1
        .endif   
    .elseif (.not destIsReg ) && (.not sourceIsReg ) ; if destination and source is not a register, it is memory
        ; check which register is open for moving memory
        ; we don't have to mark it as used, because it is not holding a value that matters if not already marked
        .if requestReg = ___math::REGY
            ___moveMem_ca65hl y, _SOURCE_, _DEST_, parameterDestSize, parameterSourceSize, immMode
        .elseif requestReg = ___math::REGX
            ___moveMem_ca65hl x, _SOURCE_, _DEST_, parameterDestSize, parameterSourceSize, immMode
        .else
            ___moveMem_ca65hl a, _SOURCE_, _DEST_, parameterDestSize, parameterSourceSize, immMode
        .endif       
    .elseif .xmatch( _DEST_, ax) ; if the dest. is reg.ax:
        .if .not sourceIsReg ; if source is not a register, assume it is memory
            .if immMode
                lda # <_SOURCE_
                ldx # >_SOURCE_
            .else
                lda _SOURCE_
                ldx _SOURCE_ + 1
            .endif   
        .endif
    .elseif .xmatch( _DEST_, ay) ; if the dest. is reg.ay:
        .if .not sourceIsReg ; if source is not a register, assume it is memory
            .if immMode
                lda # <_SOURCE_
                ldy # >_SOURCE_
            .else
                lda _SOURCE_
                ldy _SOURCE_ + 1
            .endif   
        .endif
    .elseif .xmatch( _DEST_, xy) ; if the dest. is reg.xy:
        .if .not sourceIsReg ; if source is not a register, assume it is memory
            .if immMode
                ldx # <_SOURCE_
                ldy # >_SOURCE_
            .else
                ldx _SOURCE_
                ldy _SOURCE_ + 1
            .endif   
        .endif
    .endif    
    
    .undefine _EXP
    .undefine _DEST_STR_
    .undefine _SOURCE_
    .undefine _DEST_
    .undefine _T_DEST_
    .undefine _T_SOURCE_
    
.endmacro

; --------------------------------------------------------------------------------------------
; Recursively evaluate statements: One or more instructions or macros as well as supported comparisons
; and simple match evaluations.

.macro ___evaluateStatementList statement

    ; define and then work with one parameter at a time
    .local colonPos
    colonPos .set 0
    ___findToken {statement}, :, colonPos
    .if colonPos
        .define S() .mid(0, colonPos, {statement})
    .else
        .define S() statement
    .endif
    
    printTokenListDebug {Statement: S}
    
    ; ___findToken won't find a leading ':', check if there is a leading colon, and set colonPos to -1 to indicate this.
    ; (allows for blank statements)
    .if  .xmatch( { .left( 1, {S} ) } , {:} ) || .xmatch( { .left( 1, {S} ) } , {::} )
        colonPos .set -1
    .endif
    
    .if colonPos <> -1 && .tcount({S})
        ; -------------------------------------
        ; first: special case - is a register
        .if .xmatch( .left (1,{S}), a) || .xmatch( .left (1,{S}), x) || .xmatch( .left (1,{S}), y)
            ___findCompareOperator {S}
            .if ___compare::found
                ___doCompare {S}
            .else
                ___evalMathOp {S}
            .endif
        .else 
            ; check for comparison before macros, because instructions could be redefined as macros
            ___findCompareOperator {S}
            .if ___compare::found
                ___doCompare {S}
            .elseif .definedmacro ( .left(1,{S}))
                printTokenListDebug {Macro: S}
                .if .tcount({S}) = 1
                    .left (1,{S})
                .else
                    .left (1,{S}) { .mid (1, .tcount({S}) - 1, {S} ) }
                .endif
            .elseif .ismnemonic(.left (1,{S})) || .xmatch( .left (1,{S}), adc) ; ca65 bug? .ismnemonic doesn't match 'adc'
                .left (1,{S}) .mid (1, .tcount({S}) - 1, {S} )
            .else
                ___evalMathOp {S}
            .endif

        .endif
        ; -------------------------------------
    .endif
    
    .undefine S
      ; Repeat with next if there was a colon found:
    .if colonPos
        .if colonPos = -1
            colonPos .set 0
        .endif
        ___evaluateStatementList { .mid ( colonPos + 1 , .tcount({statement}) - colonPos - 1, {statement} ) }
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Establish what branch to generate for one or more instructions/macros.
; Call ___evaluateStatementList to execute any instructions or macros, then ensure
; a flag is set for branch before exit.

.macro ___evaluateBranch statement

    .local statementTokenCount
    .local inlineBranchSetPos
    .local negateFlag
    ;defaults:
    statementTokenCount .set .tcount({statement})
    inlineBranchSetPos .set 0
    negateFlag .set -1          ; use -1 to mean 'not found'
    ; find possible '==', or '!=' for setting a branch:
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
            ; allow flag after '==' or '!=' to be negated via '.not' or '!'
            .if .xmatch ({.mid(inlineBranchSetPos, 1, {statement})}, !)
                inlineBranchSetPos .set inlineBranchSetPos + 1
                negateFlag .set !negateFlag
            .endif
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
                ___error "Expected: 'set or 'clear'."
            .endif
        .elseif .tcount({statement}) - inlineBranchSetPos = 1 ; only a flag, default is to consider it 'set'
            setBranchFlag .mid(inlineBranchSetPos, 1, {statement})
            setBranchCondition set
        .else
            ___error "Branch parameter error."
        .endif
        .if negateFlag = 1
            ___invertBranchCondition
        .endif
    .endif
    .undefine _flagMatch
    ; error check: if inlineBranchSetPos is not 0, this indicates '==' or '!=' was found. At this point the branch should be defined.
    .if inlineBranchSetPos && (!___branchSet::branchDefined)
        ___error "Unknown flag for branch."
    .endif
    ; branch condition possibly defined now. If it is not, or there is a '==', '!=' that means there is something to execute
    ; (if a user macro call tries to set the flag it will not be honored if flags already set via inlineBranchSetPos)
    .if (!___branchSet::branchDefined ) || inlineBranchSetPos
        ___evaluateStatementList {.mid(0, statementTokenCount, {statement})}
    .endif
    ; still no branch? default to non-zero is 'true'
    .if !___branchSet::branchDefined
        ___setBranch Z clear
    .endif
    
.endmacro

; --------------------------------------------------------------------------------------------
; macro for helping to evaluate || and && tokens with a possible negate applied to a bracket set.
; Called from .macro 'if'
; Will only ever be called with '&&' or '||' for parameter 'token'

.macro ___xmatchSpecial token, match, negateBracketSet
    .if negateBracketSet                    ; inside a negated bracket set?
        .if .xmatch ({token} , {&&})        ; looking to match '&&'
            match .set xmatchToken {||}     ; '||' is match
        .else
            match .set xmatchToken {&&}     ; else '&&' is match
        .endif
    .else
        match .set .xmatch ({token}, {currentToken})
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: if condition
;
; Parameters:
;
;   condition - Conditional expression to evaluate. Requires surrounding braces.
;   opt0, opt1 - can be 'long' or 'short' to override current setLongBranch settings, or 'chain'

;
; Parenthesis are required around the test condition.
; 
;   Example:
; > if (C set || V set) goto label
;
;   See Also:
;   <endif>, <elseif>, <else>
; --------------------------------------------------------------------------------------------

; The core of functionality for this file. Evaluates a condition and 
; generates branches. Calls other macros to process statements and output code/determine what
; to branch on.
; This macro will create branch instructions with regards to the logic in <condition> and branch
; to a provided label when used with 'goto'. If used with 'break', it will verify a valid break 
; (inside a loop) and branch to the next break label. If no 'goto' or 'break', it will branch to 
; the next ENDIF (or ELSE, or ELSEIF) on an inverted condition.


.macro if condition, opt0, opt1
    
    printTokenListDebug {Branch_Statement: condition}
    ; --------------------------------------------------------------------------------------------
    ; compatibility for older code that doesn't have surrounding braces for <condition>
    ; try to help it out by adding some - this will be removed at some point
  ;  .if !.xmatch (.left(1,{condition}), {(})
  ;      .local brace
  ;      brace .set 0
  ;      ;.warning "Need ("
  ;      ___findToken {condition}, goto, brace
  ;      .if !brace
  ;          ___findToken {condition}, break, brace
  ;      .endif
  ;      .if brace
  ;          if ( .left(brace, {condition}) ) .mid(brace, .tcount({condition}) - brace, {condition})
  ;      .else 
  ;          if ( condition )
  ;      .endif
  ;     .exitmacro
  ;  .endif
    ; --------------------------------------------------------------------------------------------
    
    
    .local exitBranchEvaluation         ;  label: branch to this label on failed condition (acts like a pass/true condition with a code block defined by if..endif)
    .local doLongJump                   ;  label: for long jump: branch to this label on a passed/true condition when long jumps active
    .local firstBranchToLongJump        ;  label: for verifying a long jump is needed: address from the first branch that will use a long jump
    .local negateBracketSet             ;  flag: if a set of terms in brackets to be negated
    .local negateNext                   ;  flag: if single branch term to be negated              
    .local bracketLevel                 ;  level of brackets we are in, lowest is 1, 0 is outside brackets
    .local branchLabelCounter           ;  count of how many branch labels to additional OR/AND conditions needed
    .local conditionTokenCount          ;  save token count for condition only (could be goto/break statement after)
    .local gotoUserLabel                ;  flag: if 'goto' found, branch to label passed in <condition>
    .local gotoBreakLabel               ;  flag: branch to break label to exit loop
    .local foundTokenPosition           ;  save token position of valid && or || tokens when performing look-ahead evaluating correct branch
    .local foundOR_AND                  ;  flag: matched an AND or OR when scanning ahead with ___xmatchSpecial
    .local scanAheadBracketLevel        ;  bracket level we are on when scanning ahead
    .local lowestBracketLevel           ;  when scanning ahead, save the lowest bracket level found while looking for a '&&' or '||' to branch to
    .local scanAheadNegateBrackets      ;  negate status for brackets when scanning ahead
    .local statementStartPos            ;  token position for start of found statement
    .local statementTokenCount          ;  token count for found statement
    .local foundAND                     ;  flag: found an && when scanning ahead while considering if bracket set is negated
    .local foundOR                      ;  flag: found an || when scanning ahead while considering if bracket set is negated
    .local useLongJump                  ;  flag: set true if option for 'long', branch is a backward branch, or if FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE is set
    .local chainedFlag                  ;  flag: branch to next enclosing ENDIF for ELSE and ELSEIF structures
    .local longJumpNotNeeded            ;  flag: true if long jump is not needed for this branch. False means long branch needed or unknown if needed for forward branch

    negateBracketSet        .set FLOW_CONTROL_VALUES::NEGATE_CONDITION ; when set this will negate the entire condition
    negateNext              .set 0
    bracketLevel            .set 0
    branchLabelCounter      .set 0
    conditionTokenCount     .set 0
    gotoUserLabel           .set 0
    gotoBreakLabel          .set 0
    chainedFlag             .set 0
    useLongJump             .set -1 ; invalid, needs to be set below
    
    ; these are initialized before use:
    ; foundTokenPosition      .set 0
    ; foundOR_AND             .set 0
    ; scanAheadBracketLevel   .set 0
    ; lowestBracketLevel      .set 0
    ; scanAheadNegateBrackets .set 0
    ; statementStartPos       .set 0
    ; statementTokenCount     .set 0
    ; foundAND                .set 0
    ; foundOR                 .set 0
    
    .if FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE
        ___error "Cannot use 'if' statement from within conditional expression."
    .endif
    FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE .set 1
    FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT .set -1       ; -1 means invalid. only valid after an ENDIF macro
    
    ; check additional parameters as options in any order:
    .define _IF_OPT0_ opt0
    .define _IF_OPT1_ opt1
    .repeat 2, i
        .ifnblank .left(1, .ident(.sprintf("_IF_OPT%d_", i)))
            .if .xmatch( .left(1, .ident(.sprintf("_IF_OPT%d_", i))) , long) || .xmatch( .left(1, .ident(.sprintf("_IF_OPT%d_", i))) , short)
                .if useLongJump = -1
                    useLongJump .set .xmatch( .left(1, .ident(.sprintf("_IF_OPT%d_", i))) , long)
                .else
                    ___error "Option already set."
                .endif    
            .elseif .xmatch( .left(1, .ident(.sprintf("_IF_OPT%d_", i))) , chain)
                chainedFlag .set 1
            .else
                ___error "Invalid option. Valid options are 'chain', or one of 'long', 'short'."
            .endif
        .endif
    .endrepeat
    .undefine _IF_OPT0_
    .undefine _IF_OPT1_
    
    ; array for label locations: (uses global to reuse ident)
    .define tokenPositionForBranchLabel(c)  ::.ident(.sprintf("POS_FOR_BRANCH_%02X", c))    
    startTokenListEval {condition}    ; use token macros to make processing tokens easier
    ; --------------------------------------------------------------------------------------------
    ; verify brackets and find total tokens for the condition excluding goto/break
    previousToken             ; step back before first token
    verifyNextToken {(}       ; make sure to start with a '('
    nextToken                 ; check it is a '('
    saveTokenListPosition
    allowAllTokens
    ; skip the first '('
    nextToken
    bracketLevel .set 1
    .repeat .tcount({condition}) - 1
        .if xmatchToken {(}
            .if bracketLevel = 0
                ___error "Mismatched parenthesis."
            .endif
            bracketLevel .set bracketLevel + 1
        .elseif xmatchToken {)}
            bracketLevel .set bracketLevel - 1
            conditionTokenCount .set currentTokenNumber + 1
        .endif
        .if bracketLevel < 0
            ___error "Mismatched parenthesis."
        .endif
        nextToken
    .endrepeat

    .if bracketLevel <> 0
        ___error "Mismatched parenthesis."
    .endif
    restoreTokenListPosition
    
    ; --------------------------------------------------------------------------------------------
    ; Find if there is a 'goto' or 'break' keyword and set the successful condition to branch to the label or break.
    ; If no 'goto' or 'break', invert the condition and branch to the ENDIF label on successful (inverted) condition.
    ; destinationLabel is the label to branch to if the (inverted) condition is 'true'
    .if conditionTokenCount < .tcount({condition})
        .if .xmatch( .mid(conditionTokenCount, 1, {condition}), goto )
            gotoUserLabel .set 1
            .define destinationLabel .mid(conditionTokenCount + 1, .tcount({condition}) - conditionTokenCount - 1, {condition}) ; capture everything after the 'goto'
        .elseif .xmatch( .mid(conditionTokenCount, 1, {condition}), break )
            gotoBreakLabel .set 1
             ___verifyBreakInsideLoop
            ; peek at stack:
            ___popLoopType
            ___pushLoopType ___loopType
            .define ___breakStatementCounter() .ident(.sprintf("BREAK_COUNT_%s", ___loopType))
            .define destinationLabel .ident( .sprintf( "BREAK_STATEMENT_%s_LABEL_%04X", ___loopType, FLOW_CONTROL_VALUES::___breakStatementCounter))
        .else
            .if FLOW_CONTROL_VALUES::INTERNAL_CALL
                ___error "Error in expression."
            .else
                ___error "'goto' or 'break' expected."
            .endif
        .endif
        setTokenCount conditionTokenCount ; set max tokens for EOT to exclude the goto and label
    .else 
        ; invert condition to branch to the next ENDIF label
        negateBracketSet .set !negateBracketSet
        ; if 'chain' option passed, branch to the end of the enclosing IF-ENDIF block
        .if chainedFlag
            .local ENCLOSING_IF_STATEMENT_COUNT
            stackPeek "IF_STATEMENT_STACK", ENCLOSING_IF_STATEMENT_COUNT
            
            .if ENCLOSING_IF_STATEMENT_COUNT = -1
                ___error "Invalid chain option."
            .endif    
            stackPush "IF_STATEMENT_CHAIN_NEXT_ENDIF", ENCLOSING_IF_STATEMENT_COUNT
            .define destinationLabel .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", ENCLOSING_IF_STATEMENT_COUNT ))
        .else
            .define destinationLabel .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF_LABEL", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT ))
        .endif
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; Warn of unneeded long or short setting passed to the macro
    .if useLongJump <> -1
        .ifdef destinationLabel
            depreciateLongBranch
        .endif
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; no long jump option passed, check for long branches if backward branch, or use setLongBranch setting
    .if useLongJump = -1
        .ifdef destinationLabel ; if backward branch, assume long jump could be needed and check during branch generation
            useLongJump .set 1
        .else
            useLongJump .set FLOW_CONTROL_VALUES::LONG_JUMP_ACTIVE
        .endif
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; Main loop: evaluate branches and AND OR conditions. Loop over all tokens, exclude 
    ; goto/break and anything after. More than one token will be consumed in the 5th case below, 
    ; so verify that we are not at EOT (End Of Tokens)
    .repeat conditionTokenCount
    .if !EOT
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken {!}
            negateNext .set !negateNext           
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
                    .ifndef .ident( .sprintf( "IF_STATEMENT_%04X_BRANCH_TO_TOKEN_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, currentTokenNumber ))
                        .ident( .sprintf( "IF_STATEMENT_%04X_BRANCH_TO_TOKEN_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, currentTokenNumber )):
                    .endif
                .endif
            .endrepeat
            verifyNextToken { ! abc a x y ( :: }
            nextToken
        ; --------------------------------------------------------------------------------------------
        .elseif matchToken {abc} || xmatchToken{a} || xmatchToken{x} || xmatchToken{y} || matchToken {::} ; something that could be an identifier, register, or branch setting
            ; find statementStartPos and statementTokenCount: find end of statement, but ignore anything in ()
            statementStartPos .set currentTokenNumber
            allowAllTokens
            scanAheadBracketLevel .set 0    ; use as temp
            .repeat conditionTokenCount - currentTokenNumber
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
            .if negateNext ^ negateBracketSet
                ___invertBranchCondition
            .endif
            negateNext .set 0
            
            ; save token position and scan ahead in the token list temporarily:
            allowAllTokens              
            saveTokenListPosition
            saveStackPointer "_IF_NEGATE_STACK_"
            foundTokenPosition      .set 0
            foundOR_AND             .set 0
            scanAheadBracketLevel   .set bracketLevel
            scanAheadNegateBrackets .set negateBracketSet
            ; skip immediate and repeated closed braces: eg. for 'N set' (marked in quotes): if (( C set || N set')' && V set)
            .repeat conditionTokenCount - currentTokenNumber
            .if (!EOT) && xmatchToken {)}
                scanAheadBracketLevel .set scanAheadBracketLevel - 1
                stackPop "_IF_NEGATE_STACK_", scanAheadNegateBrackets
                nextToken
            .endif
            .endrepeat
            ; lowestBracketLevel saves lowest bracket level found when scanning ahead:
            lowestBracketLevel .set scanAheadBracketLevel
            ; --------------------------------------------------------------------------------------------
            ; Where to branch to depends on if there is an '&&' or '||' following that applies to this branch
            ___xmatchSpecial {&&}, foundAND, scanAheadNegateBrackets ; special token match, considers negated bracket set
            .if foundAND
                ; Example: C set && V set || Z set
                ;  - Pass: C is set: do not branch
                ;  - Fail: (branch on inverted condition) to next '||' in this bracket level or lower
                ; If no '||' then overall condition is fail, exit branch evaluation
                ___invertBranchCondition ; always invert when an AND after this branch
                nextToken ; skip '&&'
                .repeat conditionTokenCount - currentTokenNumber
                .if (!EOT) && (!foundTokenPosition)
                    .if xmatchToken {(}
                        scanAheadBracketLevel .set scanAheadBracketLevel + 1
                        stackPush "_IF_NEGATE_STACK_", scanAheadNegateBrackets   ; keep the stack in order
                    .elseif xmatchToken {)}
                        scanAheadBracketLevel .set scanAheadBracketLevel - 1
                        .if scanAheadBracketLevel < lowestBracketLevel
                            lowestBracketLevel .set scanAheadBracketLevel
                        .endif
                        stackPop "_IF_NEGATE_STACK_", scanAheadNegateBrackets
                        
                    ; Branch to any '||' in the branch's bracket level or lower. ie ( scanAheadBracketLevel = lowestBracketLevel )
                    ; When negated, '||' (an inverted '&&') must be on a lower bracket level to maintain AND precedence.
                    .elseif scanAheadBracketLevel = lowestBracketLevel && ( lowestBracketLevel < bracketLevel || (!scanAheadNegateBrackets) )
                        ___xmatchSpecial {||}, foundOR_AND, scanAheadNegateBrackets
                        .if foundOR_AND
                            foundTokenPosition .set currentTokenNumber
                        .endif
                    .endif
                    nextToken
                .endif
                .endrepeat
            ; --------------------------------------------------------------------------------------------
            ; END &&
            .else
                ___xmatchSpecial {||}, foundOR, scanAheadNegateBrackets ; special token match, considers negated bracket set
                .if foundOR
                    ; Example: C set || V set && Z set
                    ;  - Pass: C is set: branch to next '&&' in this bracket level, or lower
                    ;       NOTE: This is correct for left to right, to allow '&&' to have priority only branch to && on lower bracket level
                    ;  - Fail: do not branch
                    ;  If no '&&' then overall condition is pass, branch to label
                    nextToken ; skip '||'
                    .repeat conditionTokenCount - currentTokenNumber
                    .if (!EOT) && (!foundTokenPosition)
                        .if xmatchToken {(}
                            scanAheadBracketLevel .set scanAheadBracketLevel + 1
                            stackPush "_IF_NEGATE_STACK_", scanAheadNegateBrackets   ; keep the stack in order
                        .elseif xmatchToken {)}
                            scanAheadBracketLevel .set scanAheadBracketLevel - 1
                            .if scanAheadBracketLevel < lowestBracketLevel
                                lowestBracketLevel .set scanAheadBracketLevel
                            .endif
                            stackPop "_IF_NEGATE_STACK_", scanAheadNegateBrackets
                            
                        ; Branch to an '&&' only in a lower bracket level to give AND precedence. ie:
                        ; scanAheadBracketLevel = lowestBracketLevel && ( lowestBracketLevel < bracketLevel )
                        ; When negated, a negated '||' will also match, but in this case:
                        ; Branch to this bracket level or lower to maintain AND precedence.
                        .elseif scanAheadBracketLevel = lowestBracketLevel && ( lowestBracketLevel < bracketLevel || scanAheadNegateBrackets )
                            ___xmatchSpecial {&&}, foundOR_AND, scanAheadNegateBrackets
                            .if foundOR_AND
                                foundTokenPosition .set currentTokenNumber
                            .endif
                        .endif
                        nextToken
                    .endif
                    .endrepeat
                .endif
            .endif
            ; --------------------------------------------------------------------------------------------
            ; END ||
            
            ; if label is a lower address, and in reach of a branch:
            longJumpNotNeeded .set .def(destinationLabel) && ( * + 2 - destinationLabel <= 128 )
            .if foundTokenPosition
                ; branch to next appropriate branch following an '&&' or '||'
                .define branchToLabel .ident(.sprintf( "IF_STATEMENT_%04X_BRANCH_TO_TOKEN_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, foundTokenPosition))
                tokenPositionForBranchLabel{branchLabelCounter} .set foundTokenPosition
                branchLabelCounter .set branchLabelCounter + 1    
            .elseif useLongJump && (!longJumpNotNeeded) ; !longJumpNotNeeded: long jump needed, or unknown if needed
                .ifndef firstBranchToLongJump
                    firstBranchToLongJump = * + 2 ; address for end of next branch
                .endif
                .if foundAND
                    ; branch to failed on inverted condition
                    .define branchToLabel exitBranchEvaluation 
                .elseif foundOR
                    .define branchToLabel doLongJump
                .else
                    ; no AND or OR found that affects this branch: single or last condition => skip the JMP on inverted test
                    ___invertBranchCondition
                    .define branchToLabel exitBranchEvaluation
                .endif
            .elseif foundAND
                ; branch to failed on inverted condition
                .define branchToLabel exitBranchEvaluation
            .else ; foundOR, OR no || or && found that affects this branch:
                .define branchToLabel destinationLabel
            .endif
            ___Branch branchFlag, branchCondition, branchToLabel    ; output the branch
            ___clearBranchSet                                       ; clear temporary settings
            restoreTokenListPosition
            restoreStackPointer "_IF_NEGATE_STACK_" 
            .undefine branchToLabel
        .endif
        ; --------------------------------------------------------------------------------------------
        ; END .if matchToken {abc} || xmatchToken{a} || xmatchToken{x} || xmatchToken{y} || matchToken {::}
    .endif
    .endrepeat
    
    ; when long jump active, JMP to destinationLabel
    .ifdef firstBranchToLongJump
        doLongJump:
        jmp destinationLabel
        .if FLOW_CONTROL_VALUES::LONG_JUMP_WARNINGS
            .ifndef destinationLabel
                .assert destinationLabel - firstBranchToLongJump - 3 > 127, warning, "Branch can be reached without a long branch. (Try 'setLongBranch -')."
            .endif    
        .endif
    .endif
    
    ; Local label for exiting branch code for this evaluation. Branch here when a condition fails, 
    ; which also is the start of a code block for an IF..ENDIF since it branches to ENDIF on an 
    ; inverted condition.
    exitBranchEvaluation:   

    .if gotoBreakLabel
        stackPush .sprintf("BREAK_STATEMENT_STACK_%s", ___loopType), FLOW_CONTROL_VALUES::___breakStatementCounter
        .undefine ___breakStatementCounter
    .elseif !gotoUserLabel
        stackPush "IF_STATEMENT_STACK", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT
    .endif
    
    FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT + 1    ; increase if statement count always
    FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE .set 0

    endTokenListEval ; clear token evaluation    
    .undefine destinationLabel
    .undefine tokenPositionForBranchLabel

.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___verifyChain
;
; Parameters:
;
;       _IF_STATEMENT_COUNT - The IF statement number that is being processed.
;
; This macro invoked only from ELSE or ELSEIF checks for any chain requests and verifies they are valid.
; It will also look for opportunities to recommend chains be used.
; Used by <elseif> and <else>.

.macro ___verifyChain _IF_STATEMENT_COUNT
    .local ENCLOSING_IF_STATEMENT_COUNT
    .if FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT <> -1 ; if last macro of IF..ELSEIF..ELSE..ENDIF was an ENDIF
        stackPop "IF_STATEMENT_CHAIN_NEXT_ENDIF", ENCLOSING_IF_STATEMENT_COUNT  ; pop, ignore if invalid stack, it will just fail this test:
        .if ENCLOSING_IF_STATEMENT_COUNT = _IF_STATEMENT_COUNT
            .assert .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF_LABEL", FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT )) = *, error, "Invalid chain option used." ; address must match last ENDIF
        .elseif ::__CA65HL_WARNING_LEVEL__ > 0
            .assert .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF_LABEL", FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT )) <> *, warning, "To optimize branch generation, apply chain to previous IF statement. eg: if (condition) , chain"
        .endif
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___checkForTailJMP
;
; Parameters:
;
;       knownFlagStatus -   if knownFlagStatus is 'jmp' this macro will check that the option is valid. If 'jmp' not used, it will 
;                           suggest that it could be used if it is valid to do so.
;
; Use this code with ca65hl to track jump commands. This is to .assert that an ELSE or ELSEIF was not proceeded by a jmp instruction.
; If it was, ca65hl will suggest to use 'jmp' option with the ELSE/ELSEIF macro that will suppress the macro's normal generation of a 
; jmp instruction to skip to the ENDIF. If 'jmp' option was used, it will verify that the usage is correct.
    
.macro ___checkForTailJMP knownFlagStatus
    .ifdef ::_CUSTOM_SYNTAX_        
        .if .xmatch( knownFlagStatus, jmp )
            .if _CUSTOM_::JMP_INSTRUCTION_COUNTER > 0
                .assert ::.ident( .sprintf( "JMP_INSTRUCTION_END_%04X", _CUSTOM_::JMP_INSTRUCTION_COUNTER - 1)) = *, warning, "No JMP immediately before ELSE/ELSEIF, possibly bad 'jmp' option."
            .else
                .warning "No JMP immediately before ELSE/ELSEIF, possibly bad 'jmp' option."
            .endif
        .else
            .if ( _CUSTOM_::JMP_INSTRUCTION_COUNTER > 0 ) && ( ::__CA65HL_WARNING_LEVEL__ > 0 )
                .assert ::.ident( .sprintf( "JMP_INSTRUCTION_END_%04X", _CUSTOM_::JMP_INSTRUCTION_COUNTER - 1)) <> *, warning, "JMP before ELSE/ELSEIF. Suggested: use 'jmp' option. eg: elseif (condition) , jmp"
            .endif
        .endif
    .endif    
.endmacro

; --------------------------------------------------------------------------------------------
; Function: elseif condition, knownFlagStatus
;
; Parameters:
;
;   condition - Conditional expression to evaluate.
;
;   options   - In any order, 'long', 'short', 'jmp', or flag status.If a flag is known to be in a state when the elseif is encountered, 
;               branch to the end if using this flag as a branch, using the syntax for <setBranch>.
;               If 'long' or 'short', force long or short branch to the end of the code block.
;               'jmp' to suppress the output of a jmp to the endif.
;
;   See Also:
;   <setBranch>, <endif>, <if>, <else>

.macro elseif condition, opt0, opt1

    .local IF_STATEMENT_COUNT
    .local useLongJump ; not used, other than to check for repeated use in options, possible future use
    .local knownFlagStatusOptionNumber
    
    useLongJump  .set -1
    knownFlagStatusOptionNumber .set -1
    
    stackPeek "IF_STATEMENT_STACK", IF_STATEMENT_COUNT ; just look, don't touch
    .if IF_STATEMENT_COUNT < 0 
        ___error "'elseif' without 'if'"
    .endif
    
    .ifdef .ident( .sprintf( "_IF_STATEMENT_ELSE_%04X_DEFINED", IF_STATEMENT_COUNT ))
        ___error "Not allowed: 'elseif' after 'else'."
    .endif
    
    ; check additional parameters as options in any order:
    ; (figure out which option is for knownFlagStatus)
    .define _ELSEIF_OPT0_ opt0
    .define _ELSEIF_OPT1_ opt1
    .repeat 2, i
        .ifnblank .left(1, .ident(.sprintf("_ELSEIF_OPT%d_", i)))
            .if .xmatch( .left(1, .ident(.sprintf("_ELSEIF_OPT%d_", i))) , long) || .xmatch( .left(1, .ident(.sprintf("_ELSEIF_OPT%d_", i))) , short)
                .if knownFlagStatusOptionNumber = -1
                    knownFlagStatusOptionNumber .set !i
                .endif
                .if useLongJump = -1
                    useLongJump .set .xmatch( .left(1, .ident(.sprintf("_ELSEIF_OPT%d_", i))) , long)
                .else
                    ___error "Option already set."
                .endif    
            .else
                .if knownFlagStatusOptionNumber = -1
                    knownFlagStatusOptionNumber .set i
                .endif
            .endif
        .endif
    .endrepeat
    
    ; if something found, define the options:
    .if knownFlagStatusOptionNumber <> -1
        .define knownFlagStatus()   .left(1, .ident(.sprintf("_ELSEIF_OPT%d_",  knownFlagStatusOptionNumber)))
        .define branchtype()        .left(1, .ident(.sprintf("_ELSEIF_OPT%d_", !knownFlagStatusOptionNumber)))
    .else
        .define knownFlagStatus
        .define branchtype
    .endif
    
    ___verifyChain IF_STATEMENT_COUNT
    
    ___checkForTailJMP knownFlagStatus
    
    ; jump to endif
    .ifnblank knownFlagStatus
        .if !.xmatch( knownFlagStatus, jmp)
            setBranch knownFlagStatus
            ___Branch branchFlag, branchCondition, .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", IF_STATEMENT_COUNT ))
            ___clearBranchSet
        .endif
    .else
        jmp .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", IF_STATEMENT_COUNT ))
    .endif
    
    ; elseif counter for this if statement:
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )
    ; if not defined, it means there are no previous ELSEIF, so create a label for the originating IF 
    .ifndef ELSE_IF_COUNT
        ; set the endif label for the original IF:
        .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF_LABEL", IF_STATEMENT_COUNT )):
        ELSE_IF_COUNT .set -1 ; start at -1, will be incremented to 0
    .else
        ; this isn't the first ELSEIF: 
        .ident( .sprintf( "IF_STATEMENT_%04X_ELSEIF_LABEL_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )):
    .endif
    
    ELSE_IF_COUNT .set ELSE_IF_COUNT + 1
    ; negate statement to GOTO the next ELSEIF/ELSE/ENDIF on failed condition 
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if { condition goto .ident( .sprintf( "IF_STATEMENT_%04X_ELSEIF_LABEL_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )) }, branchtype
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    
    .undefine ELSE_IF_COUNT
    .undefine _ELSEIF_OPT0_
    .undefine _ELSEIF_OPT1_    
    .undefine knownFlagStatus
    .undefine branchtype     
.endmacro

; --------------------------------------------------------------------------------------------
; Function: else knownFlagStatus
;
; Parameters:
;
;   knownFlagStatus - Optional - if a flag is known to be in a state when the else
;                     is encountered, branch to the end if using this flag as a branch 
;                     always, using the syntax for <setBranch>
;
;   See Also:
;   <setBranch>, <endif>, <elseif>, <if>

.macro else knownFlagStatus
    
    .local IF_STATEMENT_COUNT 
    stackPeek "IF_STATEMENT_STACK", IF_STATEMENT_COUNT ; just look, don't touch
    .if IF_STATEMENT_COUNT < 0 
        ___error "'else' without 'if'"
    .endif
    
    .ifdef .ident( .sprintf( "_IF_STATEMENT_ELSE_%04X_DEFINED", IF_STATEMENT_COUNT ))
        ___error "Duplicate 'else'."
    .endif
    ; mark this IF as having an ELSE
    .ident( .sprintf( "_IF_STATEMENT_ELSE_%04X_DEFINED", IF_STATEMENT_COUNT )) = 1
    
    ___verifyChain IF_STATEMENT_COUNT
    ___checkForTailJMP knownFlagStatus
    
    ; jump to endif:
    .ifnblank knownFlagStatus
        .if !.xmatch( knownFlagStatus, jmp)
            setBranch knownFlagStatus
            ___Branch branchFlag, branchCondition, .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", IF_STATEMENT_COUNT ))
            ___clearBranchSet
        .endif
    .else
        jmp .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", IF_STATEMENT_COUNT ))
    .endif
    
    ; elseif counter for this if statement:
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )    
    ; if ELSE_IF_COUNT is defined, means there are one or more ELSEIF, so create a label for the last one,
    ; otherwise, create a label for the originating IF
    .ifdef ELSE_IF_COUNT
        .ident( .sprintf( "IF_STATEMENT_%04X_ELSEIF_LABEL_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )):
        ELSE_IF_COUNT .set -1 ; signal it is not needed for the endif macro
    .else
        .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF_LABEL", IF_STATEMENT_COUNT )):
    .endif
    
    .undefine ELSE_IF_COUNT
    
.endmacro

; --------------------------------------------------------------------------------------------
; Function: endif
;
; End an <if> statement.
;
; Parameters: none
;
;   See Also:
;   <if>, <elseif>, <else>

.macro endif

    .local IF_STATEMENT_COUNT
    ; get LIFO label counter
    stackPop "IF_STATEMENT_STACK", IF_STATEMENT_COUNT
    ; check if all okay
    .if IF_STATEMENT_COUNT < 0 
        ___error "'endif' without 'if'"
    .endif
        
    ; if label was referenced, it means there was an ELSE or ELSEIF, so create the label,
    ; otherwise, create a label for the originating IF
    .ifref .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", IF_STATEMENT_COUNT ))
        .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF_LABEL", IF_STATEMENT_COUNT )):
        ; if there was an ELSEIF and last ELSEIF label not handled by an ELSE:
        .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT))
        .ifdef ELSE_IF_COUNT
            .if ELSE_IF_COUNT <> -1 ; -1 means ELSE handled the last ELSEIF label
                .ident( .sprintf( "IF_STATEMENT_%04X_ELSEIF_LABEL_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )):
            .endif
        .endif
        .undefine ELSE_IF_COUNT 
    .else
        .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF_LABEL", IF_STATEMENT_COUNT )):
    .endif
    FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT .set IF_STATEMENT_COUNT
.endmacro

; --------------------------------------------------------------------------------------------
; Function: do
;
; Start a do..while loop.
;
; Parameters: none

.macro do
    stackPush "DO_WHILE_LOOP_STATEMENT_STACK", FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT
    .ident( .sprintf( "DO_WHILE_LOOP_LABEL_%04X", FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT)):
    FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT + 1
    ___pushLoopType "DOWHILE"
.endmacro

; --------------------------------------------------------------------------------------------
; Function: while condition
;
; While the condition is true, branch back to <do>.
;
; Parameters:
;
;   condition - Conditional expression to evaluate.
;
;   See Also:
;   <do>, <repeat>, <until>

.macro while condition, branchtype
    .if .xmatch(.right(1, {condition}), do) ; if match 'do' at end this is a while..do..endwhile statement
        while_do {.mid(0, .tcount({condition}) - 1, {condition}) }
    .else
        .local DO_WHILE_STATEMENT_COUNT
        stackPop "DO_WHILE_LOOP_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
        ; check if all okay
        .if DO_WHILE_STATEMENT_COUNT < 0 
            ___error "'while' without 'do'"
        .endif
        FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
        if { condition goto .ident( .sprintf( "DO_WHILE_LOOP_LABEL_%04X", DO_WHILE_STATEMENT_COUNT)) }, branchtype
        FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
        ___popLoopType
        ___generateBreakLabel
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: do
;
; Start a repeat..until loop.
;
; Parameters: none

.macro repeat
    do
.endmacro

; --------------------------------------------------------------------------------------------
; Function: until condition
;
; Until the condition is true, branch back to <repeat>.
;
; Parameters:
;
;   condition - Conditional expression to evaluate.
;
;   See Also:
;   <do>, <while>, <repeat>

.macro until condition, branchtype
    .local DO_WHILE_STATEMENT_COUNT
    stackPop "DO_WHILE_LOOP_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
    ; check if all okay
    .if DO_WHILE_STATEMENT_COUNT < 0 
        ___error "'until' without 'repeat'"
    .endif
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if { condition goto .ident( .sprintf( "DO_WHILE_LOOP_LABEL_%04X", DO_WHILE_STATEMENT_COUNT)) }, branchtype
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    ___popLoopType
    ___generateBreakLabel
.endmacro

; --------------------------------------------------------------------------------------------
; Function: forever
;
; Always branch back to <repeat> or <do>.
;
; Parameters:
;
;   See Also:
;   <do>, <while>, <repeat>

.macro forever branchtype
    .local DO_WHILE_STATEMENT_COUNT
    stackPop "DO_WHILE_LOOP_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
    ; check if all okay
    .if DO_WHILE_STATEMENT_COUNT < 0 
        ___error "Missing 'repeat' or 'do'"
    .endif
    jmp .ident( .sprintf( "DO_WHILE_LOOP_LABEL_%04X", DO_WHILE_STATEMENT_COUNT))
    ___popLoopType
    ___generateBreakLabel
.endmacro

; --------------------------------------------------------------------------------------------
; Function: while_do condition
;
; While the condition is true, repeat the code block to <endwhile>.
;
; Parameters: none
;
; Invoke with 'while (condition) do'. Requires 'do' after the 
; condition to indicate it is the beginning of a code block.
;
;   See Also:
;   <do>, <while>, <repeat>, <until>

.macro while_do condition, branchtype
    ___pushLoopType "WHILEDO"
    stackPush "WHILE_DO_ENDWHILE_LOOP_STATEMENT_STACK", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT                      ; save counter
    .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_START_LABEL_%04X", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT)):
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if {condition goto .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_EXIT_LABEL_%04X", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT))}, branchtype
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT + 1      ; increment while-do counter
.endmacro

; --------------------------------------------------------------------------------------------
; Function: endwhile knownFlagStatus
;
; Mark the end of a 'while condition do' code block
;
; Parameters: none
;
;   knownFlagStatus - Optional - if a flag is known to be in a state when the else
;                     is encountered, branch to the end if using this flag as a branch 
;                     always, using the syntax for <setBranch>
;
;   See Also:
;   <do>, <while>, <repeat>, <until>
;

.macro endwhile knownFlagStatus
    .local WHILE_DO_ENDWHILE_STATEMENT_COUNT
    stackPop "WHILE_DO_ENDWHILE_LOOP_STATEMENT_STACK", WHILE_DO_ENDWHILE_STATEMENT_COUNT                                        ; get the counter
    .if WHILE_DO_ENDWHILE_STATEMENT_COUNT < 0                                                                                   ; error check
        ___error "'endwhile' without 'while-do'"
    .endif
     ; branch or JMP to start of loop
    .ifnblank knownFlagStatus
        setBranch knownFlagStatus
        ___Branch branchFlag, branchCondition, .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_START_LABEL_%04X", WHILE_DO_ENDWHILE_STATEMENT_COUNT))
        ___clearBranchSet
    .else
        jmp .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_START_LABEL_%04X", WHILE_DO_ENDWHILE_STATEMENT_COUNT))
    .endif
    .ident( .sprintf( "WHILE_DO_ENDWHILE_LOOP_EXIT_LABEL_%04X", WHILE_DO_ENDWHILE_STATEMENT_COUNT)):
    ___popLoopType
    ___generateBreakLabel
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___pushLoopType
;
; Keep track of the current type of loop for generating breaks.
;
; Parameters: Type of loop that was started
; Valid types:
;   DOWHILE
;   WHILEDO
;   FOR
;   SWITCH 

.macro ___pushLoopType loopType
    pushTokenList "LOOP_TYPE", loopType
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___popLoopType
;
; Retrieve the current loop type
;

.macro ___popLoopType 
    popTokenList "LOOP_TYPE"
    .if FLOW_CONTROL_VALUES::LOOP_TYPE_DEFINED
        .undefine ___loopType
    .endif
    .define ___loopType poppedTokenList
    FLOW_CONTROL_VALUES::LOOP_TYPE_DEFINED .set 1
.endmacro
    

; --------------------------------------------------------------------------------------------
; Function: ___verifyBreakInsideLoop
;
; Verify break is being invoked from within a loop or structure that allows break.
;
; Parameters: none

.macro ___verifyBreakInsideLoop
    .local doWhileLoop
    .local whileDoLoop
    .local forLoop
    .local switchCount
    stackPeek "DO_WHILE_LOOP_STATEMENT_STACK", doWhileLoop
    stackPeek "WHILE_DO_ENDWHILE_LOOP_STATEMENT_STACK", whileDoLoop
    stackPeek "FOR_STATEMENT_STACK", forLoop
    stackPeek "SWITCH_TABLE_STATEMENT_STACK", switchCount
    .if doWhileLoop < 0 && whileDoLoop < 0 && forLoop < 0 && switchCount < 0
        ___error "Invalid 'break'."
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: break knownFlagStatus
;
; Break from a loop. Branch or JMP out of a code block from inside a loop.
;
; Parameters:
;   knownFlagStatus - Optional - if a flag is known to be in a state when the else
;                     is encountered, branch to the end if using this flag as a branch 
;                     always, using the syntax for <setBranch>

.macro break knownFlagStatus
    ___verifyBreakInsideLoop
    ; peek at stack:
    ___popLoopType
    ___pushLoopType ___loopType
    .define ___breakStatementCounter() .ident(.sprintf("BREAK_COUNT_%s", ___loopType))
    .ifnblank knownFlagStatus
        setBranch knownFlagStatus
        ___Branch branchFlag, branchCondition, .ident( .sprintf( "BREAK_STATEMENT_%s_LABEL_%04X", ___loopType, FLOW_CONTROL_VALUES::___breakStatementCounter))
        ___clearBranchSet
    .else
        jmp .ident( .sprintf( "BREAK_STATEMENT_%s_LABEL_%04X", ___loopType, FLOW_CONTROL_VALUES::___breakStatementCounter))
    .endif
    stackPush .sprintf("BREAK_STATEMENT_STACK_%s", ___loopType), FLOW_CONTROL_VALUES::___breakStatementCounter
    .undefine ___breakStatementCounter
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___generateBreakLabel checkMoreBreakStatements
;
; Invoked at the end of a loop macro to check if any labels
; need to be created for a break command from inside that loop.
; Loop type should be popped into __loopType with ___popLoopType before this is called.
;
; Parameters:
;   checkMoreBreakStatements - used in recursion to pop any duplicate break values 
;                              from the stack. (Created when more than one break
;                              statement is used in a loop.)

.macro ___generateBreakLabel checkMoreBreakStatements
    .local _BREAK_STATEMENT_COUNT
    stackPeek .sprintf("BREAK_STATEMENT_STACK_%s", ___loopType) , _BREAK_STATEMENT_COUNT  
    .ifnblank checkMoreBreakStatements  ; recursive call to pop stack of any matching break statements
        .if _BREAK_STATEMENT_COUNT = checkMoreBreakStatements
            stackPop .sprintf("BREAK_STATEMENT_STACK_%s", ___loopType) , _BREAK_STATEMENT_COUNT
            ___generateBreakLabel _BREAK_STATEMENT_COUNT
        .endif
    .elseif _BREAK_STATEMENT_COUNT >= 0
        stackPop .sprintf("BREAK_STATEMENT_STACK_%s", ___loopType) , _BREAK_STATEMENT_COUNT
        .ident( .sprintf( "BREAK_STATEMENT_%s_LABEL_%04X", ___loopType, _BREAK_STATEMENT_COUNT)):
        ___generateBreakLabel _BREAK_STATEMENT_COUNT
        FLOW_CONTROL_VALUES::.ident(.sprintf("BREAK_COUNT_%s", ___loopType)) .set FLOW_CONTROL_VALUES::.ident(.sprintf("BREAK_COUNT_%s", ___loopType)) + 1
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: for
;
; C-style syntax for loop
;
; Parameters:
;   -  ( init, condition, increment ), strict
;
;   Requires brackets around a comma separated list of init, condition and increment
;   Values for <init> and <increment> can be any amount of instructions separated by ':'
;   and are both optional. The <condition> can be anything that follows conditional expression 
;   syntax for IF.
;
;   Code for <init> will always be executed. If any value is passed for <strict> the 
;   loop will only be executed after <condition> is checked. If <strict> is not used,
;   the loop will always be executed at least once. If it is clear the loop will be 
;   executed at least once, do not use strict - it avoids the generation of a JMP command.

.macro for init, condition, increment, op1

    ; --------------------------------------------------------------------------------------------
    ; error checks and defines:
    .if .blank({init}) || .blank({condition}) || .blank({increment})
        ___error "Error in statement."
    .endif
    .if .xmatch( .left(1, {init}), {(} )
        .define INIT () .mid(1, .tcount({init}) - 1, {init})
    .else
        ___error "'(' expected."
    .endif
    .if .xmatch( .right(1, {increment}), {)} )
        .define INCR () .left(.tcount({increment}) - 1, {increment})
    .else
        ___error "')' expected."
    .endif
    .define COND () ( condition )
    ; --------------------------------------------------------------------------------------------
    ___pushLoopType "FOR"
    ; execute the INIT, before the loop
    ___evaluateStatementList {INIT}
    
    .ifnblank op1
        .if !.xmatch(op1, strict)
            ___error "Expected: 'strict': Test condition before first loop is executed."
        .endif
        jmp .ident( .sprintf( "FOR_STATEMENT_LABEL_JMP_TO_CONDITION_%04X", FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNTER))
    .endif
    .ident( .sprintf( "FOR_STATEMENT_LABEL_%04X", FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNTER)):
    
    stackPush "FOR_STATEMENT_STACK", FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNTER
    FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNTER .set FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNTER + 1
    
    pushTokenList "FOR_STATEMENT_CONDITION", {COND}
    pushTokenList "FOR_STATEMENT_INCREMENT", {INCR}
    
    ; --------------------------------------------------------------------------------------------
    .undefine INCR
    .undefine INIT
    .undefine COND
   
.endmacro

; --------------------------------------------------------------------------------------------
; Function next
;
;   Parameters - none
;
;   End of a for loop. Outputs increment and condition code for corresponding FOR macro.

.macro next branchtype
    .local FOR_STATEMENT_COUNTER
    stackPop "FOR_STATEMENT_STACK", FOR_STATEMENT_COUNTER
    .if FOR_STATEMENT_COUNTER < 0
        ___error "'next' without 'for'."
    .endif
    ; if FOR_STATEMENT_STACK is valid, then popTokenList should be too:
    popTokenList "FOR_STATEMENT_INCREMENT"
    ___evaluateStatementList {poppedTokenList}
    ; jmp here on strict:
    .ident( .sprintf( "FOR_STATEMENT_LABEL_JMP_TO_CONDITION_%04X", FOR_STATEMENT_COUNTER)):
    popTokenList "FOR_STATEMENT_CONDITION"
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if { poppedTokenList goto .ident( .sprintf( "FOR_STATEMENT_LABEL_%04X", FOR_STATEMENT_COUNTER)) }, branchtype
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    ___popLoopType
    ___generateBreakLabel
.endmacro

; --------------------------------------------------------------------------------------------; --------------------------------------------------------------------------------------------
; Function setSwitchStatementDataSeg
;
;   Parameters -
;       string - string representing a valid segment for the linker.
;
; Set the segment for the table data for the 'switch' statement.

.macro setSwitchStatementDataSeg string

    .if FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG
        .undefine SWITCH_STATEMENT_DATA_SEG_STRING
    .endif
    .define SWITCH_STATEMENT_DATA_SEG_STRING string
    FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG .set 1

.endmacro

; --------------------------------------------------------------------------------------------
; Function switch
;
;   Parameters -
;       reg - CPU register or identifier. If parameter is not one of a, x or y, it will be assumed to be an CA65 identifier.
;       mode - Must be 'goto' or omitted. For 'goto' the case values must be in order starting at zero, and the code to
;              to look for a matching value will be skipped, and the addresses for the jump table loaded immediately.
;

; Additional notes:
; Macro 'switch' works with macros 'case', 'endswitch' to build a list of constants and corresponding address table to use as a jump table.
; if setSwitchStatementDataSeg is used first, the data table will be placed in the defined segment and will allow the macro to not have to include a JMP command to skip 
; the data tables.

.macro switch reg, mode

    .local gotoMode
    ___pushLoopType "SWITCH"
    .ifnblank mode
        .if .xmatch( {mode}, goto )
            gotoMode = 1
        .else
            ___error "Mode should be 'goto' for basic jump table, or omitted for out of order case values."
        .endif
    .else
        gotoMode = 0
    .endif
    
    .define DECREMENT_COMMAND dex   ; default
    .if !gotoMode
        .if .xmatch( reg, a )
            ;
        .elseif  .xmatch( reg, x )
            txa
        .elseif  .xmatch( reg, y )
            tya
        .else 
            lda reg
        .endif
        .define SWITCH_INFO_REG "X"
        .define INDEX_REG_FOR_TABLE x
    .else ; no compare, just branch on lookup value in reg. x or y
        .if .xmatch( reg, x )
            .define INDEX_REG_FOR_TABLE x
            .define SWITCH_INFO_REG "none"
        .elseif .xmatch( reg, a )
            tax
            .define INDEX_REG_FOR_TABLE x
            .define SWITCH_INFO_REG "X"
        .elseif .xmatch( reg, y )
            .define INDEX_REG_FOR_TABLE y
            .define SWITCH_INFO_REG "none"
            ;.undefine DECREMENT_COMMAND
            ;.define DECREMENT_COMMAND dey
        .else 
            ldx reg
            .define INDEX_REG_FOR_TABLE x
            .define SWITCH_INFO_REG "X"
        .endif
    .endif
    
    .if ::__CA65HL_WARNING_LEVEL__ > 1
        .warning .sprintf( "INFO: Switch: Register A value changed for switch. Register %s loaded/changed to index data for switch.", SWITCH_INFO_REG)
    .endif
    
    .if !gotoMode
        .local loop, found
        ldx #( .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_TOTAL_COUNT", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER)) - 1 )
        loop:
            cmp .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_CONSTANTS", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER)), INDEX_REG_FOR_TABLE
            beq found
            ;DECREMENT_COMMAND  
            dex
        bpl loop
        jmp .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER))
        found:
    .endif
    
    lda .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_HIBYTES", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER)), INDEX_REG_FOR_TABLE
    pha
    lda .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_LOBYTES", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER)), INDEX_REG_FOR_TABLE
    pha
    rts
   
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER)) = gotoMode
    stackPush "SWITCH_TABLE_STATEMENT_STACK", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER
    FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER .set FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNTER + 1
    .undefine INDEX_REG_FOR_TABLE
    .undefine SWITCH_INFO_REG
    .undefine DECREMENT_COMMAND
.endmacro

; --------------------------------------------------------------------------------------------
; Function case
;
;   Parameters
;       constant - immediate byte constant, eg #$23, or 'default' to define the default case.
; 
; The parameter must be a constant at assembly time. If it is 'default' no more case macros can be defined 
; for this switch.

.macro case constant

    ; get case statement number from the stack:
    .local SWITCH_STATEMENT_COUNTER
    stackPeek "SWITCH_TABLE_STATEMENT_STACK", SWITCH_STATEMENT_COUNTER
    .if SWITCH_STATEMENT_COUNTER < 0
        ___error "'case' without 'switch'."
    .endif
    
    ; no more if default has been created:
    .ifdef .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNTER))
        ___error "No more 'case' statements allowed after 'default'."
    .endif

    ; create the default label if requested and allowed:
    .if .xmatch( constant, default )
        .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", SWITCH_STATEMENT_COUNTER))
            ___error "Default case not valid in 'goto' mode."
        .endif
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNTER)):
        .exitmacro
    .endif
        
    ; must be immediate, must be constant:
    .if ! .xmatch(.left(1, {constant}) , #)
        ___error "Condition for 'case' must be immediate constant."
    .elseif ! .const(.mid(1, .tcount({constant}) - 1, {constant}))
        ___error "Condition for 'case' must be immediate constant."
    .endif
    
    ; keep track of the number of case macros used for this switch:
    .define thisSwitchCaseCounter .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_COUNTER", SWITCH_STATEMENT_COUNTER))
    .ifndef thisSwitchCaseCounter     ; if not defined, this is the first case for this switch
        thisSwitchCaseCounter .set 0
    .endif
    
    ; .define and save constant for this case:
    .define thisSwitchCaseConstant .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_CONSTANT", SWITCH_STATEMENT_COUNTER, thisSwitchCaseCounter))
    thisSwitchCaseConstant = .mid(1, .tcount({constant}) - 1, {constant})
    
    ; every case, check if the constants are well structured values starting at zero and incrementing:
    .if thisSwitchCaseCounter = 0  ; first case?
        .if thisSwitchCaseConstant = 0  ; if first case constant starts at zero, could be a simple goto index switch
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNTER)) .set 1
        .else
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNTER)) .set 0
        .endif
    .else ; not first case, check if this constant is equal to the old case constant + 1. If not, set to false
        .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_CONSTANT", SWITCH_STATEMENT_COUNTER, thisSwitchCaseCounter - 1 )) + 1 <> thisSwitchCaseConstant
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNTER)) .set 0
        .endif
    .endif

    ;label for this case:
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_LABEL", SWITCH_STATEMENT_COUNTER, thisSwitchCaseCounter)):    
    thisSwitchCaseCounter .set thisSwitchCaseCounter + 1
    
    .undefine thisSwitchCaseCounter
    .undefine thisSwitchCaseConstant
.endmacro

; --------------------------------------------------------------------------------------------
; Function endswitch
;
;   Parameters
;       none
; 
; End the switch code block. This macro will:
; - check if a used 'goto' option is valid, or if it should have been used.
; - define the label for exit if there are no matches to the case constants and no default case.
; - define the tables (optionally, in the segment set by setSwitchStatementDataSeg)
; - define label for 'break'

.macro endswitch
    .local SWITCH_STATEMENT_COUNTER
    .local exit
    stackPop "SWITCH_TABLE_STATEMENT_STACK", SWITCH_STATEMENT_COUNTER
    .if SWITCH_STATEMENT_COUNTER < 0
        ___error "'endswitch' without 'switch'."
    .endif
    
    ; if goto mode option used
    .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", SWITCH_STATEMENT_COUNTER)) 
        ; if NOT compliant
        .if !.ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNTER))
            ___error "Cannot use 'goto' mode with this case structure. Case values must start at zero and increment."
        .endif
    ; goto option not used:    
    .else
        .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNTER)) && (!.defined(.ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNTER))))
            .if ::__CA65HL_WARNING_LEVEL__ > 0
                .warning "Use 'goto' mode with this case structure. Case values start at zero and increment. eg: switch <value>, goto"
            .endif
        .endif
    .endif
    
    .if FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG
        .pushseg
        .segment SWITCH_STATEMENT_DATA_SEG_STRING            
    .else
        ; Jump over tables if in the same segment. It could be better to define this table at the beginning
        ; in the switch macro, but I don't see how to get ca65 to do this.
        jmp exit
    .endif
    
    ; define the tables:
    .define thisSwitchCaseCounter .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_COUNTER", SWITCH_STATEMENT_COUNTER))
    
    ; make constant table if not goto mode
    .if !.ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", SWITCH_STATEMENT_COUNTER))
        .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_CONSTANTS", SWITCH_STATEMENT_COUNTER)):
        .repeat thisSwitchCaseCounter, i
            .byte .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_CONSTANT", SWITCH_STATEMENT_COUNTER, i))
        .endrepeat
    .endif
    
    ; make lobyte table:
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_LOBYTES", SWITCH_STATEMENT_COUNTER)):
    .repeat thisSwitchCaseCounter, i
        .byte <( .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_LABEL", SWITCH_STATEMENT_COUNTER, i)) - 1 )
    .endrepeat
    
    ; make hibyte table:
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_HIBYTES", SWITCH_STATEMENT_COUNTER)):
    .repeat thisSwitchCaseCounter, i
        .byte >( .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_LABEL", SWITCH_STATEMENT_COUNTER, i)) - 1 )
    .endrepeat
    
    .if FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG
        .popseg
    .endif
    
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_TOTAL_COUNT", SWITCH_STATEMENT_COUNTER)) = thisSwitchCaseCounter
    .undefine thisSwitchCaseCounter
    exit:
    ; if no matches, 'switch' macro will jump here if no default case:
    .ifndef .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNTER))
        .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNTER)):
    .endif
    ___popLoopType
    ___generateBreakLabel
.endmacro

; --------------------------------------------------------------------------------------------


.endif
