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
; This file is only macro code, intended to add functionality. No memory is used and no supporting 6502 code needed.
;
; Some macros are intended to be 'private' to this file. They are are prefixed with a triple underscore.
; 
; Macros for use outside of this file:
;
; > setBranch
; > setLongBranch
; > mb  (Move Byte)
; > mw  (Move Word)
; > if
; > elseif
; > else
; > endif
; > do..while
; > repeat..until
; > forever
; > while <> do
; > endwhile
; > break
; > continue
; > for..next
; > switch..case..endswitch

; --------------------------------------------------------------------------------------------
; Note: types for ___loopType and corresponding labels
; 
; ___loopType               Start of Structure
;
; "FOR"                 -> "FOR_%04X_START"  
; "DO_WHILE"            -> "DO_WHILE_%04X_START"
; "WHILE_DO_ENDWHILE"   -> "WHILE_DO_ENDWHILE_%04X_START"  
; "SWITCH"              ->  No corresponding start label  
;
; Stack names:
;
; ___loopType               Stack names
;
; "FOR"                 -> "FOR_STATEMENT_STACK"
; "DO_WHILE"            -> "DO_WHILE_STATEMENT_STACK"
; "WHILE_DO_ENDWHILE"   -> "WHILE_DO_ENDWHILE_STATEMENT_STACK"
; "SWITCH"              -> "SWITCH_STATEMENT_STACK"

; --------------------------------------------------------------------------------------------

.ifndef ::CA65HL_H
::CA65HL_H = 1

; Set the following to 0 to disable customSyntax for instructions:
; ::CA65HL_USE_CUSTOM_SYNTAX = 0

; Set the following for warnings from ca65hl: (0 to 2)
; ::CA65HL_WARNING_LEVEL = 0

; Set the following for console output for debugging ca65hl
; ::CA65HL_DEBUG = 1

; --------------------------------------------------------------------------------------------

; Include macros to enable custom array syntax for instructions
.ifndef ::CA65HL_USE_CUSTOM_SYNTAX
    ::CA65HL_USE_CUSTOM_SYNTAX = 1
.endif

; Warn in some cases if turned on:
.ifndef ::CA65HL_WARNING_LEVEL
    ::CA65HL_WARNING_LEVEL = 2
.endif

; debugging for this file only. Change this to '1' for some console output.
.ifndef ::CA65HL_DEBUG
    ::CA65HL_DEBUG = 0
.endif

; check for funcCall.h - I don't remember why this is important
.ifdef ::_FUNC_CALL_H
    .warning "ca65hl.h should be included before funcCall.h"
.endif

; --------------------------------------------------------------------------------------------
.include "stacks.h"             ; macros that allow for named stacks 
.include "tokeneval.h"          ; macros to make token evaluation easier
.include "debug.h"              ; macros to help with debugging
.if ::CA65HL_USE_CUSTOM_SYNTAX
    .include "customSyntax.h"   ; macros to enable allow custom syntax for instructions
.endif

; --------------------------------------------------------------------------------------------
; symbols to track some values for if, loops, etc

.scope FLOW_CONTROL_VALUES
    IF_STATEMENT_COUNT                  .set 0  ; if statement label counter - always incremented after every 'if'
    LAST_ENDIF_COUNT                    .set 0  ; keep track of the IF count of last IF-ENDIF block
    DO_WHILE_STATEMENT_COUNT            .set 0  ; while loop counter 
    WHILE_DO_ENDWHILE_STATEMENT_COUNT   .set 0  ; while..do endwhile counter
    FOR_STATEMENT_COUNT                 .set 0  ; for statement counter
    SWITCH_STATEMENT_COUNT              .set 0  ; switch statement counter
    SWITCH_STATEMENT_DATA_SEG_DEFINED   .set 0  ; flag: if on, use data segment defined by switch_SetDataSeg
    NEGATE_CONDITION                    .set 0  ; flag: if on, conditions are inverted
    IF_STATEMENT_ACTIVE                 .set 0  ; flag: if executing an 'if' macro (no calling an 'if' while a condition is being processed)
    LONG_JUMP_ACTIVE                    .set 0  ; flag: use JMP to branch
    LONG_JUMP_WARNINGS                  .set 1  ; flag: output warnings if long jump not needed
    INTERNAL_CALL                       .set 0  ; flag: if on, 'if' macro being invoked from this file.
    LOOP_TYPE_DEFINED                   .set 0  ; flag: only undefine ___loopType if set
    PRINT_LABELS                        .set 0  ; send macro generated labels to console
    MB_RIGHT_SIDE_ONLY                  .set 0
.endscope

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
; Function: printTokenListDebugCA65HL
;
; Parameters:
;
;   parameter - token list to be displayed. Enclose in {} to include commas
;
;   If the option is enabled, ca65hl will display some information to the console
;   on what it is processing.

.macro printTokenListDebugCA65HL parameter
    .if ::CA65HL_DEBUG
        printTokenList {parameter}
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ca65hl_Listing param, tok, position
;
; Parameters:
;
;   opt1 -  Must be 'on' or 'off' (not in quotes) to turn listing on or off.
;   comment - String to print as a comment to the console
;
;   This macro turns on or off the listing of instructions and labels to the console.

.macro ca65hl_Listing opt1, comment
    .if .xmatch( opt1, on )
        FLOW_CONTROL_VALUES::PRINT_LABELS .set 1
    .elseif .xmatch( opt1, off )
        FLOW_CONTROL_VALUES::PRINT_LABELS .set 0
    .else
        .error "Invalid option. Should be: 'on' or 'off'."
    .endif
    .if ::CA65HL_USE_CUSTOM_SYNTAX
        customSyntax_Output opt1
    .endif
    .ifnblank comment
        .out .concat("; ", comment)
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___emitLabel
;
; Parameters:
;
;   labelName - name of label as a string
;
;   This macro creates a label with the name of the string, as well as optionally
;   printing the label to the console.

.macro ___emitLabel labelName
    .if FLOW_CONTROL_VALUES::PRINT_LABELS
        .out .concat(labelName, ":")
    .endif
    .ident(labelName):
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
; Function: deprecateLongBranch
;
; Parameters:
;
;   none    
;
; If long or short branch forced, will be ignored for backward branches.
; Warn that this is the case. This will be removed in future versions.

.macro deprecateLongBranch
    .warning "Long branch option depreciated for backward branches. Backward branches automatically generated!"        
.endmacro

; --------------------------------------------------------------------------------------------
; SECTION: Compare

.scope ___compare
    found       .set 0
    operator    .set 0
    position    .set 0
    regFound    .set 0
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

    .local REGA, REGX, REGY
    REGA = 1
    REGX = 2
    REGY = 3

    .define _LEFT ()  .mid (0, ___compare::position, {exp})
    .define _RIGHT () .mid (___compare::position + 1, .tcount({exp}) - ___compare::position - 1, {exp})
    
    ; first look for a register 
    ; check if register on the right side. Left is always the register.
    left .set 0
    .if .xmatch (_RIGHT, a)
        left .set REGA
    .elseif .xmatch (_RIGHT, x)
        left .set REGX
    .elseif .xmatch (_RIGHT, y)
        left .set REGY
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
            left .set REGA
        .elseif .xmatch (_LEFT, x)
            left .set REGX
        .elseif .xmatch (_LEFT, y)
            left .set REGY
        .endif
    .endif
    ; no registers found, so first we will load a register, or eval an expression
    .if !left
        .define _LEFT1 .left (1, {_LEFT})
        ; look for characters, then see if they match a supported instruction
        .if .match( _LEFT1, abc)                        
            .if .xmatch(_LEFT1,lda)
                left .set REGA
            .elseif .xmatch(_LEFT1,ldx)
                left .set REGX
            .elseif .xmatch(_LEFT1,ldy)
                left .set REGY
            .elseif .xmatch(_LEFT1,inx)
                left .set REGX
            .elseif .xmatch(_LEFT1,dex)
                left .set REGX
            .elseif .xmatch(_LEFT1,iny)
                left .set REGY
            .elseif .xmatch(_LEFT1,dey)
                left .set REGY
            .endif
            ; if match found, output the instruction
            .if left
                _LEFT1 {.right( .tcount({_LEFT}) - 1, {_LEFT} )}
            .endif
        .endif
        .if !left
            ___evalOperations {_LEFT}
            left .set ___compare::regFound
        .endif
        .undefine _LEFT1
    .endif
    
    .if !left
       ___error "Unknown register to use in comparison macro."
       .exitmacro
    .endif
    
    .if left = REGA
        cmp _RIGHT
    .elseif left = REGX
        cpx _RIGHT
    .elseif left = REGY
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
;  .macro mb section

.macro ___evalOperations exp
    FLOW_CONTROL_VALUES::MB_RIGHT_SIDE_ONLY .set 1
    mb {exp}
    FLOW_CONTROL_VALUES::MB_RIGHT_SIDE_ONLY .set 0
.endmacro

.macro ___mb_findNextOp
    .local sBracketCount, bracketCount
    sBracketCount   .set 0
    bracketCount    .set 0
    .repeat .tcount({EXP}) - currentTokenNumber
        .if  (!(  xmatchToken { + } || xmatchToken { - } || xmatchToken { & } || xmatchToken { | } || xmatchToken { << } || xmatchToken { >> } || xmatchToken { ^ } )) || sBracketCount || bracketCount 
            sBracketCount   .set sBracketCount + xmatchToken { [ } - xmatchToken { ] }
            bracketCount    .set  bracketCount + xmatchToken { ( } - xmatchToken { ) }
            nextToken
        .endif
    .endrepeat
    .if bracketCount || sBracketCount
        ___error "Mismatched () or []"
    .endif
.endmacro

.macro mb withReg, exp

    .local leftSideTokenCount, rightSideToLoadTokenCount
    .local pendingLoadRightSide
    .local leftReg, rightReg
    .local simpleXYIncrement
    .local requestReg
    .local pos
    .local REGA, REGX, REGY
    REGA = 1
    REGX = 2
    REGY = 3
    pendingLoadRightSide    .set 0
    simpleXYIncrement       .set 0
    requestReg              .set 0
    rightReg                .set 0
    leftReg                 .set 0
    pos                     .set 0
    ; -1 will allow proper operation when called from ___evalOperations
    leftSideTokenCount      .set -1

    .ifblank exp
        .define EXP () withReg
    .else
        ; register to use passed in withReg:
        .define EXP () exp
        .ifnblank withReg
            .if .xmatch({withReg}, a)
                requestReg .set REGA
            .elseif .xmatch({withReg}, x)
                requestReg .set REGX
            .elseif .xmatch({withReg}, y)
                requestReg .set REGY
            .else
                ___error "Unknown register."
            .endif
        .endif
    .endif
    printTokenListDebugCA65HL {MB_:REG_: withReg | EXP_:exp}

    startTokenListEval {EXP}
    previousToken
    verifyNextToken { a x y abc :: # ( }, "Register, identifier or immediate value expected."
    nextToken

    .if !FLOW_CONTROL_VALUES::MB_RIGHT_SIDE_ONLY
        printTokenListDebugCA65HL { mb_ first token: currentToken}
        allowAllTokens
        ; is it a register? (zero is default)
        .if xmatchToken { a } || xmatchToken { x } || xmatchToken { y }
            .if xmatchToken { a }
                leftReg .set REGA
            .elseif xmatchToken { x }
                leftReg .set REGX
            .else
                leftReg .set REGY
            .endif
            leftSideTokenCount .set 1
        .else
            ; skip until = or := found
            .repeat .tcount({EXP})
                .if (!xmatchToken { = }) && (!xmatchToken { := })
                    nextToken
                .endif
            .endrepeat
            leftSideTokenCount .set currentTokenNumber
            previousToken
        .endif
        verifyNextToken { = := }, "Assignment with '=' or ':=' expected."
        nextToken; skip the =, :=
        verifyNextToken { a x y abc :: # ( }, "Register, identifier or immediate value expected."
        nextToken
    .endif

    ; is there  a register on the right?
    .if xmatchToken { a }
        rightReg .set REGA
    .elseif xmatchToken { x }
        rightReg .set REGX
    .elseif xmatchToken { y }
        rightReg .set REGY
    .else
        allowAllTokens
        ___mb_findNextOp
        rightSideToLoadTokenCount .set currentTokenNumber - leftSideTokenCount - 1
        pendingLoadRightSide .set 1
        previousToken
    .endif
    verifyNextToken { + - & | << >> ^ }, "Operation expected."
    nextToken
    allowAllTokens
    
    saveTokenListPosition
    ; check for special cases that allow constants after the operator
    .if xmatchToken { + }
        nextToken
        .if ( rightReg = REGX || rightReg = REGY ) && matchToken { 123 }
            .if rightReg = REGX
                .repeat currentToken
                    inx
                .endrepeat
            .else
                .repeat currentToken
                    iny
                .endrepeat
            .endif
            simpleXYIncrement .set 1
        .endif
    .elseif xmatchToken { - }
        nextToken
        .if ( rightReg = REGX || rightReg = REGY ) && matchToken { 123 }
            .if rightReg = REGX
                .repeat currentToken
                    dex
                .endrepeat
            .else
                .repeat currentToken
                    dey
                .endrepeat
            .endif
            simpleXYIncrement .set 1
        .endif
    .endif
    
    restoreTokenListPosition ; return to the operator position
    
    ; only allow incrementing x or y when using inx / iny
    .if simpleXYIncrement
        nextToken
        nextToken
        .if !EOT
            ___error "Unexpected trailing characters!"
        .endif
    ; check for  need to override right side to reg A:
    .elseif xmatchToken { & } || xmatchToken { | } || xmatchToken { << } || xmatchToken { >> } || xmatchToken { ^ } || xmatchToken { + } || xmatchToken { - }
        .if rightReg = REGX
            txa
        .elseif rightReg = REGY
            tya
        .endif
        rightReg .set REGA
    .endif
    
    ; no register determined?
    .if !rightReg
        .if requestReg
            rightReg .set requestReg
        .else
            rightReg .set REGA
        .endif
    .elseif requestReg
        .if rightReg <> requestReg
            ___error "Could not use requested register for operation."
        .endif
    .endif

    ; let ___compare know which reg is active:
    ___compare::regFound .set rightReg

    .if pendingLoadRightSide 
        printTokenListDebugCA65HL { MB right side to load : .mid( leftSideTokenCount + 1, rightSideToLoadTokenCount, {EXP}) }
        .if rightReg = REGA
            lda .mid( leftSideTokenCount + 1, rightSideToLoadTokenCount, {EXP})
        .elseif rightReg = REGX
            ldx .mid( leftSideTokenCount + 1, rightSideToLoadTokenCount, {EXP})
        .else
            ldy .mid( leftSideTokenCount + 1, rightSideToLoadTokenCount, {EXP})
        .endif
    .endif
    
    .repeat .tcount({EXP}) - currentTokenNumber
    .if !EOT
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { + }
            nextToken
            .if !xmatchToken { c } ; c means WITH carry
                clc
            .else
                nextToken ; skip the c
            .endif
            pos .set currentTokenNumber
            ___mb_findNextOp
            adc {.mid( pos, currentTokenNumber - pos, {EXP} )}
        .endif
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { - }
            nextToken
            .if !xmatchToken { c } ; c means WITH carry
                sec
            .else
                nextToken ; skip the c
            .endif
            pos .set currentTokenNumber
            ___mb_findNextOp
            sbc {.mid( pos, currentTokenNumber - pos, {EXP} )}
        .endif        
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { & }
            nextToken
            pos .set currentTokenNumber
            ___mb_findNextOp
            and {.mid( pos, currentTokenNumber - pos, {EXP} )}
        .endif
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { | }
            nextToken
            pos .set currentTokenNumber
            ___mb_findNextOp
            ora {.mid( pos, currentTokenNumber - pos, {EXP} )}
        .endif
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { << }
            nextToken
            .if matchToken { 123 }
                .repeat currentToken
                    asl
                .endrepeat
                nextToken
                .if !EOT
                    ___error "No addition operations allowed after shift."
                .endif
            .else
                ___error "Shift left requires constant value for number of shift operations."
            .endif
            ;___mb_findNextOp
        .endif
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { >> }
            nextToken
            .if matchToken { 123 }
                .repeat currentToken
                    lsr
                .endrepeat
                nextToken
                .if !EOT
                    ___error "No addition operations allowed after shift."
                .endif
            .else
                ___error "Shift left requires constant value for number of shift operations."
            .endif
            ;___mb_findNextOp
        .endif
        ; --------------------------------------------------------------------------------------------
        .if xmatchToken { ^ }
            nextToken
            pos .set currentTokenNumber
            ___mb_findNextOp
            eor {.mid( pos, currentTokenNumber - pos, {EXP} )}
        .endif
    .endif
    .endrepeat
    
    .if !FLOW_CONTROL_VALUES::MB_RIGHT_SIDE_ONLY
        .if !leftReg
            .if rightReg = REGA
                sta .left(leftSideTokenCount, {EXP})
            .elseif rightReg = REGX
                stx .left(leftSideTokenCount, {EXP})
            .else
                sty .left(leftSideTokenCount, {EXP})
            .endif
        .elseif rightReg = REGA
            .if leftReg = REGX
                tax
            .elseif leftReg = REGY
                tay
            .endif
        .elseif rightReg = REGY
            .if leftReg = REGA
                tya
            .elseif leftReg <> REGY
                __error "Not supported."
            .endif
        .elseif rightReg = REGX
            .if leftReg = REGA
                txa
            .elseif leftReg <> REGX
                __error "Not supported."
            .endif
        .endif
    .endif

    .undefine EXP
    endTokenListEval
.endmacro

; --------------------------------------------------------------------------------------------
; Helper macros for .macro mw (Move Word)
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
        .if !( value = ___moveWord::previousImm )
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
    ; set to a value that couldn't fit into one byte to avoid matching the first byte:
    ___moveWord::previousImm .set $0100
    .if imm
        .repeat destsize, i
            ; nothing to do for source if sourcecount is -1
            .if sourcecount <> -1 
                .if sourcecount = 0 && source <> 0
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
;   Support for moving 16 bits as address or immediate.
;   Can accept one parameter that specifies the CPU register to use for moving memory. 
;   If no register passed, will use register A
;   (The macro will adjust arguments if exp is empty).

.macro mw register, exp

    .local destSize  
    .local sourceSize 
    .local immMode
    .local castbase
    .local requestReg
    .local assignPos
    requestReg  .set 0
    destSize    .set 0  ; size of memory for destination
    sourceSize  .set 0  ; size of source data
    immMode     .set 0  ; if copy should be immediate mode
    destIsReg   .set 0  ; if the destination matched a register or pair
    sourceIsReg .set 0  ; if the source matched a register or pair
    assignPos   .set 0  ; assignment token position
    
    .ifblank exp
        .define EXP () register            ; no register
    .else
        ; register passed in 'register':
        .define EXP () exp
        .if .xmatch(register, a)
            requestReg .set REGA
        .elseif .xmatch(register, x)
            requestReg .set REGX
        .elseif .xmatch(register, y)
            requestReg .set REGY
        .else
            ___error "Unknown register."
        .endif
    .endif
    printTokenListDebugCA65HL {MW_: REG_: register | EXP_: exp}	

    ; find assignment token: (accept := or =)
    ___findToken {EXP}, =, assignPos
    .if !assignPos
        ___findToken {EXP}, :=, assignPos
    .endif
    .if !assignPos
        ___error "No assignment."
    .endif
    
    .define _DEST_      .mid(0, assignPos, {EXP})
    .define _T_SOURCE_  .mid(assignPos + 1, .tcount({EXP}) - assignPos - 1, {EXP})
    ; for now, this will always be two for 16 bit move:
    destSize .set 2
    
    ; --------------------------------------------------------------------------------------------
    ; find destination:
    ; support ax, ay, and xy pairs:
    .if .xmatch( { _DEST_ }, { ax } ) || .xmatch( { _DEST_ }, { ay } ) || .xmatch( { _DEST_ }, { xy } )     
        destIsReg .set 1
        ; _DEST_STR_ is for nicer error messages:
        .if .xmatch(_DEST_,ax) 
            .define _DEST_STR_ "register ax"
        .elseif .xmatch(_DEST_,ay) 
            .define _DEST_STR_ "register ay"
        .elseif .xmatch(_DEST_,xy) 
            .define _DEST_STR_ "register xy"
        .endif
    .else
        .define _DEST_STR_ .string(_DEST_)
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; find source:
    .if .xmatch( {.left(1, {_T_SOURCE_} ) } , {&} ) 
            sourceSize .set 2
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
            .if ( _SOURCE_ & $FF000000 ) 
                sourceSize .set 4
            .elseif ( _SOURCE_ & $FF0000 )
                sourceSize .set 3
            .elseif ( _SOURCE_ & $FF00 )
                sourceSize .set 2
            .else
                sourceSize .set 1
            .endif
        .else ; not a constant, so just match destination size. 
            sourceSize .set destSize
        .endif
    ; Check if source is using a size cast:
    .elseif .xmatch( .left( 1, {_T_SOURCE_}), {(} ) ; size cast
        .if .xmatch( .mid( 1, 1 , {_T_SOURCE_}) ,  .byte )
            castbase .set 1
        .elseif .xmatch( .mid( 1, 1 , {_T_SOURCE_}) ,  .word ) || .xmatch( .mid(1, 1 , {_T_SOURCE_}) ,  .addr )
            castbase .set 2
        .else
            .error "Size expected for cast: .byte, .word, .addr"
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
        sourceSize .set castbase
    
    .else
        ; Start here: no leading tokens to skip due to not address operator, not immediate, not cast size:
        .define _SOURCE_  _T_SOURCE_
        ; is source a register?
        ; if not check for other operators supported:
        ; < for lowbyte
        ; > for high bye
        ; otherwise check for an identifier and find its size if possible
        
        ; size of ident may be difficult to determine, so default to this for ca65 to evaluate later and copy destSize number of bytes:
        sourceSize .set destSize ; default!
        
        ; see if we can determine the size:
        ; Check if register:
        .if .xmatch(_SOURCE_,a) || .xmatch(_SOURCE_,x) || .xmatch(_SOURCE_,y)
            sourceSize .set 1
            sourceIsReg .set 1
        .elseif .xmatch(_SOURCE_,ax) || .xmatch(_SOURCE_,ay) || .xmatch(_SOURCE_,xy)
            sourceSize .set 2
            sourceIsReg .set 1
        ; Check if lowbyte or high byte operator: <, >
        .elseif .xmatch( {.left(1, {_SOURCE_} ) } , {<} ) || .xmatch( {.left(1, {_SOURCE_} ) } , {>} )
            sourceSize .set 1
            
        ; see if we can get the size of an single token ident. ca65 is funny with scoping due to one pass, not much more can easily be done.
        ; could be more strict with checking ident. sizes, but it would require calls to use explicit scoping or casting idents.
        .elseif .tcount( { _SOURCE_ } ) = 1
            .if .match( { _SOURCE_ }, 1234 ) ; number: load from address
                .if _SOURCE_ >= 0 && _SOURCE_ <= $FFFF
                    sourceSize .set destSize
                .else
                    .error "Address out of range."
                .endif
            .elseif .defined ( _SOURCE_ )
                sourceSize .set .sizeof(  _SOURCE_  )
            .endif
        .endif
    .endif
    
    ; error if source is bigger than dest.
    .if sourceSize > destSize
        .error .sprintf( "Overflow for '%s'. Destination size: %d bytes. Source size: %d bytes.", _DEST_STR_, destSize, sourceSize)
        .fatal "STOP"
    .endif
    
    ; check for register moves not supported:
    .if destIsReg && sourceIsReg
        ___error .sprintf("Move for register %s: Not supported with register source.", .string(_DEST_))
    .endif
    
    ; if source is a CPU reg. and destination is not a register: (assume it is memory)
    .if (.xmatch( _SOURCE_, a) || .xmatch( _SOURCE_, x) || .xmatch( _SOURCE_, y) ) && (!destIsReg)
        .if .xmatch( _SOURCE_, a) ; if the source is reg.a:
            sta _DEST_
        .elseif .xmatch( _SOURCE_, x) ; if the source is reg.x:
            stx _DEST_
        .elseif .xmatch( _SOURCE_, y) ; if the source is reg.y
            sty _DEST_
        .endif   
        ; check if _DEST_ memory size is bigger than one byte and warn
        .if destSize > 1 && ::CA65HL_WARNING_LEVEL > 0
            .warning "MW: Register source size is 1 byte: High byte not set."
        .endif
    .elseif (.xmatch( _SOURCE_, ax) || .xmatch( _SOURCE_, ay) || .xmatch( _SOURCE_, xy) ) && (!destIsReg)
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
        ; check which register is supposed to be used for moving memory
        .if requestReg = REGY
            ___moveMem_ca65hl y, _SOURCE_, _DEST_, destSize, sourceSize, immMode
        .elseif requestReg = REGX
            ___moveMem_ca65hl x, _SOURCE_, _DEST_, destSize, sourceSize, immMode
        .else
            ___moveMem_ca65hl a, _SOURCE_, _DEST_, destSize, sourceSize, immMode
        .endif       
    .elseif .xmatch( _DEST_, ax) ; if the dest. is reg.ax:
        .if immMode
            lda # <_SOURCE_
            ldx # >_SOURCE_
        .else
            lda _SOURCE_
            ldx _SOURCE_ + 1
        .endif   
    .elseif .xmatch( _DEST_, ay) ; if the dest. is reg.ay:
        .if immMode
            lda # <_SOURCE_
            ldy # >_SOURCE_
        .else
            lda _SOURCE_
            ldy _SOURCE_ + 1
        .endif   
    .elseif .xmatch( _DEST_, xy) ; if the dest. is reg.xy:
        .if immMode
            ldx # <_SOURCE_
            ldy # >_SOURCE_
        .else
            ldx _SOURCE_
            ldy _SOURCE_ + 1
        .endif   
    .endif    
    
    .undefine EXP
    .undefine _DEST_STR_
    .undefine _SOURCE_
    .undefine _DEST_
    .undefine _T_SOURCE_
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___evaluateStatementList statement
;
; Parameters:
;    register - Optional - register to use 
;    statement - macro or instruction or other
;
; Recursively evaluate statements: One or more instructions or macros as well as supported 
; comparisons and simple math evaluations.

.macro ___evaluateStatementList statement

    ; define and then work with one parameter at a time
    .local colonPos
    .local isInstruction, isMacro
    colonPos .set 0
    ___findToken {statement}, :, colonPos
    .if colonPos
        .define S() .mid(0, colonPos, {statement})
    .else
        .define S() statement
    .endif
    
    printTokenListDebugCA65HL {Statement: S}
    
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
                ___evalOperations {S}
            .endif
        .else 
            ; not a register, first save if it is an instruction or macro
            .if ::CA65HL_USE_CUSTOM_SYNTAX
                .feature ubiquitous_idents -
                isInstruction = .ismnemonic (.left (1,{S})) || .xmatch( .left (1,{S}), adc) ; adc bug, fixed in recent builds of ca65
                isMacro       = .definedmacro ( .left(1,{S})) && (!isInstruction)
                .feature ubiquitous_idents +
            .else
                isInstruction = .ismnemonic (.left (1,{S})) || .xmatch( .left (1,{S}), adc)
                isMacro       = .definedmacro ( .left(1,{S})) && (!isInstruction)
            .endif
            ; is it a macro?
            .if isMacro
                printTokenListDebugCA65HL {Macro: S}
                .if .tcount({S}) = 1
                    .left (1,{S})
                .else
                    .left (1,{S}) { .mid (1, .tcount({S}) - 1, {S} ) }
                .endif
            .else
                ; not a macro, check for comparison before instructions, as instructions could be mixed with a compare
                ___findCompareOperator {S}
                .if ___compare::found
                    ___doCompare {S}
                .elseif isInstruction
                    .left (1,{S}) .mid (1, .tcount({S}) - 1, {S} )
                .else
                    ; not a macro, not a compare, not an instruction, could be an operation
                    ___evalOperations {S}
                .endif
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
; Function: ___evaluateBranch statement
;
; Parameters:
;    statement - macro or instruction or other
;
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
    ; error check: if inlineBranchSetPos is not 0, this indicates '==' or '!=' was found: At this point the branch should be defined.
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
; Read tokens from options, compare each to possible matches in optionList
; .. and set bits in order of any matches in optionList
; Wildcard (*) will match anything: for checking for certain values, but allowing any

.macro readOptions optionBits, options, optionList, errorMessage
    .local matchFound
    optionBits .set 0
    .repeat .tcount( {options} ), c
        .define OPTION () .mid(c, 1, {options})
        .ifnblank .mid(c, 1, {options})
            matchFound .set 0
            .repeat .tcount( {optionList} ), i
                .if .xmatch( {OPTION}, { .mid( i, 1, {optionList} ) } ) || .xmatch( {*}, { .mid( i, 1, {optionList} ) } )
                    matchFound .set 1
                    optionBits .set optionBits | ( 1 << i )
                .endif
            .endrepeat
            .if !matchFound
                ___error .sprintf("Unknown option. %s", errorMessage)
            .endif
        .endif
        .undefine OPTION
    .endrepeat
.endmacro

; --------------------------------------------------------------------------------------------
; Section: IF ELSEIF ELSE ENDIF
; --------------------------------------------------------------------------------------------

; --------------------------------------------------------------------------------------------
; Function: ___xmatchSpecial
;
; Macro for helping to evaluate || and && tokens with a possible negate applied to a bracket set.
; Only called from .macro 'if'. Will only ever be called with '&&' or '||' for parameter 'token'

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

; The core of functionality for this file. Evaluates a condition and generates branches. 
; Calls other macros to process statements and output code/determine what to branch on.
; This macro will create branch instructions with regards to the logic in <condition> and branch
; to a provided label when used with 'goto'. If no 'goto' any code included on the same line
; after the condition in brackets () will be executed (if the condition passes at runtime).
; If nothing after the condition, branches to the next ENDIF (or ELSE, or ELSEIF) on an 
; inverted condition.


.macro if condition, opt0, opt1
    
    printTokenListDebugCA65HL {Branch_Statement: condition}
    
    .local firstBranchToLongJump        ;  label: for verifying a long jump is needed: address from the first branch that will use a long jump
    .local negateBracketSet             ;  flag: if a set of terms in brackets to be negated
    .local negateNext                   ;  flag: if single branch term to be negated              
    .local bracketLevel                 ;  level of brackets we are in, lowest is 1, 0 is outside brackets
    .local branchLabelCounter           ;  count of how many branch labels to additional OR/AND conditions needed
    .local conditionTokenCount          ;  save token count for condition only (could be goto/break statement after)
    .local gotoUserLabel                ;  flag: if 'goto' found, branch to label passed in <condition>
    .local foundTokPosForBranch         ;  save token position of valid && or || tokens when performing look-ahead evaluating correct branch
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
    .local backwardBranchOK             ;  flag: true if long jump is not needed for this branch. False means long branch needed or unknown if needed for forward branch
    .local stackCounter                 ;  hold popped stack count for continue and break
    .local inLineCode                   ;  flag: Set if IF is followed by code to execute
    .local tokenPosSavedOffset

    negateBracketSet        .set FLOW_CONTROL_VALUES::NEGATE_CONDITION ; when set this will negate the entire condition
    negateNext              .set 0
    branchLabelCounter      .set 0
    gotoUserLabel           .set 0
    chainedFlag             .set 0
    inLineCode              .set 0
    useLongJump             .set -1 ; invalid, needs to be set below
    
    ; these are initialized elsewhere before use:
    ; foundTokPosForBranch    .set 0
    ; foundOR_AND             .set 0
    ; scanAheadBracketLevel   .set 0
    ; lowestBracketLevel      .set 0
    ; scanAheadNegateBrackets .set 0
    ; statementStartPos       .set 0
    ; statementTokenCount     .set 0
    ; foundAND                .set 0
    ; foundOR                 .set 0
    ; backwardBranchOK        .set 0  
    ; conditionTokenCount     .set 0
    ; bracketLevel            .set 0
    ; stackCounter            .set 0

    .if FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE
        ___error "Cannot use 'if' statement from within conditional expression."
    .endif
    FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE .set 1
    FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT .set -1       ; -1 means invalid. only valid after an ENDIF macro
    
    ; --------------------------------------------------------------------------------------------
    ; evaluate options in any order
    .local ifOptionBits
    .if FLOW_CONTROL_VALUES::INTERNAL_CALL
        readOptions ifOptionBits, { opt0 opt1 }, { long short }, "Valid options are one of 'long', 'short'."
    .else
        readOptions ifOptionBits, { opt0 opt1 }, { long short chain }, "Valid options are 'chain', or one of 'long', 'short'."
        chainedFlag .set ifOptionBits & 4
    .endif
    .if ifOptionBits & ( 1 | 2 ) = 1 | 2
        ___error "Conflicting option for long/short branch."
    .elseif ifOptionBits & 1
        useLongJump .set 1
    .elseif ifOptionBits & 2
        useLongJump .set 0
    .endif
    
    ; array for label locations: (uses global to reuse ident)
    .define tokenPositionForBranchLabel(c)  ::.ident(.sprintf("POS_FOR_BRANCH_%02X", c))
    
    ; define for exiting IF condition (branch to this label when a condition fails)
    .define exitBranchEvaluation .ident(.sprintf("IF_STATEMENT_%04X_EXIT_BRANCH_EVALUATION", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT ))
    
    ; label for long jmp
    .define longJumpLabel .ident(.sprintf("IF_STATEMENT_%04X_LONG_JMP", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT))
    
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
    ; If there are tokens after the condition:
    ;   - Find if there is a 'goto', 'break' or 'continue' and set the successful condition to branch to the label.
    ;   - or execute whatever is after the condition as code via ___evaluateStatementList, branch over on inverted condition
    ; If no tokens after the condition, invert the condition and branch to the ENDIF label on successful (inverted) condition.
    ; destinationLabel is the label to branch to if the (inverted) condition is 'true'

    .if conditionTokenCount < .tcount({condition})
        .if .xmatch( .mid(conditionTokenCount, 1, {condition}), goto ) ; goto
            gotoUserLabel .set 1
            ; capture everything after the 'goto'
            .define destinationLabel .mid(conditionTokenCount + 1, .tcount({condition}) - conditionTokenCount - 1, {condition})
        .elseif .xmatch( .mid(conditionTokenCount, 1, {condition}), break ) ; break
            ___verifyBreak_Continue
            ___peekLoopType
            stackPeek .sprintf("%s_STATEMENT_STACK", ___loopType), stackCounter
            .define destinationLabel .ident(.sprintf( "%s_BREAK_%04X", ___loopType, stackCounter))
        .elseif .xmatch( .mid(conditionTokenCount, 1, {condition}), continue ) ; continue
            ___verifyBreak_Continue cont
            ___peekLoopType
            stackPeek .sprintf("%s_STATEMENT_STACK", ___loopType), stackCounter
            .if .xmatch ( .ident(___loopType), FOR)
                .define destinationLabel .ident(.sprintf( "FOR_%04X_CONTINUE", stackCounter))
            .else
                .define destinationLabel .ident(.sprintf("%s_%04X_START", ___loopType, stackCounter))   
            .endif
        .else
            ; No matches, but there is something after the end of the condition:
            .if FLOW_CONTROL_VALUES::INTERNAL_CALL
                ___error "Error in expression."
            .else
                ; allow and execute code following IF condition on the same line (no ENDIF)
                inLineCode .set 1
                negateBracketSet .set !negateBracketSet
                .define destinationLabel .ident(.sprintf( "IF_STATEMENT_%04X_ENDIF", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT ))
            .endif
        .endif
        setTokenCount conditionTokenCount ; set max tokens for EOT to exclude anything after the condition
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
            .define destinationLabel .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", ENCLOSING_IF_STATEMENT_COUNT ))
        .else
            .define destinationLabel .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT ))
        .endif
    .endif
    
    ; --------------------------------------------------------------------------------------------
    ; Warn of unneeded long or short setting passed to the macro
    .if useLongJump <> -1
        .ifdef destinationLabel
            deprecateLongBranch
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
    ; Main loop: evaluate branches and AND OR conditions. Loop over all tokens, exclude tokens
    ; after the condition. More than one token will be consumed in the 5th case below, 
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
                        ___emitLabel .sprintf( "IF_STATEMENT_%04X_BRANCH_TO_TOKEN_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, currentTokenNumber )
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
            ; exit tokeneval, save offset so other macros can use tokeneval
            endTokenListEval tokenPosSavedOffset
            ; evaluate the statement(s) and determine the branch
            ___evaluateBranch {.mid(statementStartPos, statementTokenCount, {condition})}
            ; restore tokeneval
            startTokenListEval {condition}, tokenPosSavedOffset
            setTokenCount conditionTokenCount

            .if negateNext ^ negateBracketSet
                ___invertBranchCondition
            .endif
            negateNext .set 0
            
            ; save token position and scan ahead in the token list temporarily:
            allowAllTokens              
            saveTokenListPosition
            saveStackPointer "_IF_NEGATE_STACK_"
            foundTokPosForBranch    .set 0
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
                .if (!EOT) && (!foundTokPosForBranch)
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
                            foundTokPosForBranch .set currentTokenNumber
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
                    .if (!EOT) && (!foundTokPosForBranch)
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
                                foundTokPosForBranch .set currentTokenNumber
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
            backwardBranchOK .set .def(destinationLabel) && ( * + 2 - destinationLabel <= 128 )
            .if foundTokPosForBranch
                ; branch to next appropriate branch following an '&&' or '||'
                .define branchToLabel .ident(.sprintf( "IF_STATEMENT_%04X_BRANCH_TO_TOKEN_%02X", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT, foundTokPosForBranch))
                tokenPositionForBranchLabel{branchLabelCounter} .set foundTokPosForBranch
                branchLabelCounter .set branchLabelCounter + 1    
            .elseif useLongJump && (!backwardBranchOK) ; !backwardBranchOK: long jump needed, or unknown if needed
                .ifndef firstBranchToLongJump
                    firstBranchToLongJump = * + 2 ; address for end of next branch
                .endif
                .if foundAND
                    ; branch to failed on inverted condition
                    .define branchToLabel exitBranchEvaluation 
                .elseif foundOR
                    .define branchToLabel longJumpLabel
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

    endTokenListEval ; clear token evaluation, no more tokens to read    
    
    ; when long jump active, JMP to destinationLabel
    .ifdef firstBranchToLongJump
        .ifref longJumpLabel
            ___emitLabel .string(longJumpLabel)
        .endif    
        jmp destinationLabel
        .if FLOW_CONTROL_VALUES::LONG_JUMP_WARNINGS
            .ifndef destinationLabel
                .assert destinationLabel - firstBranchToLongJump - 3 > 127, warning, "Branch can be reached without a long branch. See: 'setLongBranch -'"
            .endif    
        .endif
    .endif
    
    ; Branch here when a condition fails, which also is the start of a code block ..
    ; .. for an IF..ENDIF since it branches to ENDIF on an inverted condition.
    .ifref exitBranchEvaluation
        ___emitLabel .string(exitBranchEvaluation)
    .endif

    .if inLineCode
        ; capture everything after the condition
        ___evaluateStatementList { .mid(conditionTokenCount, .tcount({condition}) - conditionTokenCount, {condition}) }
        ___emitLabel .string(destinationLabel)
    .elseif !gotoUserLabel
        ; branch to ENDIF
        stackPush "IF_STATEMENT_STACK", FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT
    .endif
    
    FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::IF_STATEMENT_COUNT + 1    ; increase if statement count always
    FLOW_CONTROL_VALUES::IF_STATEMENT_ACTIVE .set 0

    .undefine destinationLabel
    .undefine tokenPositionForBranchLabel
    .undefine exitBranchEvaluation
    .undefine longJumpLabel
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___verifyChain
;
; Parameters:
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
            .assert .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF", FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT )) = *, error, "Invalid chain option used." ; address must match last ENDIF
        .elseif ::CA65HL_WARNING_LEVEL > 0
            .assert .ident( .sprintf( "IF_STATEMENT_%04X_ENDIF", FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT )) <> *, warning, "To optimize branch generation, apply chain to previous IF statement. eg: if (condition) , chain"
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
    .ifdef ::_CUSTOM_SYNTAX_H        
        .if .xmatch( knownFlagStatus, jmp )
            .if CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER > 0
                .assert ::.ident( .sprintf( "LAST_JMP_INSTRUCTION_END_ADDRESS_%04X", CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER - 1)) = *, warning, "No JMP immediately before ELSE/ELSEIF, possibly bad 'jmp' option."
            .else
                .warning "No JMP immediately before ELSE/ELSEIF, possibly bad 'jmp' option."
            .endif
        .else
            .if ( CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER > 0 ) && ( ::CA65HL_WARNING_LEVEL > 0 )
                .assert ::.ident( .sprintf( "LAST_JMP_INSTRUCTION_END_ADDRESS_%04X", CUSTOM_SYNTAX::JMP_INSTRUCTION_COUNTER - 1)) <> *, warning, "JMP before ELSE/ELSEIF. Suggested: use 'jmp' option. eg: elseif (condition) , jmp"
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
;   options   - In any order, 'long', 'short', 'jmp', or flag status. If a flag is known to be in a 
;               state when the elseif is encountered, branch to the end if using this flag as a branch, 
;               using the syntax for <setBranch>.
;               If 'long' or 'short', force long or short branch to the end of the code block.
;               Use 'jmp' to suppress the output of a jmp to the endif.
;
;   See Also:
;   <setBranch>, <endif>, <if>, <else>

.macro elseif condition, opt0, opt1

    .local IF_STATEMENT_COUNT
    .local optionBits
    
    ; allow valid options in any order
    ; opt0: will always be a flag setting or 'jmp', opt1: will always be 'long' or 'short'
    ; check for 'long' or 'short' for opt0
    readOptions optionBits, { opt0 }, { long short * }
    .if optionBits & ( 1 | 2 )  ; if match 'long' or 'short'
        ; make sure opt1 is not also 'long' or 'short'
        readOptions optionBits, { opt1 }, { long short * }
        .if optionBits & ( 1 | 2 ) 
            ___error "Conflicting option for long/short branch."
        .endif
        ; call macro again, with correct option order and terminate this call
        elseif condition, opt1, opt0
        .exitmacro
    .endif
    
    stackPeek "IF_STATEMENT_STACK", IF_STATEMENT_COUNT ; just look, don't touch
    .if IF_STATEMENT_COUNT < 0 
        ___error "'elseif' without 'if'"
    .endif
    
    .ifdef .ident( .sprintf( "_IF_STATEMENT_ELSE_%04X_DEFINED", IF_STATEMENT_COUNT ))
        ___error "Not allowed: 'elseif' after 'else'."
    .endif
    
    ; verify any chain requests, also check if chain would be valid
    ___verifyChain IF_STATEMENT_COUNT
    
    ; check for JMP request, also check if jmp would be valid
    ___checkForTailJMP opt0
    
    ; Branch on user's known flag, or JMP to endif
    .ifnblank opt0
        .if !.xmatch(opt0, jmp)
            setBranch opt0
            ___Branch branchFlag, branchCondition, .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
            ___clearBranchSet
        .else
            .refto .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
        .endif
    .else
        jmp .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
    .endif
    
    ; elseif counter for this if statement:
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )
    ; if not defined, it means there are no previous ELSEIF, so create a label for the originating IF 
    .ifndef ELSE_IF_COUNT
        ; set the endif label for the original IF:
        ___emitLabel .sprintf( "IF_STATEMENT_%04X_ENDIF", IF_STATEMENT_COUNT )
        ELSE_IF_COUNT .set -1 ; start at -1, will be incremented to 0
    .else
        ; this isn't the first ELSEIF: 
        ___emitLabel .sprintf( "IF_STATEMENT_%04X_ELSEIF_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )
    .endif
    
    ELSE_IF_COUNT .set ELSE_IF_COUNT + 1
    ; negate statement to GOTO the next ELSEIF/ELSE/ENDIF on failed condition 
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if { condition goto .ident( .sprintf( "IF_STATEMENT_%04X_ELSEIF_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )) }, opt1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    
    .undefine ELSE_IF_COUNT

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
    stackPeek "IF_STATEMENT_STACK", IF_STATEMENT_COUNT
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
        .if !.xmatch(knownFlagStatus, jmp)
            setBranch knownFlagStatus
            ___Branch branchFlag, branchCondition, .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
            ___clearBranchSet
        .else
            .refto .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
        .endif
    .else
        jmp .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
    .endif
    
    ; elseif counter for this if statement:
    .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT) )    
    ; if ELSE_IF_COUNT is defined, means there are one or more ELSEIF, so create a label for the last one,
    ; otherwise, create a label for the originating IF
    .ifdef ELSE_IF_COUNT
        ___emitLabel .sprintf( "IF_STATEMENT_%04X_ELSEIF_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )
        ELSE_IF_COUNT .set -1 ; signal it is not needed for the endif macro
    .else
        ___emitLabel .sprintf( "IF_STATEMENT_%04X_ENDIF", IF_STATEMENT_COUNT )
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
    .ifref .ident( .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT ))
        ___emitLabel .sprintf( "IF_STATEMENT_%04X_ELSE_ENDIF", IF_STATEMENT_COUNT )
        ; if there was an ELSEIF and last ELSEIF label not handled by an ELSE:
        .define ELSE_IF_COUNT .ident( .sprintf("IF_STATEMENT_%04X_ELSEIF_COUNT", IF_STATEMENT_COUNT))
        .ifdef ELSE_IF_COUNT
            .if ELSE_IF_COUNT <> -1 ; -1 means ELSE handled the last ELSEIF label
                ___emitLabel .sprintf( "IF_STATEMENT_%04X_ELSEIF_%04X", IF_STATEMENT_COUNT, ELSE_IF_COUNT )
            .endif
        .endif
        .undefine ELSE_IF_COUNT 
    .else
        ___emitLabel .sprintf( "IF_STATEMENT_%04X_ENDIF", IF_STATEMENT_COUNT )
    .endif
    FLOW_CONTROL_VALUES::LAST_ENDIF_COUNT .set IF_STATEMENT_COUNT
.endmacro

; --------------------------------------------------------------------------------------------
; STRUCTURES/LOOPS
; --------------------------------------------------------------------------------------------

; --------------------------------------------------------------------------------------------
; Function: ___pushLoopType
;
; Keep track of the current type of loop for generating breaks or continue
;
; Parameters: Type of loop that was started
; Valid types:
;   DO_WHILE
;   WHILE_DO_ENDWHILE
;   FOR
;   SWITCH 

.macro ___pushLoopType loopType
    pushTokenList "LOOP_TYPE", loopType
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___popLoopType
;
; Retrieve the current loop type

.macro ___popLoopType 
    popTokenList "LOOP_TYPE"
    .if FLOW_CONTROL_VALUES::LOOP_TYPE_DEFINED
        .undefine ___loopType
    .endif
    .define ___loopType () poppedTokenList
    FLOW_CONTROL_VALUES::LOOP_TYPE_DEFINED .set 1
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___peekLoopType
;
; Retrieve the current loop type without changing the stack
;

.macro ___peekLoopType 
    peekTokenList "LOOP_TYPE"
    .if FLOW_CONTROL_VALUES::LOOP_TYPE_DEFINED
        .undefine ___loopType
    .endif
    .define ___loopType () poppedTokenList
    FLOW_CONTROL_VALUES::LOOP_TYPE_DEFINED .set 1
.endmacro
    
; --------------------------------------------------------------------------------------------
; Function: ___verifyBreak_Continue
;
; Verify break or continue is being invoked from within structure that allows it.
;
; Parameters: continue
;
; If continue is non-blank, check for valid continue, otherwise check for valid break

.macro ___verifyBreak_Continue cont
    ___peekLoopType
    .ifnblank cont
        ; if no structure, or most inner structure is a switch    
        .if .xmatch(___loopType, null)
            ___error "Invalid 'continue'."
        .elseif .xmatch ( .ident(___loopType), SWITCH )
            ___error "Invalid 'continue'."
        .endif
    .else
        ; If not in a structure
        .if .xmatch(___loopType, null)
            ___error "Invalid 'break'."
        .endif
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: ___generateBreakLabel count
;
; Invoked at the end of a loop macro to check if any labels
; need to be created for a break command from inside that loop.
; Loop type should be popped into __loopType with ___popLoopType before this is called.
;
; Parameters:
;   count - stack count for label

.macro ___generateBreakLabel count
    .ifref .ident(.sprintf( "%s_BREAK_%04X",___loopType, count))
        ___emitLabel .sprintf( "%s_BREAK_%04X",___loopType, count)
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

    ; this will check for a valid inner loop
    ___verifyBreak_Continue 
    ; peek at stack: (____verifyBreak_Continue will set ___loopType )
    ;___peekLoopType ___loopType
    
    .local stackCounter
    stackPeek .sprintf("%s_STATEMENT_STACK", ___loopType), stackCounter

    .ifnblank knownFlagStatus
        setBranch knownFlagStatus
        ___Branch branchFlag, branchCondition, .ident(.sprintf( "%s_BREAK_%04X", ___loopType, stackCounter))
        ___clearBranchSet
    .else
        jmp .ident(.sprintf( "%s_BREAK_%04X", ___loopType, stackCounter))
    .endif

.endmacro

; --------------------------------------------------------------------------------------------
; Function: continue
;
; Branch or jump to the start of the current loop
;
; Parameters:
;   knownFlagStatus - Optional - if a flag is known to be in a state when the else
;                     is encountered, branch to the end if using this flag as a branch 
;                     always, using the syntax for <setBranch>

.macro continue knownFlagStatus

    ; this will check for a valid inner loop, will not allow switch to use continue:
    ___verifyBreak_Continue cont
    ; peek at stack: (____verifyBreak_Continue will set ___loopType )
    ;___peekLoopType ___loopType

    .local stackCounter
    stackPeek .sprintf("%s_STATEMENT_STACK", ___loopType), stackCounter

    ; if FOR loop, continue will branch to the end of the loop ..
    ; .. to do the increment and condition
    .if .xmatch ( .ident(___loopType), FOR)
        .define continueLabel .ident(.sprintf( "FOR_%04X_CONTINUE", stackCounter))
    .else
        .define continueLabel .ident(.sprintf("%s_%04X_START", ___loopType, stackCounter))
    .endif

    .ifnblank knownFlagStatus
        setBranch knownFlagStatus
        ___Branch branchFlag, branchCondition, continueLabel
        ___clearBranchSet
    .else
        jmp continueLabel
    .endif

    .undefine continueLabel
.endmacro

; --------------------------------------------------------------------------------------------
; Function: goto
;
; Branch or jump to a given label
;
; Parameters:
;   opt0 - Optional - if a flag is known to be in a state when the else
;          is encountered, branch using this flag as a branch 
;          always, using the syntax for <setBranch>
;   opt1 - Can be 'long' or 'short' (no quotes) where long indicates to use
;          a JMP instruction
;
;  If a known flag value is passed the macro will always use the correct
;  long or short branch if the label has already been created.
;  If the label is yet to be defined, it will look for long and use JMP if indicated.
;  If the label can be reached without a long branch the assert will issue a warning.
;  If no flag value is passed it will always be a JMP
;  Options can be in any order.

.macro goto label, opt0, opt1 ; ( knownFlagStatus, branchType )

    .local useLongJump
    .local optionBits

    ; allow valid options in any order
    ; opt0: will always be a flag setting, opt1: will always be 'long' or 'short'
    ; check for 'long' or 'short' for opt0
    readOptions optionBits, { opt0 }, { long short * }
    .if optionBits & ( 1 | 2 )  ; if match 'long' or 'short'
        ; make sure opt1 is not also 'long' or 'short'
        readOptions optionBits, { opt1 }, { long short * }
        .if optionBits & ( 1 | 2 ) 
            ___error "Conflicting option for long/short branch."
        .endif
        ; call macro again, with correct option order and terminate this call
        goto label, opt1, opt0
        .exitmacro
    .endif
    ; syntax check:
    readOptions optionBits, { opt1 }, { long short }, "Option should be long, or short"

    ; figure out if we need to JMP or branch:
    .ifdef label
        useLongJump = (!(* + 2 - label <= 128)) || .blank(opt0)
    .else
        useLongJump = .xmatch( opt1, long ) || .blank(opt0)
    .endif

    .if useLongJump
        .if (!.blank(opt0)) && (::CA65HL_WARNING_LEVEL > 1)
            .assert label - 3 - * > 127, warning, "Branch can be reached without long branch."
        .endif
        jmp label
    .else
        setBranch opt0
        ___Branch branchFlag, branchCondition, label
        ___clearBranchSet
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; Function: do
;
; Start a do..while loop.

.macro do
    stackPush "DO_WHILE_STATEMENT_STACK", FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT
    ___emitLabel .sprintf( "DO_WHILE_%04X_START", FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT)
    FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::DO_WHILE_STATEMENT_COUNT + 1
    ___pushLoopType "DO_WHILE"
.endmacro

; --------------------------------------------------------------------------------------------
; Function: repeat
;
; Start a repeat..until loop.
;
; Parameters: none

.macro repeat
    do
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

.macro while condition, branchType
    .if .xmatch(.right(1, {condition}), do) ; if match 'do' at end this is a while..do..endwhile statement
        while_do {.mid(0, .tcount({condition}) - 1, {condition}) }
    .else
        .local DO_WHILE_STATEMENT_COUNT
        stackPop "DO_WHILE_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
        .if DO_WHILE_STATEMENT_COUNT < 0 
            ___error "'while' without 'do'"
        .endif
        FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
        if { condition goto .ident( .sprintf( "DO_WHILE_%04X_START", DO_WHILE_STATEMENT_COUNT)) }, branchType
        FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
        ___popLoopType
        ___generateBreakLabel DO_WHILE_STATEMENT_COUNT
    .endif
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

.macro until condition, branchType
    .local DO_WHILE_STATEMENT_COUNT
    stackPop "DO_WHILE_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
    .if DO_WHILE_STATEMENT_COUNT < 0 
        ___error "'until' without 'repeat'"
    .endif
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if { condition goto .ident( .sprintf( "DO_WHILE_%04X_START", DO_WHILE_STATEMENT_COUNT)) }, branchType
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 0
    ___popLoopType
    ___generateBreakLabel DO_WHILE_STATEMENT_COUNT
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

.macro forever branchType
    .local DO_WHILE_STATEMENT_COUNT
    stackPop "DO_WHILE_STATEMENT_STACK", DO_WHILE_STATEMENT_COUNT
    .if DO_WHILE_STATEMENT_COUNT < 0 
        ___error "Missing 'repeat' or 'do'"
    .endif
    jmp .ident( .sprintf( "DO_WHILE_%04X_START", DO_WHILE_STATEMENT_COUNT))
    ___popLoopType
    ___generateBreakLabel DO_WHILE_STATEMENT_COUNT
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

.macro while_do condition, branchType
    ___pushLoopType "WHILE_DO_ENDWHILE"
    stackPush "WHILE_DO_ENDWHILE_STATEMENT_STACK", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT                      ; save counter
    ___emitLabel .sprintf( "WHILE_DO_ENDWHILE_%04X_START", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT)
    FLOW_CONTROL_VALUES::NEGATE_CONDITION .set 1
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if {condition goto .ident( .sprintf( "WHILE_DO_ENDWHILE_%04X_EXIT", FLOW_CONTROL_VALUES::WHILE_DO_ENDWHILE_STATEMENT_COUNT))}, branchType
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
    stackPop "WHILE_DO_ENDWHILE_STATEMENT_STACK", WHILE_DO_ENDWHILE_STATEMENT_COUNT
    .if WHILE_DO_ENDWHILE_STATEMENT_COUNT < 0
        ___error "'endwhile' without 'while-do'"
    .endif
     ; branch or JMP to start of loop
    .ifnblank knownFlagStatus
        setBranch knownFlagStatus
        ___Branch branchFlag, branchCondition, .ident( .sprintf( "WHILE_DO_ENDWHILE_%04X_START", WHILE_DO_ENDWHILE_STATEMENT_COUNT))
        ___clearBranchSet
    .else
        jmp .ident( .sprintf( "WHILE_DO_ENDWHILE_%04X_START", WHILE_DO_ENDWHILE_STATEMENT_COUNT))
    .endif
    ___emitLabel .sprintf( "WHILE_DO_ENDWHILE_%04X_EXIT", WHILE_DO_ENDWHILE_STATEMENT_COUNT)
    ___popLoopType
    ___generateBreakLabel WHILE_DO_ENDWHILE_STATEMENT_COUNT
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
;   Code for <init> will always be executed. If 'strict' is passed for <strict> the 
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
    
    ; check for 'strict'
    .ifnblank op1
        .if !.xmatch(op1, strict)
            ___error "Expected: 'strict': Test condition before first loop is executed."
        .endif
        jmp .ident( .sprintf( "FOR_%04X_JMP_TO_CONDITION", FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNT))
    .endif
    ___emitLabel .sprintf( "FOR_%04X_START", FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNT)
    
    stackPush "FOR_STATEMENT_STACK", FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNT
    FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::FOR_STATEMENT_COUNT + 1
    
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

.macro next branchType
    .local FOR_STATEMENT_COUNT
    
    stackPop "FOR_STATEMENT_STACK", FOR_STATEMENT_COUNT
    .if FOR_STATEMENT_COUNT < 0
        ___error "'next' without 'for'."
    .endif
    
    ; branch here if there was a 'continue' command
    .ifref .ident(.sprintf( "FOR_%04X_CONTINUE", FOR_STATEMENT_COUNT))
        ___emitLabel .sprintf( "FOR_%04X_CONTINUE", FOR_STATEMENT_COUNT)
    .endif

    ; code to execute before next loop:
    popTokenList "FOR_STATEMENT_INCREMENT"
    ___evaluateStatementList {poppedTokenList}
    
    ; branch here if 'strict' requested
    .ifref .ident( .sprintf( "FOR_%04X_JMP_TO_CONDITION", FOR_STATEMENT_COUNT))
        ___emitLabel .sprintf( "FOR_%04X_JMP_TO_CONDITION", FOR_STATEMENT_COUNT)
    .endif

    popTokenList "FOR_STATEMENT_CONDITION"
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 1
    if { poppedTokenList goto .ident( .sprintf( "FOR_%04X_START", FOR_STATEMENT_COUNT)) }, branchType
    FLOW_CONTROL_VALUES::INTERNAL_CALL .set 0
    ___popLoopType
    ___generateBreakLabel FOR_STATEMENT_COUNT
.endmacro

; --------------------------------------------------------------------------------------------; --------------------------------------------------------------------------------------------
; Function switch_SetDataSeg
;
;   Parameters -
;       string - string representing a valid segment for the linker.
;
; Set the segment for the table data for the 'switch' statement.

.macro switch_SetDataSeg string
    .if FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG_DEFINED
        .undefine SWITCH_STATEMENT_DATA_SEG_STRING
    .endif
    .define SWITCH_STATEMENT_DATA_SEG_STRING string
    FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG_DEFINED .set 1
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
; if switch_SetDataSeg is used first, the data table will be placed in the defined segment and will allow the macro to not have to include a JMP command to skip 
; the data tables.

.macro switch reg, mode

    ___pushLoopType "SWITCH"
    .local gotoMode
    gotoMode .set 0
    
    .ifnblank mode
        .if .xmatch( {mode}, goto )
            gotoMode .set 1
        .else
            ___error "Mode should be 'goto' for basic jump table, or omitted for any case values."
        .endif
    .endif
    
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
        .define SWITCH_INFO_REG "Register X loaded/changed to index data for switch."
        .define INDEX_REG_FOR_TABLE x
    .else ; no compare, just branch on lookup value in reg. x or y
        .if .xmatch( reg, x )
            .define INDEX_REG_FOR_TABLE x
            .define SWITCH_INFO_REG ""
        .elseif .xmatch( reg, a )
            tax
            .define INDEX_REG_FOR_TABLE x
            .define SWITCH_INFO_REG "Register X loaded/changed to index data for switch."
        .elseif .xmatch( reg, y )
            .define INDEX_REG_FOR_TABLE y
            .define SWITCH_INFO_REG ""
        .else 
            ldx reg
            .define INDEX_REG_FOR_TABLE x
            .define SWITCH_INFO_REG "Register X loaded/changed to index data for switch."
        .endif
    .endif
    
    .if ::CA65HL_WARNING_LEVEL > 1
        .warning .sprintf( "INFO: Switch: Register A value changed for switch. %s", SWITCH_INFO_REG)
    .endif
    
    .if !gotoMode
        .local loop, found
        ldx #( .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_TOTAL_COUNT", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT)) - 1 )
        loop:
            cmp .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_CONSTANTS", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT)), x
            beq found
            dex
        bpl loop
        jmp .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT))
        found:
    .endif
    lda .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_HIBYTES", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT)), INDEX_REG_FOR_TABLE
    pha
    lda .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_LOBYTES", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT)), INDEX_REG_FOR_TABLE
    pha
    rts
   
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT)) = gotoMode
    stackPush "SWITCH_STATEMENT_STACK", FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT
    FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT .set FLOW_CONTROL_VALUES::SWITCH_STATEMENT_COUNT + 1
    .undefine INDEX_REG_FOR_TABLE
    .undefine SWITCH_INFO_REG
.endmacro

; --------------------------------------------------------------------------------------------
; Function case
;
;   Parameters
;       constant - immediate byte constant, eg #$23, or 'default' to define the default case.
; 
; The parameter must be a constant. If it is 'default' no more case macros can be defined 
; for this switch.

.macro case constant

    .local SWITCH_STATEMENT_COUNT
    stackPeek "SWITCH_STATEMENT_STACK", SWITCH_STATEMENT_COUNT
    .if SWITCH_STATEMENT_COUNT < 0
        ___error "'case' without 'switch'."
    .endif
    
    ; no more if default has been created:
    .ifdef .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNT))
        ___error "No more 'case' statements allowed after 'default'."
    .endif

    ; create the default label if requested and allowed:
    .if .xmatch( constant, default )
        .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", SWITCH_STATEMENT_COUNT))
            ___error "Default case not valid in 'goto' mode."
        .endif
            ___emitLabel .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNT)
        .exitmacro
    .endif

    ; must be immediate, must be constant:
    .if ! .xmatch(.left(1, {constant}) , #)
        ___error "Condition for 'case' must be immediate constant."
        ; .elseif ! .const(.mid(1, .tcount({constant}) - 1, {constant}))        ; ca65 too picky about scopes
            ; ___error "Condition for 'case' must be immediate constant."
    .endif
    
    ; keep track of the number of case macros used for this switch:
    .define thisSwitchCaseCounter .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_COUNTER", SWITCH_STATEMENT_COUNT))
    .ifndef thisSwitchCaseCounter     ; if not defined, this is the first case for this switch
        thisSwitchCaseCounter .set 0
    .endif
    
    ; .define and save constant for this case:
    .define thisSwitchCaseConstant .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_CONSTANT", SWITCH_STATEMENT_COUNT, thisSwitchCaseCounter))
    thisSwitchCaseConstant = .mid(1, .tcount({constant}) - 1, {constant})
    
    ; every case, check if the constants are well structured values starting at zero and incrementing:
    .if thisSwitchCaseCounter = 0  ; first case?
        .if thisSwitchCaseConstant = 0  ; if first case constant starts at zero, could be a simple goto index switch
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNT)) .set 1
        .else
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNT)) .set 0
        .endif
    .else ; not first case, check if this constant is equal to the old case constant + 1. If not, set to false
        .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_CONSTANT", SWITCH_STATEMENT_COUNT, thisSwitchCaseCounter - 1 )) + 1 <> thisSwitchCaseConstant
            .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNT)) .set 0
        .endif
    .endif

    ;label for this case:
    ___emitLabel .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_LABEL", SWITCH_STATEMENT_COUNT, thisSwitchCaseCounter)
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
; - define the tables (optionally, in the segment set by switch_SetDataSeg)
; - define label for 'break'

.macro endswitch
    .local SWITCH_STATEMENT_COUNT
    .local exit
    stackPeek "SWITCH_STATEMENT_STACK", SWITCH_STATEMENT_COUNT ; peek so we can use 'break'
    .if SWITCH_STATEMENT_COUNT < 0
        ___error "'endswitch' without 'switch'."
    .endif
    
    ; if goto mode option used
    .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", SWITCH_STATEMENT_COUNT)) 
        ; if NOT compliant
        .if !.ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNT))
            ___error "Cannot use 'goto' mode with this case structure. Case values must start at zero and increment."
        .endif
    ; goto option not used:    
    .else
        .if .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_START_ZERO_INCREMENTED", SWITCH_STATEMENT_COUNT)) && (!.defined(.ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNT))))
            .if ::CA65HL_WARNING_LEVEL > 1
                .warning "Use 'goto' mode with this case structure. Case values start at zero and increment. eg: switch <value>, goto"
            .endif
        .endif
    .endif
    
    .if FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG_DEFINED
        .pushseg
        .segment SWITCH_STATEMENT_DATA_SEG_STRING            
    .else
        .if ::CA65HL_WARNING_LEVEL > 1
            .warning "Data segment not defined for SWITCH. Define a data segment for optimized code. See: switch_SetDataSeg"
        .endif
        ; use break to jump over tables if in the same segment. It could be better to define this table at the beginning
        ; in the switch macro, but I don't see how to get ca65 to do this.
        break
    .endif
    
    ; define the tables:
    .define thisSwitchCaseCounter .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_COUNTER", SWITCH_STATEMENT_COUNT))
    
    ; make constant table if not goto mode
    .if !.ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_GOTO_MODE", SWITCH_STATEMENT_COUNT))
        ___emitLabel .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_CONSTANTS", SWITCH_STATEMENT_COUNT)
        .repeat thisSwitchCaseCounter, i
            .byte .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_CONSTANT", SWITCH_STATEMENT_COUNT, i))
        .endrepeat
    .endif
    
    ; make lobyte table:
    ___emitLabel .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_LOBYTES", SWITCH_STATEMENT_COUNT)
    .repeat thisSwitchCaseCounter, i
        .byte <( .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_LABEL", SWITCH_STATEMENT_COUNT, i)) - 1 )
    .endrepeat
    
    ; make hibyte table:
    ___emitLabel .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_HIBYTES", SWITCH_STATEMENT_COUNT)
    .repeat thisSwitchCaseCounter, i
        .byte >( .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_CASE_%02X_LABEL", SWITCH_STATEMENT_COUNT, i)) - 1 )
    .endrepeat
    
    .if FLOW_CONTROL_VALUES::SWITCH_STATEMENT_DATA_SEG_DEFINED
        .popseg
    .endif
    .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_TABLE_TOTAL_COUNT", SWITCH_STATEMENT_COUNT)) = thisSwitchCaseCounter
    .undefine thisSwitchCaseCounter
    ; if no matches, 'switch' macro will jump here if no default case:
    .ifndef .ident( .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNT))
        ___emitLabel .sprintf( "SWITCH_TABLE_STATEMENT_%04X_DEFAULT", SWITCH_STATEMENT_COUNT)
    .endif
    stackPop "SWITCH_STATEMENT_STACK", SWITCH_STATEMENT_COUNT ; now pop to keep stack valid
    ___popLoopType
    ___generateBreakLabel SWITCH_STATEMENT_COUNT
.endmacro

; --------------------------------------------------------------------------------------------

.endif
