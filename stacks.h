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
; Section: stacks.h
;
; Macros for stacks in ca65

; --------------------------------------------------------------------------------------------
; SECTION: Stack
;
; push and pop macros.. save a value on a user named stack.
; stackname - string
; value - value to be stored
.ifndef _STACKS_
_STACKS_ = 1

.scope stacksValues
    poppedTokenListActive .set 0    ; if the .define poppedTokenList is set
    saveStackPointer      .set -1   ; if save is available 
.endscope

; --------------------------------------------------------------------------------------------
; save and restore a stack pointer - can only save one at a time

.macro saveStackPointer stackname
    .if stacksValues::saveStackPointer <> -1
        .error "Stack pointer yet to be restored."
    .endif
    .define thisStackPointer    ::.ident(.sprintf("___%s_STACKPOINTER__", stackname))
    stacksValues::saveStackPointer .set thisStackPointer
    .undefine thisStackPointer
.endmacro

.macro restoreStackPointer stackname
    .define thisStackPointer    ::.ident(.sprintf("___%s_STACKPOINTER__", stackname))
    thisStackPointer .set stacksValues::saveStackPointer
    .undefine thisStackPointer
    stacksValues::saveStackPointer .set -1
.endmacro

; --------------------------------------------------------------------------------------------
; push a value to a named stack
; stackname - string
; value - value to save

.macro stackPush stackname, value
    .define thisStackPointer    ::.ident(.sprintf("___%s_STACKPOINTER__", stackname))
    ; if not defined, create it:
    .ifndef thisStackPointer
        thisStackPointer .set 0
    .endif
    ::.ident( .sprintf("%s_%04X_",stackname, thisStackPointer )) .set value
    thisStackPointer .set thisStackPointer  + 1
    .undefine thisStackPointer
.endmacro

; --------------------------------------------------------------------------------------------
; pop a value from a named stack
; stackname - string
; value - ident to pop/store value into

.macro stackPop stackname, value ; puts a -1 in var if there is a problem
    .define thisStackPointer    ::.ident(.sprintf("___%s_STACKPOINTER__", stackname))
    .ifndef thisStackPointer ; stack not defined
        value .set -1
    .elseif  thisStackPointer - 1 < 0  ; or negative
        value .set -1
    .else
        thisStackPointer .set thisStackPointer - 1
        value .set ::.ident( .sprintf("%s_%04X_", stackname, thisStackPointer) )
    .endif
    .undefine thisStackPointer
.endmacro

; --------------------------------------------------------------------------------------------
; look at a value from a named stack without changing the stack
; stackname - string
; value - ident to pop/store value into
; sets value to -1 if stack underflow

.macro stackPeek stackname, value 
    .define thisStackPointer    ::.ident(.sprintf("___%s_STACKPOINTER__", stackname))
    .ifndef thisStackPointer ; stack not defined
        value .set -1
    .elseif  thisStackPointer - 1 < 0  ; or negative
        value .set -1
    .else
        value .set ::.ident( .sprintf("%s_%04X_", stackname, thisStackPointer - 1 ) )
    .endif
    .undefine thisStackPointer
.endmacro

; --------------------------------------------------------------------------------------------
; Save a list of tokens to a named stack
; stackname - string to identify stack
; tokenList - any number of tokens that could be valid in ca65 (doesn't have to be valid code)
; After a pop, access the token list with poppedTokenList

.macro pushTokenList stackname, tokenList
    .define thisStackPointer ::.ident(.sprintf("___%s_TL_STACKPOINTER__", stackname))
    ; if not defined, create it:
    .ifndef thisStackPointer
        thisStackPointer .set 0
    .endif
    .define .ident( .sprintf("%s_%04X_",stackname, ::.ident(.sprintf("___%s_TL_STACKPOINTER__", stackname)) )) () tokenList
    thisStackPointer .set thisStackPointer  + 1
    .undefine thisStackPointer
.endmacro

; ca65 copies the token list on macro call, so use this macro to get a copy and be able to undefine the stack element
.macro tokenListPopHelper tokenlist
    .define poppedTokenList() tokenlist
.endmacro

.macro popTokenList stackname
    .define thisStackPointer ::.ident(.sprintf("___%s_TL_STACKPOINTER__", stackname))
    .if stacksValues::poppedTokenListActive
        .undefine poppedTokenList
    .endif
    .if (!.defined(thisStackPointer)) ||  (thisStackPointer - 1 < 0) ; stack not defined OR empty stack
        tokenListPopHelper null
    .else
        thisStackPointer .set thisStackPointer - 1
        tokenListPopHelper { .ident( .sprintf("%s_%04X_", stackname, thisStackPointer) ) }
        .undefine .ident( .sprintf("%s_%04X_", stackname, ::.ident(.sprintf("___%s_TL_STACKPOINTER__", stackname)) ))
    .endif
    stacksValues::poppedTokenListActive .set 1
    .undefine thisStackPointer
.endmacro

.macro peekTokenList stackname
    .define thisStackPointer ::.ident(.sprintf("___%s_TL_STACKPOINTER__", stackname))
    .if stacksValues::poppedTokenListActive
        .undefine poppedTokenList
    .endif
    .if (!.defined(thisStackPointer)) ||  (thisStackPointer - 1 < 0) ; stack not defined OR empty stack
        tokenListPopHelper null
    .else
        tokenListPopHelper { .ident( .sprintf("%s_%04X_", stackname, thisStackPointer - 1) ) }
    .endif
    stacksValues::poppedTokenListActive .set 1
    .undefine thisStackPointer
.endmacro

; --------------------------------------------------------------------------------------------

.endif
