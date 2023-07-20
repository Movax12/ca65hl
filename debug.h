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

; Macros to help with debugging.

.ifndef ::_DEBUG_H_
::_DEBUG_H_ = 1

; default to off
.ifndef ::__DEBUG__
    ::__DEBUG__ = 0
.endif

.ifndef ::__TODO_MESSAGES__
    ::__TODO_MESSAGES__ = 0
.endif



; Macros for conditional debug code, TODO reminders, etc
; --------------------------------------------------------------------------------------------

; use this .define to optionally output a line of code. Any code with a comma should be enclosed in curly braces ( {} )

.if ::__DEBUG__
    .define debugCode(code) code
.else 
    .define debugCode(code)
.endif

.if ::__TODO_MESSAGES__
    .define TODO(text) .warning .concat("TODO MESSAGE:",text)
.else 
    .define TODO(text)
.endif

; --------------------------------------------------------------------------------------------
; print a token string to the console to aid debugging
; --------------------------------------------------------------------------------------------

.scope PRINTIDENT
    __concatStrDefined__ .set 0
    _COUNTER_ .set 0
.endscope

.macro buildTokenStr Str
    
    .if PRINTIDENT::__concatStrDefined__
        .define __tempTStr__ _tokentStr_
        .undefine _tokentStr_
        .define _tokentStr_ .concat(__tempTStr__,Str)
        .undefine __tempTStr__
    .else
        .define _tokentStr_ Str
        PRINTIDENT::__concatStrDefined__ .set 1
    .endif
    
.endmacro

.macro clrTokenString
    .if PRINTIDENT::__concatStrDefined__
        .undefine _tokentStr_
        PRINTIDENT::__concatStrDefined__ .set 0
    .endif
.endmacro


.macro printTokenList exp

    ; exit if not in debug mode
    .if ::__DEBUG__ = 0
        .exitmacro
    .endif

     .if PRINTIDENT::_COUNTER_ >= .tcount({exp})
        .if PRINTIDENT::__concatStrDefined__ 
            .out _tokentStr_
        .endif
        PRINTIDENT::_COUNTER_ .set 0
        clrTokenString
        .exitmacro
    .endif
    
    .define THISTOKEN() .mid(PRINTIDENT::_COUNTER_, 1, {exp})
    .if .match({THISTOKEN}, an_identname)
        ; attempt to format things a bit:
        .if .xmatch({.mid(PRINTIDENT::_COUNTER_ + 1, 1, {exp})}, ::) || .xmatch({.mid(PRINTIDENT::_COUNTER_ + 1, 1, {exp})}, {,}) || \ 
            .xmatch({.mid(PRINTIDENT::_COUNTER_ + 1, 1, {exp})}, :) || .xmatch({.mid(PRINTIDENT::_COUNTER_ + 1, 1, {exp})}, {)})
            buildTokenStr .string(THISTOKEN)
        .else
            buildTokenStr .concat(.string(THISTOKEN), " ")
        .endif
    .elseif .match({THISTOKEN}, 1)
        buildTokenStr .sprintf("$%02X", THISTOKEN)
    .elseif .xmatch({THISTOKEN}, a)
        buildTokenStr "a "
    .elseif .xmatch({THISTOKEN}, x)
        buildTokenStr "x "
    .elseif .xmatch({THISTOKEN}, y)
        buildTokenStr "y "
    .elseif .xmatch({THISTOKEN}, s)
        buildTokenStr "s "
    .elseif .xmatch({THISTOKEN}, :=)
        buildTokenStr ":= "
    .elseif .xmatch({THISTOKEN}, =)
        buildTokenStr "= "
    .elseif .xmatch({THISTOKEN}, <>)
        buildTokenStr "<> "
    .elseif .xmatch({THISTOKEN}, <)
        buildTokenStr "<"
    .elseif .xmatch({THISTOKEN}, >)
        buildTokenStr ">"
    .elseif .xmatch({THISTOKEN}, <=)
        buildTokenStr "<= "
    .elseif .xmatch({THISTOKEN}, >=)
        buildTokenStr ">= "
    .elseif .xmatch({THISTOKEN}, .and)
        buildTokenStr "&& "
    .elseif .xmatch({THISTOKEN}, .or)
        buildTokenStr "|| "
    .elseif .xmatch({THISTOKEN}, .xor)
        buildTokenStr ".xor "
    .elseif .xmatch({THISTOKEN}, .not)
        buildTokenStr "! "
    .elseif .xmatch({THISTOKEN}, +)
        buildTokenStr "+ " 
    .elseif .xmatch({THISTOKEN}, -)
        buildTokenStr "- "
    .elseif .xmatch({THISTOKEN}, *)
        buildTokenStr "* "
    .elseif .xmatch({THISTOKEN}, /)
        buildTokenStr "/ "
    .elseif .xmatch({THISTOKEN}, |)
        buildTokenStr "| "
    .elseif .xmatch({THISTOKEN}, &)
        buildTokenStr "& "
    .elseif .xmatch({THISTOKEN}, <<)
        buildTokenStr "<< "
    .elseif .xmatch({THISTOKEN}, >>)
        buildTokenStr ">> "
    .elseif .xmatch({THISTOKEN}, ~)
        buildTokenStr "~"
    .elseif .xmatch({THISTOKEN}, ::)
        buildTokenStr "::"
    .elseif .xmatch({THISTOKEN}, {.})
        buildTokenStr "."
    .elseif .xmatch({THISTOKEN}, {,})
        buildTokenStr ", "
    .elseif .xmatch({THISTOKEN}, #)
        buildTokenStr "#"
    .elseif .xmatch({THISTOKEN}, :)
        buildTokenStr ":"
    .elseif .xmatch({THISTOKEN}, {(})
        buildTokenStr "("
    .elseif .xmatch({THISTOKEN}, {)})
        buildTokenStr ")"
    .elseif .xmatch({THISTOKEN}, [)
        buildTokenStr "["
    .elseif .xmatch({THISTOKEN}, ])
        buildTokenStr "]"
    .elseif .xmatch({THISTOKEN}, z:)
        buildTokenStr "z:"
    .elseif .xmatch({THISTOKEN}, a:)
        buildTokenStr "a:"
    .elseif .xmatch({THISTOKEN}, f:)
        buildTokenStr "f:"
    .else 
        buildTokenStr "?"
    .endif
    .undefine THISTOKEN
    
    PRINTIDENT::_COUNTER_ .set PRINTIDENT::_COUNTER_ + 1
    printTokenList {exp}
    
.endmacro

; --------------------------------------------------------------------------------------------

.endif