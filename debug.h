; --------------------------------------------------------------------------------------------
; Macros to help with debugging.

.ifndef _DEBUG_MACROS_
_DEBUG_MACROS_ = 1


.ifndef DEBUG_H_ON
    DEBUG_H_ON = 0
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

    .if ::DEBUG_H_ON = 0
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
    
    .define THISTOKEN() .mid(PRINTIDENT::_COUNTER_,1,{exp})
    .if .match({THISTOKEN}, an_identname)
        buildTokenStr .concat( " ",.string(THISTOKEN), " ")
    .elseif .match({THISTOKEN}, 12345)
        buildTokenStr .string(THISTOKEN)
    .elseif .xmatch({THISTOKEN}, a)
        buildTokenStr "a "
    .elseif .xmatch({THISTOKEN}, x)
        buildTokenStr "x "
    .elseif .xmatch({THISTOKEN}, y)
        buildTokenStr "y "
    .elseif .xmatch({THISTOKEN}, s)
        buildTokenStr "s "
    .elseif .xmatch({THISTOKEN}, :=)
        buildTokenStr ":="
    .elseif .xmatch({THISTOKEN}, =)
        buildTokenStr "="
    .elseif .xmatch({THISTOKEN}, <>)
        buildTokenStr "<>"
    .elseif .xmatch({THISTOKEN}, <)
        buildTokenStr "<"
    .elseif .xmatch({THISTOKEN}, >)
        buildTokenStr ">"
    .elseif .xmatch({THISTOKEN}, <=)
        buildTokenStr "<="
    .elseif .xmatch({THISTOKEN}, >=)
        buildTokenStr ">="
    .elseif .xmatch({THISTOKEN}, .and)
        buildTokenStr " .and "
    .elseif .xmatch({THISTOKEN}, .or)
        buildTokenStr " .or "
    .elseif .xmatch({THISTOKEN}, .xor)
        buildTokenStr " .xor "
    .elseif .xmatch({THISTOKEN}, .not)
        buildTokenStr " .not "
    .elseif .xmatch({THISTOKEN}, +)
        buildTokenStr " + " 
    .elseif .xmatch({THISTOKEN}, -)
        buildTokenStr " - "
    .elseif .xmatch({THISTOKEN}, *)
        buildTokenStr " * "
    .elseif .xmatch({THISTOKEN}, /)
        buildTokenStr " / "
    .elseif .xmatch({THISTOKEN}, !)
        buildTokenStr " ! "
    .elseif .xmatch({THISTOKEN}, |)
        buildTokenStr " | "
    .elseif .xmatch({THISTOKEN}, ^)
        buildTokenStr "^"
    .elseif .xmatch({THISTOKEN}, &)
        buildTokenStr " & "
    .elseif .xmatch({THISTOKEN}, <<)
        buildTokenStr " << "
    .elseif .xmatch({THISTOKEN}, >>)
        buildTokenStr " >> "
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
        buildTokenStr "( "
    .elseif .xmatch({THISTOKEN}, {)})
        buildTokenStr " )"
    .elseif .xmatch({THISTOKEN}, [)
        buildTokenStr "["
    .elseif .xmatch({THISTOKEN}, ])
        buildTokenStr "]"
    .elseif .xmatch({THISTOKEN}, Z:)
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