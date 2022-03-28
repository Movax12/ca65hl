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
; Section: tokeneval.h
; --------------------------------------------------------------------------------------------

; Token evaluation
; --------------------------------------------------------------------------------------------
; macros to aid with parsing long token strings

.ifndef _TOKENEVAL_
_TOKENEVAL_ = 1

.scope tokenListEval
    active                  .set 0  ; token list is being evaluated and related .defines active
    tokenOffset             .set 0  ; position in the token list
    savedTokenOffset        .set 0  ; save position to allow restoring previous
    tokenCount              .set 0  ; total tokens before EOT will be true
    verifyTokenOn           .set 0  ; verify the tokens when invoking nextToken
.endscope

; --------------------------------------------------------------------------------------------
; Establish a token list to be evaluated

.macro startTokenListEval tokenList, startOffset

    .if tokenListEval::active
        .error "startTokenListEval is active."
    .endif
    
    .ifnblank startOffset
        tokenListEval::tokenOffset .set startOffset
    .else
        tokenListEval::tokenOffset .set 0
    .endif
    tokenListEval::tokenCount .set .tcount( { tokenList } )
    tokenListEval::verifyTokenOn .set 0
    tokenListEval::active .set 1
    
    .define thisTokenList () tokenList
    .define currentToken ()  .mid (tokenListEval::tokenOffset, 1, {thisTokenList} )
    .define matchToken (t) .match ( {t}, {currentToken} )
    .define xmatchToken (t) .xmatch ( {t}, {currentToken} )
    .define EOT () ( tokenListEval::tokenOffset + 1 > tokenListEval::tokenCount )
    .define currentTokenNumber tokenListEval::tokenOffset
    .define allowedTokens
.endmacro

; --------------------------------------------------------------------------------------------
; pass a list of tokens that must have a match 
; when nextToken is next called

.macro verifyNextToken tokenList
    .undefine allowedTokens
    .define allowedTokens () tokenList
    tokenListEval::verifyTokenOn .set 1
.endmacro

; don't verify tokens
.macro allowAllTokens
    tokenListEval::verifyTokenOn .set 0
.endmacro

; --------------------------------------------------------------------------------------------
; Override the total amount of tokens for current token list

.macro setTokenCount count
    tokenListEval::tokenCount .set count
.endmacro

; --------------------------------------------------------------------------------------------
; read the next token and verify it is in the list of allowed tokens if needed

.macro nextToken
    .local matchFound
    .if !EOT
        tokenListEval::tokenOffset .set tokenListEval::tokenOffset + 1
        .if (!EOT) && tokenListEval::verifyTokenOn
            matchFound .set 0
            .repeat .tcount( {allowedTokens} ), i
                .if .match(  {currentToken}, { .mid( i,1,{allowedTokens} ) } )
                    matchFound .set 1
                .endif
            .endrepeat
            .if !matchFound
                ; it would be nice to output the offending token, but .string() doesn't work with all tokens
                .error "Error in expression."
                .fatal "STOP"
            .endif
        .endif
    .endif
.endmacro

; --------------------------------------------------------------------------------------------
; step back one token

.macro previousToken
    tokenListEval::tokenOffset .set tokenListEval::tokenOffset - 1     
.endmacro

; --------------------------------------------------------------------------------------------
; save/restore position
; can save/restore  one position. (Could add support for multiple via stack if needed.)
.macro saveTokenListPosition
    tokenListEval::savedTokenOffset .set tokenListEval::tokenOffset
.endmacro

.macro restoreTokenListPosition
    tokenListEval::tokenOffset .set tokenListEval::savedTokenOffset
.endmacro

; --------------------------------------------------------------------------------------------
; clear token list evaluation.

.macro endTokenListEval savedOffset
    .if tokenListEval::active
        .ifnblank savedOffset
            savedOffset .set tokenListEval::tokenOffset
        .endif
        .undefine thisTokenList
        .undefine currentToken
        .undefine matchToken
        .undefine xmatchToken
        .undefine EOT
        .undefine currentTokenNumber
        .undefine allowedTokens
        tokenListEval::active .set 0
    .endif
.endmacro

.endif
