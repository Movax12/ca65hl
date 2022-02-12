; --------------------------------------------------------------------------------------------
; Token evaluation
; --------------------------------------------------------------------------------------------
; SECTION: Token List
; macros to aid with parsing long token strings
.ifndef _TOKENEVAL_
_TOKENEVAL_ = 1

.scope tokenListEval
    active                  .set 0
    tokenOffset             .set 0
    savedTokenOffset        .set 0
    tokenCount              .set 0
    verifyTokenOn           .set 0
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
    .define EOT () ( tokenListEval::tokenOffset  = tokenListEval::tokenCount )
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
    matchFound .set 0
    tokenListEval::tokenOffset .set tokenListEval::tokenOffset + 1
    .if .not EOT
        .if tokenListEval::verifyTokenOn
            .repeat .tcount( {allowedTokens} ), i
                .if .match(  {currentToken}, { .mid( i,1,{allowedTokens} ) } )
                    matchFound .set 1
                .endif
            .endrepeat
            .if .not matchFound
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
