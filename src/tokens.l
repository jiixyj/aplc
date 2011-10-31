/* →  /xe2 /x86 /x92 */
NSND [^0-9/\xe2,"\-\*\[\][:space:]?:=>|^&<+%_()]|\xe2[^\x86]|\xe2\x86[^\x92]
NS   {NSND}|[0-9]

%{
#include <string>
#include "node.h"
#include "parser.hpp"
#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval.token = t)
extern "C" int yywrap() { }
%}

%%

[0-9]*                  SAVE_TOKEN; return TINTEGER;
\"(\\.|[^\\"])*\"       SAVE_TOKEN; return TSTRING;
{NSND}{NS}*             SAVE_TOKEN; return TIDENTIFIER;

^[ ]+                   ;
[ ]+$                   ;
[ ]+                    return TOKEN(TSPACE);
[[:space:]]             ;

[ ]*"?"[ ]*             return TOKEN(TQUES);
[ ]*":"[ ]*             return TOKEN(TCOL);
[ ]*"="[ ]*             return TOKEN(TASSIGN);
[ ]*"→"[ ]*             return TOKEN(TARROW);
[ ]*">>>"[ ]*           return TOKEN(TSHRS);
[ ]*"||"[ ]*            return TOKEN(TCOR);
[ ]*"^^"[ ]*            return TOKEN(TCXOR);
[ ]*"&&"[ ]*            return TOKEN(TCAND);
[ ]*"=="[ ]*            return TOKEN(TCEQ);
[ ]*"!="[ ]*            return TOKEN(TCNE);
[ ]*"<="[ ]*            return TOKEN(TCLE);
[ ]*">="[ ]*            return TOKEN(TCGE);
[ ]*">>"[ ]*            return TOKEN(TSHR);
[ ]*"<<"[ ]*            return TOKEN(TSHL);
[ ]*">"[ ]*             return TOKEN(TCGT);
[ ]*"<"[ ]*             return TOKEN(TCLT);
[ ]*"|"[ ]*             return TOKEN(TOR);
[ ]*"^"[ ]*             return TOKEN(TXOR);
[ ]*"&"[ ]*             return TOKEN(TAND);
[ ]*"+"[ ]*             return TOKEN(TPLUS);
[ ]*"-"[ ]*             return TOKEN(TMINUS);
[ ]*"*"[ ]*             return TOKEN(TMUL);
[ ]*"/"[ ]*             return TOKEN(TDIV);
[ ]*"%"[ ]*             return TOKEN(TMOD);
"_"[ ]*                 return TOKEN(TUNDER);
"("[ ]*                 return TOKEN(TLPAREN);
[ ]*")"                 return TOKEN(TRPAREN);
"["[ ]*                 return TOKEN(TLBRACK);
[ ]*"]"                 return TOKEN(TRBRACK);
[ ]*","[ ]*             return TOKEN(TCOMMA);
.                       printf("Unknown token!\n"); yyterminate();

%%
