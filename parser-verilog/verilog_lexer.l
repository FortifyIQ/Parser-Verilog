/* Avoid redefinition warnings on MSVC */
%top{
#include <stdint.h>
}

%{
/* C++ string header, for string ops below */
#include <string>
#include <variant>

/* Implementation of yyFlexScanner */ 
#include "verilog_scanner.hpp"
#undef  YY_DECL
#define YY_DECL int verilog::VerilogScanner::yylex(verilog::VerilogParser::semantic_type * const lval, verilog::VerilogParser::location_type *loc )

/* typedef to make the returns for the tokens shorter */
using token = verilog::VerilogParser::token;

/* define yyterminate as this instead of NULL */
#define yyterminate() return( token::END )

/* msvc2010 requires that we exclude this header file. */
#define YY_NO_UNISTD_H

/* update location on matching */
#define YY_USER_ACTION loc->step(); loc->columns(yyleng);
%}

/* Make the generated scanner run in debug mode. */
%option debug

/* Cause the default rule (that unmatched scanner input is echoed to stdout) to be suppressed. */
%option nodefault

/* Inform flex that a derived NAME is implemented as a subclass of yyFlexLexer, so flex will place your actions in the member function foo::yylex() instead of yyFlexLexer::yylex(). */
%option yyclass="verilog::VerilogScanner"

/* Set yywrap always returns 1. yywrap is called when reaching EOF */
%option noyywrap

/* Suppress the warning message yyunput is defined but not used */
%option nounput
%option never-interactive
%option c++

/* Define inclusive/exclusive states */
%x in_comment 
%x in_attribute
%x in_directive

/* Predefined rules */
NEWLINE         "\n"|"\r\n"
SPACE           " "|"\t"|"\f"
COMMENT_BEGIN   "/*"
COMMENT_END     "*/" 
COMMENT_LINE  "//".*\n

ATTRIBUTE_BEGIN "(*"
ATTRIBUTE_END   "*)"

/* Name rule http://www.asic-world.com/verilog/syntax1.html */
NAME           [_a-zA-Z][$_a-zA-Z0-9]*{0,1023}
/* 
   Check the escape rule inside character class 
   https://www.regular-expressions.info/charclass.html
 */
ESCAPED_NAME   \\[\\^!"#$%&',()*+\-.a-zA-Z0-9/{|}~[\]_:;<=>?@]+[\t\f ]
INTEGER        [+-]?[1-9][0-9]*|0
BINARY         [+-]?[0-9]*"'"[Bb][01_xXzZ]+
OCTAL          [+-]?[0-9]*"'"[Oo][0-7_xXzZ]+ 
DECIMAL        [+-]?[0-9]*"'"[Dd][0-9_xXzZ]+
HEX            [+-]?[0-9]*"'"[Hh][0-9a-fA-F_xXzZ]+ 
REAL           [-+]?[0-9]*\.[0-9]+|[0-9]+
EXP            [-+]?[0-9]+"."?[0-9]*"E"[-+]?[0-9]+
STRING         \".*\"


/* Token rule section begins */
%%
%{ /** Code executed at the beginning of yylex **/
   yylval = lval;
%}

<*>{SPACE} { /* ignore any space */ }
{NEWLINE}  { // Update line numbers 
             loc->lines();
             // return( token::NEWLINE ); 
           }


{COMMENT_LINE}  { loc->lines(); }

{COMMENT_BEGIN} { BEGIN(in_comment); }
<in_comment><<EOF>> { 
                      BEGIN(INITIAL);
                      std::cerr << "Unclosed comment at line " << loc->end.line << " col " << loc->end.column << '\n';
                      yyterminate();
                    }
<in_comment>{NEWLINE} { loc->lines(); }
<in_comment>. { /* ignore characters in comment */ }
<in_comment>{COMMENT_END} { BEGIN(INITIAL); }


{ATTRIBUTE_BEGIN} { BEGIN(in_attribute); }
<in_attribute><<EOF>> { 
                      BEGIN(INITIAL);
                      std::cerr << "Unclosed attribute at line " << loc->end.line << " col " << loc->end.column << '\n';
                      yyterminate();
                    }
<in_attribute>{NEWLINE} { loc->lines(); }
<in_attribute>. { /* ignore characters in comment */ }
<in_attribute>{ATTRIBUTE_END} { BEGIN(INITIAL); }

\` { BEGIN(in_directive); }
<in_directive><<EOF>> { 
                      BEGIN(INITIAL);
                      std::cerr << "Unclosed directive at line " << loc->end.line << " col " << loc->end.column << '\n';
                      yyterminate();
                    }
<in_directive>\\{NEWLINE} { loc->lines(); }
<in_directive>. { /* ignore characters in directive */ }
<in_directive>{NEWLINE} { BEGIN(INITIAL); loc->lines(); }


":"|"."|"{"|"}"|"["|"]"|","|"*"|";"|"="|"-"|"+"|"%"|"("|")"|"#"|"@" {
  return yytext[0];
}

"<<"		{ yylval->build<std::string>(yytext); return token::SL; }
">>"		{ yylval->build<std::string>(yytext); return token::SR; }
">="		{ yylval->build<std::string>(yytext); return token::GE; }
">"			{ yylval->build<std::string>(yytext); return token::GT; }
"<="		{ yylval->build<std::string>(yytext); return token::LE; }
"<"			{ yylval->build<std::string>(yytext); return token::LT; }
"=="		{ yylval->build<std::string>(yytext); return token::EQ; }
"!="		{ yylval->build<std::string>(yytext); return token::NE; }
"&&"		{ yylval->build<std::string>(yytext); return token::LAND; }
"||"		{ yylval->build<std::string>(yytext); return token::LOR; }
"&"			{ yylval->build<std::string>(yytext); return token::BAND; }
"^"			{ yylval->build<std::string>(yytext); return token::BXOR; }
"|"			{ yylval->build<std::string>(yytext); return token::BOR; }
"~&"		{ yylval->build<std::string>(yytext); return token::BNAND; }
"~|"		{ yylval->build<std::string>(yytext); return token::BNOR; }
"~^"		{ yylval->build<std::string>(yytext); return token::BXNOR; }
"^~"		{ yylval->build<std::string>(yytext); return token::BXNOR; }
"~"			{ yylval->build<std::string>(yytext); return token::BNOT; }
"!"     { yylval->build<std::string>(yytext); return token::LNOT; }

module    { return token::MODULE; }
endmodule { return token::ENDMODULE; }
input     { return token::INPUT; } 
output    { return token::OUTPUT; }
inout     { return token::INOUT; }
reg       { return token::REG; }
wire      { return token::WIRE; } 
wor       { return token::WOR; }
wand      { return token::WAND; }
tri       { return token::TRI; }
trior     { return token::TRIOR; }
triand    { return token::TRIAND; }
SUPPLY0   { return token::SUPPLY0; }
SUPPLY1   { return token::SUPPLY1; }
assign    { return token::ASSIGN; }

always    { return token::ALWAYS; }
and       { return token::AND; }
begin     { return token::BEGINKEY; }
case      { return token::CASE; }
casex     { return token::CASEX; }
casez     { return token::CASEZ; }
default   { return token::DEFAULT; }
else      { return token::ELSE; }
end       { return token::ENDKEY; }
endcase   { return token::ENDCASE; }
endtask   { return token::ENDTASK; }
for       { return token::FOR; }
if        { return token::IF; }
initial   { return token::INITIALKEY; }
integer   { return token::INTEGERKEY; }
logic     { return token::LOGIC; }
negedge   { return token::NEGEDGE; }
or        { return token::OR; }
parameter { return token::PARAMETER; }
posedge   { return token::POSEDGE; }
real      { return token::REALKEY; }
realtime  { return token::REALTIME; }
repeat    { return token::REPEAT; }
signed    { return token::SIGNED; }
task      { return token::TASK; }
time      { return token::TIME; }
tri0      { return token::TRI0; }
tri1      { return token::TRI1; }
wait      { return token::WAIT; }
while      { return token::WHILE; }

{ESCAPED_NAME}  { 
  yylval->build<std::string>(yytext);
  return token::ESCAPED_NAME;
}

{NAME} { 
  yylval->build<std::string>( yytext );
  return token::NAME;
}

\${NAME} {
  yylval->build<std::string>(yytext);
  return token::SYSTASKFUNC;
}

{INTEGER} {
            yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::INTEGER)); 
            return token::INTEGER;
          }

{BINARY} {
           yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::BINARY));
           return token::BINARY;
         }

{OCTAL} {
          yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::OCTAL));
          return token::OCTAL;
        }

{DECIMAL} {
            yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::DECIMAL));
            return token::DECIMAL;
          }

{HEX} {
        yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::HEX));
        return token::HEX;
      }

{REAL} {
         yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::REAL));
         return token::REAL;
       }

{EXP} {
        yylval->build<verilog::Constant>(verilog::Constant(yytext, verilog::ConstantType::EXP));
        return token::EXP;
      }

{STRING} {
  yylval->build<std::string>(yytext);
  return token::STRING;
}


. {  
    /* Last rule catches any unmatched character */
    std::cerr << "Failed to match : " << yytext << '\n';
    yyterminate();
  }
%%


