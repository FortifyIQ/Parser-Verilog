%skeleton "lalr1.cc"
%require  "3.0"
%debug 
%defines 
%define api.namespace {verilog}
%define api.parser.class {VerilogParser}

%define parse.error verbose

%code requires{
  #include "verilog_data.hpp"

  namespace verilog {
    class ParserVerilogInterface;
    class VerilogScanner;
  }

// The following definitions is missing when %locations isn't used
# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif 

}

%parse-param { VerilogScanner &scanner }
%parse-param { ParserVerilogInterface *driver }

%code {
  #include <iostream>
  #include <cstdlib>
  #include <fstream>
  #include <utility>
  #include <tuple>
  
   /* include for all driver functions */
  #include "verilog_driver.hpp"

#undef yylex
#define yylex scanner.yylex
}

%define api.value.type variant
%define parse.assert

%left LOR
%left LAND
%left BOR
%left BXOR OR
%left BAND AND
%left EQ NE
%left GT GE LT LE
%left SL SR
%left '-' '+'
%left '*' '/' '%'
%left UMINUS

%right Right Shift

%nonassoc UNARY

%token              END    0     "end of file"
%token              NEWLINE
%token              UNDEFINED 

/* Valid name (Identifiers) */
%token<std::string> NAME 
%token<std::string> ESCAPED_NAME  

/* Actual string */
%token<std::string> STRING

/* Built-in system tasks and functions names*/
%token<std::string> SYSTASKFUNC

%token<verilog::Constant> INTEGER BINARY OCTAL DECIMAL HEX REAL EXP

/* Keyword tokens */
// Structural
%token MODULE ENDMODULE INPUT OUTPUT INOUT REG WIRE WAND WOR TRI TRIOR TRIAND SUPPLY0 SUPPLY1 ASSIGN
// Procedural
%token<std::string> SL SR GE GT LE LT EQ NE LAND LOR BAND BXOR BOR BNAND BNOR BXNOR BNOT LNOT
%token ALWAYS AND BEGINKEY CASE CASEX CASEZ DEFAULT ELSE ENDKEY ENDCASE ENDTASK FOR IF INITIALKEY INTEGERKEY LOGIC NEGEDGE OR PARAMETER POSEDGE REALKEY REALTIME REPEAT SIGNED TASK TIME TRI0 TRI1 WAIT WHILE

/* Nonterminal Symbols */
%type<std::string> valid_name hierarchical_identifier system_function_call function_call

%type<std::pair<verilog::PortDirection, verilog::ConnectionType>> port_type 
%type<verilog::Port> port_declarations port_decl port_decl_statements

%type<verilog::NetType> net_type
%type<verilog::Net> net_decl_statements net_decl 

%type<verilog::Parameter> param_assignment
%type<std::vector<Parameter>> parameter_decl list_of_param_assignments

%type<verilog::Var> variable_type
%type<std::vector<verilog::Var>> reg_decl logic_decl integer_decl list_of_variable_identifiers

%type<verilog::Task> task_decl task_item_decls task_item_decl
%type<verilog::Expression> task_arg
%type<std::vector<verilog::Expression>> task_args

%type<verilog::Block> seq_block block_item_decls block_item_decl
%type<verilog::Statement> block_statement
%type<std::vector<verilog::Statement>> block_statements

%type<verilog::BlockingAssignment> blocking_assignment
%type<verilog::CaseStatement> case_statement
%type<verilog::CaseItem> case_item
%type<std::vector<verilog::CaseItem>> case_items
%type<verilog::ConditionalStatement> conditional_statement
%type<std::vector<std::pair<verilog::Expression, verilog::Statement>>> else_if_statements
%type<verilog::RepeatStatement> repeat_statement
%type<verilog::WhileStatement> while_statement
%type<verilog::ForStatement> for_statement
%type<std::string> system_task_enable task_enable
%type<verilog::WaitStatement> wait_statement
%type<std::string> procedural_timing_control event_control delay_control event_expr

%type<verilog::Constant> constant
%type<verilog::Assignment> assignment 
%type<std::vector<std::variant<std::string, verilog::NetBit, verilog::NetRange>>> lhs lhs_concat lhs_exprs lhs_expr
%type<std::vector<verilog::NetConcat>> rhs rhs_concat rhs_exprs rhs_expr 

%type<verilog::Instance> instance  
%type<std::pair<std::vector<std::variant<std::string, NetBit, NetRange>>, std::vector<std::vector<verilog::NetConcat>>>> inst_pins nets_by_name 

%type<std::vector<std::vector<verilog::NetConcat>>> nets_by_position

%type<std::pair<std::variant<std::string, NetBit, NetRange>, std::vector<verilog::NetConcat>>> net_by_name 

%type<verilog::Expression> expr primary variable_lvalue optional_range range_expr
%type<std::vector<verilog::Expression>> expr_list concatenation
%type<std::pair<verilog::Expression, verilog::Expression>> variable_assignment

%type<std::string> binary_operator

%locations 
%start design

%%

valid_name
  : NAME { $$ = $1; }
  | ESCAPED_NAME { $$ = $1; }
  ;

design 
  : modules;

modules
  :
  | modules module
  ;

module
  : MODULE valid_name module_parameter_port_list ';' 
    { 
      driver->add_module(std::move($2));
    }
    statements ENDMODULE  
  | MODULE valid_name module_parameter_port_list '(' ')' ';'
    {
      driver->add_module(std::move($2));
    }
    statements ENDMODULE
  | MODULE valid_name module_parameter_port_list '(' port_names ')' ';' 
    {
      driver->add_module(std::move($2));
    }
    statements ENDMODULE
  | MODULE valid_name module_parameter_port_list '(' 
    { 
      driver->add_module(std::move($2)); 
    } 
    port_declarations ')' 
    { 
      driver->add_port(std::move($6)); 
    }
    ';' statements ENDMODULE 
  ;

module_parameter_port_list
	:
	| '#' '(' parameter_declarations ')'
	;

parameter_declarations
	: parameter_declarations ',' PARAMETER parameter_declaration
	| PARAMETER parameter_declaration
	;

parameter_declaration
	: SIGNED range_expr parameter_assignments
	| INTEGER parameter_assignments
	| REAL parameter_assignments
	| REALTIME parameter_assignments
	| TIME parameter_assignments
	;

parameter_assignments
	: parameter_assignments ',' %prec Right valid_name '=' expr
	| valid_name '=' expr
	;

// port names are ignored as they will be parsed later in declaration
port_names 
  : valid_name { }
  | port_names ',' valid_name  { }
  ; 


port_type 
  : INPUT      { $$ = std::make_pair(verilog::PortDirection::INPUT, verilog::ConnectionType::NONE); }
  | INPUT WIRE { $$ = std::make_pair(verilog::PortDirection::INPUT, verilog::ConnectionType::WIRE); }
  | OUTPUT     { $$ = std::make_pair(verilog::PortDirection::OUTPUT,verilog::ConnectionType::NONE); }
  | OUTPUT REG { $$ = std::make_pair(verilog::PortDirection::OUTPUT,verilog::ConnectionType::REG);  }
  | INOUT      { $$ = std::make_pair(verilog::PortDirection::INOUT, verilog::ConnectionType::NONE); }
  | INOUT WIRE { $$ = std::make_pair(verilog::PortDirection::INOUT, verilog::ConnectionType::WIRE); }
  | INOUT REG  { $$ = std::make_pair(verilog::PortDirection::INOUT, verilog::ConnectionType::REG);  }
  ;

// e.g. "input a, b, output c, d" is allowed in port declarations
port_declarations
  : port_decl 
    {
      $$ = $1;
    }
  | port_declarations ',' port_decl  
    {
      driver->add_port(std::move($1));
      $$ = $3;
    }
  | port_declarations ',' valid_name 
    {
      $1.names.emplace_back(std::move($3));    
      $$ = $1;
    }
  ;

port_decl 
  : port_type valid_name 
    {
      $$.dir  = std::get<0>($1);
      $$.type = std::get<1>($1);
      $$.names.emplace_back(std::move($2)); 
    }
  | port_type '[' INTEGER ':' INTEGER ']' valid_name  
    {
      $$.dir  = std::get<0>($1);
      $$.type = std::get<1>($1);
      $$.beg = std::stoi($3.value);
      $$.end = std::stoi($5.value);
      $$.names.push_back(std::move($7)); 
    }
  ;

statements 
  : // empty
  | statements statement
  | statements statement_assign
  ; 

statement
  : declaration
  | instance
  | initial_construct
  | always_construct
  ;


declaration 
  : port_decl_statements ';' { driver->add_port(std::move($1)); } 
  | net_decl_statements  ';' { driver->add_net(std::move($1)); }
  | parameter_decl
    {
      for (auto &decl : $1)
        driver->add_parameter(std::move(decl));
    }
  | reg_decl
    {
      for (auto &decl : $1)
        driver->add_var(std::move(decl));
    }
  | logic_decl
    {
      for (auto &decl : $1)
        driver->add_var(std::move(decl));
    }
  | integer_decl
    {
      for (auto &decl : $1)
        driver->add_var(std::move(decl));
    }
  | task_decl {driver->add_task(std::move($1));}
  ;

// e.g. "input a, b, output c, d" is not allowed in port declaration statements 
port_decl_statements
  : port_decl 
    {
      $$ = $1;
    }
  | port_decl_statements ',' valid_name 
    {
      $1.names.emplace_back(std::move($3));    
      $$ = $1;
    }
  ;


net_type 
  :  WIRE    { $$ = verilog::NetType::WIRE;    }
  |  WAND    { $$ = verilog::NetType::WAND;    }
  |  WOR     { $$ = verilog::NetType::WOR;     }
  |  TRI     { $$ = verilog::NetType::TRI;     }
  |  TRIOR   { $$ = verilog::NetType::TRIOR;   }
  |  TRIAND  { $$ = verilog::NetType::TRIAND;  }
  |  SUPPLY0 { $$ = verilog::NetType::SUPPLY0; }
  |  SUPPLY1 { $$ = verilog::NetType::SUPPLY1; }
  ;

net_decl_statements
  : net_decl 
    {
      $$ = $1;
    }
  | net_decl_statements ',' valid_name 
    {
      $1.names.push_back(std::move($3));
      $$ = $1;
    }
  ;

net_decl 
  : net_type valid_name 
    {
      $$.type = $1;
      $$.names.push_back(std::move($2)); 
    }
  | net_type '[' INTEGER ':' INTEGER ']' valid_name  
    {
      $$.type = $1;
      $$.beg = std::stoi($3.value);
      $$.end = std::stoi($5.value);
      $$.names.push_back(std::move($7)); 
    }
  ;

parameter_decl
  : PARAMETER list_of_param_assignments ';'
    {$$ = std::move($2);}
  ;

list_of_param_assignments
  : param_assignment
    {$$.push_back(std::move($1));}
  | list_of_param_assignments ',' param_assignment
    {
      $1.push_back(std::move($3));
      $$ = std::move($1);
    }
  ;

param_assignment
  : hierarchical_identifier '=' expr
    {
      $$.name = std::move($1);
      $$.rval = std::move($3);
    }
  ;

reg_decl
  : REG optional_range list_of_variable_identifiers ';'
    {
      for (auto &decl : $3) {
        decl.type = verilog::VarType::REG;
        decl.range = std::move($2);
        $$.push_back(std::move(decl));
      }
    }
  ;

list_of_variable_identifiers
  : variable_type
    {$$.push_back(std::move($1));}
  | list_of_variable_identifiers ',' variable_type
    {
      $1.push_back(std::move($3));
      $$ = std::move($1);
    }
  ;

variable_type
  : valid_name
    {$$.name = $1;}
  | valid_name '=' expr
    {
      $$.name = $1;
      $$.rval = std::move($3);
    }
  | valid_name range_expr
    {$$.name = $1;}
  ;

logic_decl
  : LOGIC optional_range list_of_variable_identifiers ';'
    {
      for (auto &decl : $3) {
        decl.type = verilog::VarType::LOGIC;
        decl.range = std::move($2);
        $$.push_back(std::move(decl));
      }
    }
  ;

integer_decl
  : INTEGERKEY list_of_variable_identifiers ';'
    {
      for (auto &decl : $2) {
        decl.type = verilog::VarType::INTEGERKEY;
        $$.push_back(std::move(decl));
      }
    }
  ;

task_decl
  : TASK valid_name ';' block_statement ENDTASK
    {
      $$.name = $2;
      $$.statement = std::move($4);
    }
  | TASK valid_name ';' task_item_decls block_statement ENDTASK
    {
      $$.name = $2;
      $$.args = std::move($4.args);
      $$.parameters = std::move($4.parameters);
      $$.vars = std::move($4.vars);
      $$.statement = std::move($5);
    }
  | TASK valid_name '(' ')' ';' block_statement ENDTASK
    {
      $$.name = $2;
      $$.statement = std::move($6);
    }
  | TASK valid_name '(' ')' ';' block_item_decls block_statement ENDTASK
    {
      $$.name = $2;
      $$.parameters = std::move($6.parameters);
      $$.vars = std::move($6.vars);
      $$.statement = std::move($7);
    }
  | TASK valid_name '(' task_args ')' ';' block_statement ENDTASK
    {
      $$.name = $2;
      $$.args = std::move($4);
      $$.statement = std::move($7);
    }
  | TASK valid_name '(' task_args ')' ';' block_item_decls block_statement ENDTASK
    {
      $$.name = $2;
      $$.args = std::move($4);
      $$.parameters = std::move($7.parameters);
      $$.vars = std::move($7.vars);
      $$.statement = std::move($8);
    }
  ;

task_item_decls
: task_item_decl
    {$$ = std::move($1);}
  | task_item_decls task_item_decl
    {
      $1.args.insert($1.args.end(), std::make_move_iterator($2.args.begin()), std::make_move_iterator($2.args.end()));
      $1.parameters.insert($1.parameters.end(), std::make_move_iterator($2.parameters.begin()), std::make_move_iterator($2.parameters.end()));
      $1.vars.insert($1.vars.end(), std::make_move_iterator($2.vars.begin()), std::make_move_iterator($2.vars.end()));
      $$ = std::move($1);
    }
  ;

task_item_decl
  : block_item_decl
    {
      $$.parameters = std::move($1.parameters);
      $$.vars = std::move($1.vars);
    }
  | task_arg
    {$$.args.push_back(std::move($1));}
  ;

task_args
	: task_args ',' task_arg
    {
      $1.emplace_back(std::move($3));
      $$ = std::move($1);
    }
	| task_arg
    {$$.emplace_back(std::move($1));}
	;

task_arg
	: INOUT optional_range valid_name
    {
      $$.type = verilog::ExpressionType::IDENTIFIER;
      $$.string = $3;
      $$.leftOperand = std::move($2.leftOperand);
      $$.rightOperand = std::move($2.rightOperand);
    }
	| INPUT optional_range valid_name
    {
      $$.type = verilog::ExpressionType::IDENTIFIER;
      $$.string = $3;
      $$.leftOperand = std::move($2.leftOperand);
      $$.rightOperand = std::move($2.rightOperand);
    }
	| OUTPUT optional_range valid_name
    {
      $$.type = verilog::ExpressionType::IDENTIFIER;
      $$.string = $3;
      $$.leftOperand = std::move($2.leftOperand);
      $$.rightOperand = std::move($2.rightOperand);
    }
  ;

seq_block
  : BEGINKEY block_statements ENDKEY
    {$$.statements = std::move($2);}
  | BEGINKEY ':' valid_name block_statements ENDKEY
    {
      $$.name = $3;
      $$.statements = std::move($4);
    }
  | BEGINKEY ':' valid_name block_item_decls block_statements ENDKEY
    {
      $$.name = $3;
      $$.parameters = std::move($4.parameters);
      $$.vars = std::move($4.vars);
      $$.statements = std::move($5);
    }
  ;

block_item_decls
  : block_item_decl
    {$$ = std::move($1);}
  | block_item_decls block_item_decl
    {
      $1.parameters.insert($1.parameters.end(), std::make_move_iterator($2.parameters.begin()), std::make_move_iterator($2.parameters.end()));
      $1.vars.insert($1.vars.end(), std::make_move_iterator($2.vars.begin()), std::make_move_iterator($2.vars.end()));
      $$ = std::move($1);
    }
  ;

block_item_decl
  : parameter_decl
    {
      $$.parameters = std::move($1);
    }
  | reg_decl
    {
      $$.vars = std::move($1);
    }
  | integer_decl
    {
      $$.vars = std::move($1);
    }
  ;

block_statements
	: block_statements block_statement
    {
      $1.emplace_back(std::move($2));
      $$ = std::move($1);
    }
	| block_statement
    {$$.emplace_back(std::move($1));}
	;

block_statement
	: ';'
    {$$ = "";}
  | blocking_assignment ';'
    {$$ = std::move($1);}
  | case_statement
    {$$ = std::move($1);}
  | conditional_statement
    {$$ = std::move($1);}
  | repeat_statement
    {$$ = std::move($1);}
  | while_statement
    {$$ = std::move($1);}
  | for_statement
    {$$ = std::move($1);}
  | seq_block
    {$$ = std::move($1);}
  | system_task_enable
    {$$ = "//System task skipped";}
  | task_enable
    {$$ = std::move($1);}
  | wait_statement
    {$$ = std::move($1);}
  | event_control block_statement
    {$$ = verilog::TimingControl(std::move($1), std::move($2), std::move(verilog::ControlType::EVENT));}
  | delay_control block_statement
    {$$ = verilog::TimingControl(std::move($1), std::move($2), std::move(verilog::ControlType::DELAY));}
	;

blocking_assignment
  : variable_lvalue '=' expr
    {
      $$.lval = std::move($1);
      $$.rval = std::move($3);
    }
  | variable_lvalue '=' procedural_timing_control expr
    {
      $$.timeControl = std::move($3);
      $$.lval = std::move($1);
      $$.rval = std::move($4);
    }
  ;

variable_lvalue
  : hierarchical_identifier optional_range
    {
      $$.string = $1;
      $$.leftOperand = std::move($2.leftOperand);
      $$.rightOperand = std::move($2.rightOperand);
      $$.type = verilog::ExpressionType::IDENTIFIER;
    }
  ;

case_statement
	: CASE '(' expr ')' case_items ENDCASE
    {
      $$.type = verilog::CaseType::CASE;
      $$.expr = std::move($3);
      $$.items = std::move($5);
    }
	| CASEX '(' expr ')' case_items ENDCASE
    {
      $$.type = verilog::CaseType::CASEX;
      $$.expr = std::move($3);
      $$.items = std::move($5);
    }
	| CASEZ '(' expr ')' case_items ENDCASE
    {
      $$.type = verilog::CaseType::CASEZ;
      $$.expr = std::move($3);
      $$.items = std::move($5);
    }
	;

case_items
	: case_items case_item
    {
      $1.emplace_back(std::move($2));
      $$ = std::move($1);
    }
	| case_item
    {
      $$.emplace_back(std::move($1));
    }
	;

case_item
	: expr_list ':' block_statement
    {
      $$.expressions = std::move($1);
      $$.statement = std::move($3);
    }
	| DEFAULT ':' block_statement
    {
      $$.isDefault = true;
      $$.statement = std::move($3);
    }
	| DEFAULT block_statement
    {
      $$.isDefault = true;
      $$.statement = std::move($2);
    }
	;

conditional_statement
  : IF '(' expr ')' block_statement
    {
      $$.ifCondition = std::move($3);
      $$.ifStatement = std::move($5);
    }
  | IF '(' expr ')' block_statement ELSE block_statement
    {
      $$.ifCondition = std::move($3);
      $$.ifStatement = std::move($5);
      $$.elseStatement = std::move($7);
    }
  | IF '(' expr ')' block_statement else_if_statements
    {
      $$.ifCondition = std::move($3);
      $$.ifStatement = std::move($5);
      $$.elifs = std::move($6);
    }
  | IF '(' expr ')' block_statement else_if_statements ELSE block_statement
    {
      $$.ifCondition = std::move($3);
      $$.ifStatement = std::move($5);
      $$.elifs = std::move($6);
      $$.elseStatement = std::move($8);
    }
  ;

else_if_statements
  : ELSE IF '(' expr ')' block_statement
    {
      $$.emplace_back(std::move(std::make_pair($4, $6)));
    }
  | else_if_statements ELSE IF '(' expr ')' block_statement
    {
      $1.emplace_back(std::move(std::make_pair($5, $7)));
      $$ = std::move($1);
    }
  ;

repeat_statement
  : REPEAT '(' expr ')' block_statement
    {
      $$.expr = std::move($3);
      $$.statement = std::move($5);
    }
  ;

while_statement
  : WHILE '(' expr ')' block_statement
    {
      $$.condition = std::move($3);
      $$.statement = std::move($5);
    }
  ;

for_statement
  : FOR '(' variable_assignment ';' expr ';' variable_assignment ')' block_statement
    {
      $$.initAssign = std::move($3);
      $$.condition = std::move($5);
      $$.iterAssign = std::move($7);
      $$.statement = std::move($9);
    }
  ;

variable_assignment
  : variable_lvalue '=' expr
    {
      $$.first = std::move($1);
      $$.second = std::move($3);
    }
  ;

system_task_enable
  : SYSTASKFUNC ';'
    {
      $$ = $1;
    }
  | SYSTASKFUNC '(' ')' ';'
    {
      $$ = $1;
    }
  | SYSTASKFUNC '(' expr_list ')' ';'
    {
      $$ = $1;
    }
  ;

task_enable
  : hierarchical_identifier ';'
    {$$ = $1 + "();";}
  | hierarchical_identifier '(' expr_list ')' ';'
    {
      $$ = $1 + '(';
      for (const auto &expression : $3)
        $$ += expression.string;
      $$ += ");";
    }
  ;

wait_statement
  : WAIT '(' expr ')' block_statement
    {
      $$.expr = std::move($3);
      $$.statement = std::move($5);
    }
  ;

procedural_timing_control
	: delay_control
	| event_control
	;

delay_control
  : '#' INTEGER
    {$$ = $2.value;}
  | '#' REAL
    {$$ = $2.value;}
  | '#' valid_name
    {$$ = $2;}
	;

event_control
	: '@' hierarchical_identifier
    {$$ = $2;}
	| '@' '(' event_expr ')'
    {$$ = $3;}
	| '@' '(' '*' ')'
    {$$ = "( * )";}
	| '@' '*'
    {$$ = " * ";}
	;

event_expr
	: expr
    {$$ = $1.string;}
	| POSEDGE expr
    {$$ = "posedge(" + $2.string + ')';}
	| NEGEDGE expr
    {$$ = "negedge(" + $2.string + ')';}
	| event_expr OR event_expr
    {$$ = $1 + " || " + $3;}
	| event_expr ',' event_expr
    {$$ = $1 + ", " + $3;}
	;


statement_assign
  : ASSIGN assignments ';'

assignments
  : assignment 
  | assignments ',' assignment 
  ;

assignment
  : lhs '=' rhs { $$.lhs = $1; $$.rhs = $3; driver->add_assignment(std::move($$)); }
  ;


// Should try to merge lhs & rhs definition
lhs
  : valid_name { $$.push_back(std::move($1)); }
  | valid_name '[' INTEGER ']' 
    { $$.emplace_back(verilog::NetBit(std::move($1), std::stoi($3.value))); }
  | valid_name '[' INTEGER ':' INTEGER ']' 
    { $$.emplace_back(verilog::NetRange(std::move($1), std::stoi($3.value), std::stoi($5.value))); }
  | lhs_concat { $$ = $1; }
  ;
 
lhs_concat
  : '{' lhs_exprs '}' { std::move($2.begin(), $2.end(), std::back_inserter($$)); }
  ; 

lhs_exprs 
  : lhs_expr { std::move($1.begin(), $1.end(), std::back_inserter($$)); }
  | lhs_exprs ',' lhs_expr 
    { 
      std::move($1.begin(), $1.end(), std::back_inserter($$));
      std::move($3.begin(), $3.end(), std::back_inserter($$));
    } 
  ;

lhs_expr 
  : valid_name { $$.push_back(std::move($1)); }
  | valid_name '[' INTEGER ']' 
    { $$.emplace_back(verilog::NetBit(std::move($1), std::stoi($3.value))); }
  | valid_name '[' INTEGER ':' INTEGER ']' 
    { $$.emplace_back(verilog::NetRange(std::move($1), std::stoi($3.value), std::stoi($5.value))); }
  | lhs_concat 
    { std::move($1.begin(), $1.end(), std::back_inserter($$)); }
  ;



constant
  : INTEGER  { $$=$1; }
  | BINARY { $$=$1; }
  | OCTAL { $$=$1; } 
  | DECIMAL { $$=$1; }
  | HEX { $$=$1; }
  | REAL { $$=$1; }
  | EXP  { $$=$1; }
  ;

rhs
  : valid_name { $$.emplace_back($1); }
  | valid_name '[' INTEGER ']' 
    { $$.emplace_back(verilog::NetBit(std::move($1), std::stoi($3.value))); }
  | valid_name '[' INTEGER ':' INTEGER ']' 
    { $$.emplace_back(verilog::NetRange(std::move($1), std::stoi($3.value), std::stoi($5.value))); } 
  | constant { $$.push_back(std::move($1)); }
  | rhs_concat { $$ = $1; }
  ;
 
rhs_concat
  : '{' rhs_exprs '}' { std::move($2.begin(), $2.end(), std::back_inserter($$)); }
  ; 

rhs_exprs 
  : rhs_expr { std::move($1.begin(), $1.end(), std::back_inserter($$)); }
  | rhs_exprs ',' rhs_expr 
    { 
      std::move($1.begin(), $1.end(), std::back_inserter($$));
      std::move($3.begin(), $3.end(), std::back_inserter($$));
    } 
  ;

rhs_expr 
  : valid_name { $$.push_back(std::move($1)); }
  | valid_name '[' INTEGER ']' 
    { $$.emplace_back(verilog::NetBit(std::move($1), std::stoi($3.value))); }
  | valid_name '[' INTEGER ':' INTEGER ']' 
    { $$.emplace_back(verilog::NetRange(std::move($1), std::stoi($3.value), std::stoi($5.value))); } 
  | constant { $$.push_back(std::move($1)); }
  | rhs_concat 
    { std::move($1.begin(), $1.end(), std::back_inserter($$)); }
  ;





instance 
  : valid_name valid_name '(' inst_pins ')' ';'
    { 
      std::swap($$.module_name, $1); 
      std::swap($$.inst_name, $2); 
      std::swap($$.pin_names, std::get<0>($4));  
      std::swap($$.net_names, std::get<1>($4));  
      driver->add_instance(std::move($$));
    }
  | valid_name parameters valid_name '(' inst_pins ')' ';'
    { 
      std::swap($$.module_name, $1); 
      std::swap($$.inst_name, $3); 
      std::swap($$.pin_names, std::get<0>($5));  
      std::swap($$.net_names, std::get<1>($5));  
      driver->add_instance(std::move($$));
    }
  ;

inst_pins 
  : { } // empty
  | nets_by_position { std::swap(std::get<1>($$), $1); }
  | nets_by_name 
    {
      std::swap(std::get<0>($$), std::get<0>($1));
      std::swap(std::get<1>($$), std::get<1>($1));
    }
  ;

nets_by_position
  : rhs { $$.emplace_back(std::move($1)); }
  | nets_by_position ',' rhs
    { 
      std::move($1.begin(), $1.end(), std::back_inserter($$));   
      $$.push_back(std::move($3));
    }
  ;
  

nets_by_name 
  : net_by_name 
    { 
      std::get<0>($$).push_back(std::move(std::get<0>($1))); 
      std::get<1>($$).push_back(std::move(std::get<1>($1))); 
    }
  | nets_by_name ',' net_by_name 
    { 
      auto &pin_names = std::get<0>($1);
      auto &net_names = std::get<1>($1);
      std::move(pin_names.begin(), pin_names.end(), std::back_inserter(std::get<0>($$)));
      std::move(net_names.begin(), net_names.end(), std::back_inserter(std::get<1>($$)));

      std::get<0>($$).push_back(std::move(std::get<0>($3))); 
      std::get<1>($$).push_back(std::move(std::get<1>($3))); 
    }
  ;


net_by_name
  : '.' valid_name '(' ')' 
    { std::get<0>($$) = $2; } 
  | '.' valid_name '(' valid_name ')' 
    { 
       std::get<0>($$) = $2; 
       std::get<1>($$).push_back(std::move($4)); 
    } 
  | '.' valid_name '(' valid_name '[' INTEGER ']' ')' 
    { 
      std::get<0>($$) = $2; 
      std::get<1>($$).emplace_back(verilog::NetBit(std::move($4), std::stoi($6.value))); 
    } 
  // The previous two rules are also in rhs. But I don't want to create special rule just for this case 
  | '.' valid_name '(' rhs ')' 
    { 
      std::get<0>($$) = $2; 
      std::get<1>($$) = $4; 
    }
  // Bus port bit 
  | '.' valid_name '[' INTEGER ']' '(' ')'
    {
      std::get<0>($$) = verilog::NetBit(std::move($2), std::stoi($4.value)); 
    }
  | '.' valid_name '[' INTEGER ']' '(' rhs ')'
    {
      std::get<0>($$) = verilog::NetBit(std::move($2), std::stoi($4.value)); 
      std::get<1>($$) = $7; 
    }
  // Bus port part 
  | '.' valid_name '[' INTEGER ':' INTEGER ']' '(' ')'
    {
      std::get<0>($$) = verilog::NetRange(std::move($2), std::stoi($4.value), std::stoi($6.value)); 
    }
  | '.' valid_name '[' INTEGER ':' INTEGER ']' '(' rhs ')'
    {
      std::get<0>($$) = verilog::NetRange(std::move($2), std::stoi($4.value), std::stoi($6.value)); 
      std::get<1>($$) = $9; 
    }
  ;


// parameters are ignored for now
parameters 
  : '#' '(' expr_list ')'
  ;


initial_construct
  : INITIALKEY block_statement
  ;

always_construct
  : ALWAYS block_statement
  ;

optional_range
  :
    { }
  | range_expr
    {$$ = std::move($1);}
  ;

range_expr
	: '[' expr ']'
    {
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| '[' expr ':' expr ']'
    {
      $$.leftOperand = std::make_shared<verilog::Expression>(std::move($2));
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($4));
    }
	;

expr
  : primary
    {$$ = std::move($1);}
  | expr binary_operator expr
		{
      $$.type = verilog::ExpressionType::BINARYOP;
      $$.op = std::move($2);
      $$.leftOperand = std::make_shared<verilog::Expression>(std::move($1));
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($3));
    }
  | '-' expr %prec UMINUS
    {
      $$.type = verilog::ExpressionType::UNARYOP;
      $$.op = "-";
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| LNOT expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::UNARYOP;
      $$.op = std::move($1);
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BAND expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::NONE;
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BOR expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::NONE;
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BNAND expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::NONE;
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BNOR expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::NONE;
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BXOR expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::NONE;
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BXNOR expr %prec UNARY
    {
      $$.type = verilog::ExpressionType::NONE;
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| BNOT expr %prec UNARY
		{
      $$.type = verilog::ExpressionType::UNARYOP;
      $$.op = std::move($1);
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
	| '+' expr %prec UNARY
		{
      $$.type = verilog::ExpressionType::UNARYOP;
      $$.op = "+";
      $$.rightOperand = std::make_shared<verilog::Expression>(std::move($2));
    }
    // | expr '?' expr ':' expr
  ;

primary
  : constant
    {
      if($1.type == verilog::ConstantType::BINARY
        || $1.type == verilog::ConstantType::OCTAL
        || $1.type == verilog::ConstantType::DECIMAL
        || $1.type == verilog::ConstantType::HEX)
        $$.isBitString = true;
      $$.string = $1.value;
      $$.type = verilog::ExpressionType::CONSTANT;
    }
  | hierarchical_identifier optional_range
    {
      $$.string = $1;
      $$.leftOperand = std::move($2.leftOperand);
      $$.rightOperand = std::move($2.rightOperand);
      $$.type = verilog::ExpressionType::IDENTIFIER;
    }
  | concatenation
    {
      $$.type = verilog::ExpressionType::CONCATENATION;
    }
  | multiple_concatenation
    {
      $$.type = verilog::ExpressionType::MULTICONCATENATION;
    }
  | function_call
    {
      $$.string = $1;
      $$.type = verilog::ExpressionType::FUNCTIONCALL;
    }
  | system_function_call
    {
      $$.string = $1;
      $$.type = verilog::ExpressionType::SYSTEMFUNCTIONCALL;
    }
  | '(' expr ')'
    {$$ = std::move($2);}
  | STRING
    {
      $$.string = $1;
      $$.type = verilog::ExpressionType::STRING;
    }
  ;

hierarchical_identifier
	: hierarchical_identifier '.' valid_name
    {
      $1 += $3;
      $$ = $1;
    }
	| valid_name
    {$$ = $1;}
	;

concatenation
  : '{' expr_list '}'
    {$$ = std::move($2);}
  ;

expr_list
  : expr_list ',' expr
    {$1.emplace_back(std::move($3));
    $$ = std::move($1);}
  | expr
    {$$.emplace_back(std::move($1));}
  ;

multiple_concatenation
  : '{' expr concatenation '}'
  ;

function_call
  : hierarchical_identifier '(' expr_list ')'
    {$$ = $1;}
  ;

system_function_call
  : SYSTASKFUNC
    {$$ = $1;}
	| SYSTASKFUNC '(' expr_list ')'
    {$$ = $1;}
	;

  binary_operator
  : '+'
    {
      $$ = "+";
    }
  | '-'
    {
      $$ = "-";
    }
  | '*'
    {
      $$ = "*";
    }
  | '/'
    {
      $$ = "/";
    }
  | '%'
    {
      $$ = "%";
    }
  | EQ
		{
      $$ = std::move($1);
    }
	| NE
		{
      $$ = std::move($1);
    }
	| GT
		{
      $$ = std::move($1);
    }
	| GE
		{
      $$ = std::move($1);
    }
	| LT
		{
      $$ = std::move($1);
    }
	| LE
		{
      $$ = std::move($1);
    }
	| SL
		{
      $$ = std::move($1);
    }
	| SR
		{
      $$ = std::move($1);
    }
	| LAND
		{
      $$ = std::move($1);
    }
	| LOR
		{
      $$ = std::move($1);
    }
	| BAND
		{
      $$ = std::move($1);
    }
	| BXOR
		{
      $$ = std::move($1);
    }
	| BOR
		{
      $$ = std::move($1);
    }
  ;


%%

void verilog::VerilogParser::error(const location_type &l, const std::string &err_message) {
  std::cerr << "Parser error: " << err_message  << '\n'
            << "  begin at line " << l.begin.line <<  " col " << l.begin.column  << '\n' 
            << "  end   at line " << l.end.line <<  " col " << l.end.column << "\n";
  std::abort();
}


