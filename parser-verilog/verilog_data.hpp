#ifndef DATA_VERILOG_HPP_
#define DATA_VERILOG_HPP_

#include <string>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <variant>
#include <boost/variant.hpp>
#include <vector>


namespace verilog {

  enum class ConstantType {
    NONE,
    INTEGER,
    BINARY,
    OCTAL, 
    DECIMAL,
    HEX, 
    REAL,
    EXP
  };

  inline std::ostream& operator<<(std::ostream& os, const ConstantType& t) {  
    switch(t) {
      case ConstantType::NONE:    os << "NONE";    break; 
      case ConstantType::INTEGER: os << "INTEGER"; break;
      case ConstantType::BINARY:  os << "BINARY";  break; 
      case ConstantType::OCTAL:   os << "OCTAL";   break; 
      case ConstantType::DECIMAL: os << "DECIMAL"; break;
      case ConstantType::HEX:     os << "HEX";     break; 
      case ConstantType::REAL:    os << "REAL";    break;
      case ConstantType::EXP:     os << "EXP";     break; 
    }
    return os;  
  }  
  
  struct Constant {
    Constant() = default;  // Need this default constructor for return token
    Constant(std::string&& v, ConstantType t) : value(std::move(v)), type(t) {}
    std::string value;
    ConstantType type {ConstantType::NONE};
  };

  inline std::ostream& operator<<(std::ostream& os, const Constant& c) {  
    std::cout << " constant value: " << c.value << " type: " << c.type;
    return os;  
  }  

  enum class PortDirection {
    INPUT,
    OUTPUT,
    INOUT
  };

  inline std::ostream& operator<<(std::ostream& os, const PortDirection& dir) {  
    switch(dir){
      case PortDirection::INPUT:  os << "INPUT";  break; 
      case PortDirection::OUTPUT: os << "OUTPUT"; break;
      case PortDirection::INOUT:  os << "INOUT";  break; 
    }
    return os;  
  }  
    
  enum class ConnectionType {
    NONE,
    WIRE,
    REG
  };

  inline std::ostream& operator<<(std::ostream& os, const ConnectionType& ct) {  
    switch(ct){
      case ConnectionType::NONE:  os << "NONE"; break;
      case ConnectionType::WIRE:  os << "WIRE"; break;
      case ConnectionType::REG:   os << "REG";  break; 
    }
    return os;  
  } 

  struct Port {
    std::vector<std::string> names;
    int beg {-1};
    int end {-1};
    PortDirection dir;
    ConnectionType type {ConnectionType::NONE};
  };

  inline std::ostream& operator<<(std::ostream& os, const Port& port) {  
    os << "beg: " << port.beg << "  end: " << port.end << '\n';
    os << "Dir: " << port.dir << "  type: " << port.type << '\n';
    for(const auto& n: port.names){
      os << n << '\n';
    }
    return os;  
  }

  enum class NetType {
    NONE,
    WIRE, 
    WAND,
    WOR,
    TRI,
    TRIOR,
    TRIAND,
    SUPPLY0,
    SUPPLY1
  };

  inline std::ostream& operator<<(std::ostream& os, const NetType& t) {  
    switch(t){
      case NetType::NONE:    os << "NONE";    break; 
      case NetType::WIRE:    os << "WIRE";    break;
      case NetType::WAND:    os << "WAND";    break;
      case NetType::WOR:     os << "WOR";     break;
      case NetType::TRI:     os << "TRI";     break;
      case NetType::TRIAND:  os << "TRIAND";  break;
      case NetType::TRIOR:   os << "TRIOR";   break;
      case NetType::SUPPLY0: os << "SUPPLY0"; break;
      case NetType::SUPPLY1: os << "SUPPLY1"; break;
    }
    return os;  
  } 

  struct Net {
    std::vector<std::string> names;
    int beg {-1};
    int end {-1};
    NetType type {NetType::NONE};
  };

  inline std::ostream& operator<<(std::ostream& os, const Net& net) {  
    os << "beg: " << net.beg << "  end: " << net.end << '\n';
    os << "type: " << net.type << '\n';
    for(const auto& n: net.names){
      os << n << '\n';
    }
    return os;  
  }

  struct NetBit {
    NetBit(std::string&& n, int b): name(std::move(n)), bit(b) {}
    std::string name;
    int bit {-1};
  };

  inline std::ostream& operator<<(std::ostream& os, const NetBit& n) {  
    os << n.name << '[' << n.bit << "] ";
    return os;
  }

  struct NetRange {
    NetRange(std::string&& n, int b, int e): name(std::move(n)), beg(b), end(e) {}
    std::string name;
    int beg {-1};
    int end {-1};
  };

  inline std::ostream& operator<<(std::ostream& os, const NetRange& n) {  
    os << n.name << '[' << n.beg << ':' << n.end << "] ";
    return os;
  }
  
  struct Assignment {
    // Left hand side can be: a wire, a bit in a wire, a part of a wire  
    std::vector<std::variant<std::string, NetBit, NetRange>> lhs;

    // Right hand side can be: a wire, a bit in a wire, a part of a wire, a constant
    std::vector<std::variant<std::string, NetBit, NetRange, Constant>> rhs;
  };

  inline std::ostream& operator<<(std::ostream& os, const Assignment& ast) {  
    os << "LHS: ";
    for(const auto& l: ast.lhs){
      switch(l.index()){
        case 0: os << std::get<0>(l) << ' '; break;
        case 1: os << std::get<1>(l).name << '/' << std::get<1>(l).bit << ' '; 
                break;
        case 2: os << std::get<2>(l).name << '/' << std::get<2>(l).beg << '/' << std::get<2>(l).end << ' '; 
                break;
      }
    }
    os << '\n';
    os << "RHS: ";
    for(const auto& r: ast.rhs){
      switch(r.index()){
        case 0: os << std::get<0>(r) << ' '; break;
        case 1: os << std::get<1>(r).name << '/' << std::get<1>(r).bit << ' '; 
                break;
        case 2: os << std::get<2>(r).name << '/' << std::get<2>(r).beg << '/' << std::get<2>(r).end << ' '; 
                break;
        case 3: os << std::get<3>(r) << ' '; break;
      }
    }

    return os;  
  }

  using NetConcat = std::variant<std::string, NetBit, NetRange, Constant>;
  struct Expression;

  struct Instance {
    std::string module_name;
    std::vector<std::pair<std::string, Expression>> parameters;
    std::string inst_name;
  
    // pin_names might be empty. e.g. my_module m1(net1, net2);
    std::vector<std::variant<std::string, NetBit, NetRange>> pin_names;
    std::vector<std::vector<NetConcat>> net_names;
  };

  inline std::ostream& operator<<(std::ostream& os, const Instance& inst) {  
    os << inst.module_name << ' ' << inst.inst_name << '(';

    if(!inst.pin_names.empty()){
      for(size_t i=0; i<inst.pin_names.size(); i++){
        std::visit([](const auto& name){ std::cout << name << ' '; }, inst.pin_names[i]);
        std::cout << '(' ;
        if(inst.net_names[i].size() > 1){
          std::cout <<  '{';
        }
        for(const auto& v: inst.net_names[i]){
          std::visit([](const auto& v){ std::cout << v << ' '; }, v);
        }
        if(inst.net_names[i].size() > 1){
          std::cout <<  '}';
        }
        std::cout << ") " ;
      }
    }
    else{
      for(size_t i=0; i<inst.net_names.size(); i++){
        if(inst.net_names[i].size() > 1){
          std::cout <<  '{';
        }
        for(const auto& v: inst.net_names[i]){
          std::visit([](const auto& v){ std::cout << v << ' '; }, v);
        }
        if(inst.net_names[i].size() > 1){
          std::cout <<  '}';
        }
        std::cout << ',';
      }
    }
    os << ')';
    return os;
  }

  enum class ExpressionType {
    NONE,
    CONSTANT,
    IDENTIFIER,
    CONCATENATION,
    MULTICONCATENATION,
    FUNCTIONCALL,
    SYSTEMFUNCTIONCALL,
    STRING,

    UNARYOP,
    BINARYOP
  };

  inline std::ostream& operator<<(std::ostream& os, const ExpressionType& t) {
    switch(t){
      case ExpressionType::NONE:                os << "NONE";               break;
      case ExpressionType::CONSTANT:            os << "CONSTANT";           break;
      case ExpressionType::IDENTIFIER:          os << "IDENTIFIER";         break;
      case ExpressionType::CONCATENATION:       os << "CONCATENATION";      break;
      case ExpressionType::MULTICONCATENATION:  os << "MULTICONCATENATION"; break;
      case ExpressionType::FUNCTIONCALL:        os << "FUNCTIONCALL";       break;
      case ExpressionType::SYSTEMFUNCTIONCALL:  os << "SYSTEMFUNCTIONCALL"; break;
      case ExpressionType::STRING:              os << "STRING";             break;
      case ExpressionType::UNARYOP:             os << "UNARYOP";            break;
      case ExpressionType::BINARYOP:            os << "BINARYOP";           break;
    }
    return os;
  }

  struct Expression {
    bool isBitString = false;
    std::string string; // expression as string
    std::string op;
    // variables hold lval and rval in case of BINARYOP
    // or msb and lsb of range-slice (optionally in case of IDENTIFIER)
    // for unary operations only right operand is set
    // as well as for indexing
    std::shared_ptr<Expression> leftOperand{nullptr};
    std::shared_ptr<Expression> rightOperand{nullptr};
    ExpressionType type{ExpressionType::NONE};
  };

  inline std::ostream& operator<<(std::ostream& os, const Expression& expr) {
    os << "Expression: " << expr.type << ": ";
    switch (expr.type) {
    case verilog::ExpressionType::CONSTANT:
      os << expr.string;
      break;
    case verilog::ExpressionType::IDENTIFIER:
      if (expr.leftOperand)
        os << expr.string << '[' << (*expr.leftOperand) << ':' << (*expr.rightOperand) << ']';
      else if (expr.rightOperand)
        os << expr.string << '[' << (*expr.rightOperand) << ']';
      else
        os << expr.string;
      break;
    case verilog::ExpressionType::BINARYOP:
      os << (*expr.leftOperand) << expr.op << (*expr.rightOperand);
      break;
    case verilog::ExpressionType::UNARYOP:
      os << expr.op << (*expr.rightOperand);
      break;
    case verilog::ExpressionType::STRING:
      os << expr.string;
      break;
    default:
      os << std::string();
    }
    return os;
  }

  enum class VarType {
    REG,
    LOGIC,
    INTEGERKEY
  };

  inline std::ostream& operator<<(std::ostream& os, const VarType& t) {
    switch(t){
      case VarType::REG:        os << "REG";        break;
      case VarType::LOGIC:      os << "LOGIC";      break;
      case VarType::INTEGERKEY: os << "INTEGERKEY"; break;
    }
    return os;
  }

  struct Var {
    std::string name;
    Expression range;
    VarType type;
    Expression rval;
  };

  inline std::ostream& operator<<(std::ostream& os, const Var& var) {
    if (var.range.leftOperand)
      os << '[' << (*var.range.leftOperand) << ':' << (*var.range.rightOperand) << "] ";
    os << var.name << '\n';
    os << "type: " << var.type << '\n';
    os << "rval: " << var.rval << '\n';
    return os;
  }

  struct Parameter {
    std::string name;
    Expression rval;
  };

  inline std::ostream& operator<<(std::ostream& os, const Parameter& param) {
    os << "name: " << param.name << '\n';
    os << "rval: " << param.rval << '\n';
    return os;
  }

  struct Block;

  struct BlockingAssignment;
  struct TaskEnable;
  struct CaseStatement;
  struct ConditionalStatement;
  struct WhileStatement;
  struct ForStatement;
  struct RepeatStatement;
  struct WaitStatement;
  struct TimingControl;

  // Block contains statements, statement can be a block itself
  using Statement = boost::variant<std::string, Block, BlockingAssignment, TaskEnable,
                                   boost::recursive_wrapper<CaseStatement>,
                                   boost::recursive_wrapper<ConditionalStatement>,
                                   boost::recursive_wrapper<WhileStatement>,
                                   boost::recursive_wrapper<ForStatement>,
                                   boost::recursive_wrapper<WaitStatement>,
                                   boost::recursive_wrapper<TimingControl>,
                                   boost::recursive_wrapper<RepeatStatement>
                                  >;

  struct Block {
    std::string name;
    std::vector<Parameter> parameters;
    std::vector<Var> vars;
    std::vector<Statement> statements;
  };

  struct BlockingAssignment {
    std::string timeControl;
    Expression lval;
    Expression rval;
  };

  struct TaskEnable {
    bool isSystem = false;
    std::string name;
    std::vector<Expression> args;
  };

  struct ConditionalStatement {
    Expression ifCondition;
    Statement ifStatement;
    std::vector<std::pair<Expression, Statement>> elifs;
    Statement elseStatement;
  };

  struct CaseItem {
    bool isDefault = false;
    std::vector<Expression> expressions;
    Statement statement;
  };

  enum class CaseType {
    CASE,
    CASEX,
    CASEZ
  };

  inline std::ostream& operator<<(std::ostream& os, const CaseType& t) {
    switch(t){
      case CaseType::CASE:  os << "CASE";   break;
      case CaseType::CASEX: os << "CASEX";  break;
      case CaseType::CASEZ: os << "CASEZ";  break;
    }
    return os;
  }

  struct CaseStatement {
    Expression expr;
    std::vector<CaseItem> items;
    CaseType type;
  };

  struct WhileStatement {
    Expression condition;
    Statement statement;
  };

  struct ForStatement {
    std::pair<Expression, Expression> initAssign; // left and right values of counter assignment
    Expression condition;
    std::pair<Expression, Expression> iterAssign; // counter update is performed only through assignment in Verilog
    Statement statement;
  };

  struct RepeatStatement {
    Expression expr;
    Statement statement;
  };

  struct WaitStatement {
    Expression expr;
    Statement statement;
  };

  enum class ControlType {
    DELAY,
    EVENT
  };

  inline std::ostream& operator<<(std::ostream& os, const ControlType& t) {
    switch(t){
      case ControlType::DELAY:  os << "DELAY";  break;
      case ControlType::EVENT:  os << "EVENT";  break;
    }
    return os;
  }

  struct TimingControl {
    TimingControl(std::string _control, Statement _stmt, ControlType _type)
    : control(std::move(_control)), statement(std::move(_stmt)), type(std::move(_type))
    {}

    std::string control;
    Statement statement;
    ControlType type;
  };


  class printer : public boost::static_visitor<>
  {
  public:

    void operator()(const std::string &obj) const
    {
      std::cout << obj << '\n';
    }

    void operator()(const BlockingAssignment &obj) const
    {
      std::cout << "Blocking Assignment:\n";
      std::cout << obj.lval << '\n';
      std::cout << obj.rval << '\n';
    }

    void operator()(const TaskEnable &obj) const
    {
      if (obj.isSystem)
        std::cout << "System ";
      std::cout << "Task Enabling:\n";
      std::cout << obj.name << "(\n";
      std::cout << "arguments:\n";
      for (const Expression &expr : obj.args)
        std::cout << expr;
      std::cout << "\n)";
    }

    void operator()(const CaseStatement &obj) const
    {
      std::cout << "Case Statement:\n";
      std::cout << obj.type << '(' << obj.expr << ')' << '\n';
      for (const CaseItem &item : obj.items) {
        if (item.isDefault) {
          std::cout << "default: ";
          boost::apply_visitor(printer(), item.statement);
        } else {
          for (const Expression &expr : item.expressions)
            std::cout << expr;
          std::cout << ":\n";
          boost::apply_visitor(printer(), item.statement);
        }
      }
    }

    void operator()(const ConditionalStatement &obj) const
    {
      std::cout << "Conditional Statement:\n";
      std::cout << "if (" << obj.ifCondition << ")\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.ifStatement);
      for (const auto &[expr, statement] : obj.elifs) {
        std::cout << "else if (" << expr << ")\n";
        std::cout << "    ";
        boost::apply_visitor(printer(), statement);
      }
      std::cout << "else\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.elseStatement);
    }

    void operator()(const WhileStatement &obj) const
    {
      std::cout << "While Statement:\n";
      std::cout << "while (" << obj.condition << ")\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.statement);
    }

    void operator()(const ForStatement &obj) const
    {
      std::cout << "For Statement:\n";
      std::cout << "for (" << obj.initAssign.first << '=' << obj.initAssign.second << ';'
                           << obj.condition << ';'
                           << obj.iterAssign.first << '=' << obj.iterAssign.second << ")\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.statement);
    }

    void operator()(const RepeatStatement &obj) const
    {
      std::cout << "Repeat Statement:\n";
      std::cout << "repeat(" << obj.expr << ")\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.statement);
    }

    void operator()(const WaitStatement &obj) const
    {
      std::cout << "Wait Statement:\n";
      std::cout << "wait(" << obj.expr << ")\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.statement);
    }

    void operator()(const TimingControl &obj) const
    {
      std::cout << "Timing Control Statement:\n";
      std::cout << obj.type << ' ' << obj.control << "\n";
      std::cout << "    ";
      boost::apply_visitor(printer(), obj.statement);
    }

    void operator()(const Block &obj) const
    {
      std::cout << obj.name << '\n';

      std::cout << "Block Declarations:\n";
      for(const auto &var: obj.vars)
        std::cout << "Var decl: " << var << '\n';
      for(const auto &param: obj.parameters)
        std::cout << "Param decl: " << param << '\n';

      if(!obj.statements.empty()){
        std::cout << "Block Statements: {\n";
        for(const auto& statement: obj.statements)
          boost::apply_visitor(printer(), statement);
        std::cout << "}\n";
      }
    }
  };

  struct Task {
    std::string name;
    std::vector<Expression> args;
    std::vector<Parameter> parameters;
    std::vector<Var> vars;
    Statement statement;
  };

  inline std::ostream& operator<<(std::ostream& os, const Task& task) {
    std::cout << task.name << '\n';
    std::cout << "Task args: " << '\n';
    for (const auto &arg: task.args)
      std::cout << arg;

    std::cout << "Task Declarations:\n";
    for(const auto &var: task.vars)
      std::cout << "Var decl: " << var << '\n';
    for(const auto &param: task.parameters)
      std::cout << "Param decl: " << param << '\n';

    os << "Task Statements: {\n";
    boost::apply_visitor(printer(), task.statement);
    os << "}\n";

    return os;
  }

}
#endif


