#ifndef DATA_VERILOG_HPP_
#define DATA_VERILOG_HPP_

#include <string>
#include <cstddef>
#include <fstream>
#include <iostream>
#include <variant>
#include <optional>
#include <experimental/filesystem>
#include <boost/variant.hpp>


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

  struct Instance {
    std::string module_name;
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

  struct Expression {
    bool isBitString = false;
    std::string string; // expression as string
    std::string beg;
    std::string end;
    std::string op;
    std::shared_ptr<Expression> leftOperand{nullptr};
    std::shared_ptr<Expression> rightOperand{nullptr}; // for unary operations only right operand is set
    ExpressionType type{ExpressionType::NONE};
  };

  inline std::ostream& operator<<(std::ostream& os, const Expression& expr) {
    os << "Expression: " << expr.string;
    os << ", range: " << expr.beg << " : " << expr.end << '\n';
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
    std::string beg; // expression contained as string
    std::string end; // expression contained as string
    VarType type;
    Expression rval;
  };

  inline std::ostream& operator<<(std::ostream& os, const Var& var) {
    os << var.name << '\n';
    os << "beg: " << var.beg << "  end: " << var.end << '\n';
    os << "type: " << var.type << '\n' << "rval: " << var.rval << '\n';
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
  struct CaseStatement;
  struct ConditionalStatement;
  struct WhileStatement;
  struct ForStatement;
  struct RepeatStatement;
  struct WaitStatement;
  struct EventControl;

  // Block contains statements, statement can be a block itself
  using Statement = boost::variant<std::string, Block, BlockingAssignment,
                                   boost::recursive_wrapper<CaseStatement>,
                                   boost::recursive_wrapper<ConditionalStatement>,
                                   boost::recursive_wrapper<WhileStatement>,
                                   boost::recursive_wrapper<ForStatement>,
                                   boost::recursive_wrapper<WaitStatement>,
                                   boost::recursive_wrapper<EventControl>,
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

  struct EventControl {
    EventControl(std::string _string, Statement _stmt)
    : string(std::move(_string)), statement(std::move(_stmt))
    {}

    std::string string;
    Statement statement;
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
          std::cout << obj.lval << '\n';
          std::cout << obj.rval << '\n';
      }

      void operator()(const CaseStatement &obj) const
      {
          std::cout << obj.expr << '\n';
      }

      void operator()(const ConditionalStatement &obj) const
      {
        std::cout << "ConditionalStatement\n";
      }

      void operator()(const WhileStatement &obj) const
      {
        std::cout << "while expr: " << obj.condition << '\n';
        std::cout << "while block: " << '\n';
        boost::apply_visitor(printer(), obj.statement);
      }

      void operator()(const ForStatement &obj) const
      {
        std::cout << "for statement\n";
      }

      void operator()(const RepeatStatement &obj) const
      {
        std::cout << "repeat expr: " << obj.expr << '\n';
        boost::apply_visitor(printer(), obj.statement);
      }

      void operator()(const WaitStatement &obj) const
      {
        std::cout << "wait expr: " << obj.expr << '\n';
        boost::apply_visitor(printer(), obj.statement);
      }

      void operator()(const EventControl &obj) const
      {
        std::cout << "event expr: " << obj.string << '\n';
        boost::apply_visitor(printer(), obj.statement);
      }

      void operator()(const Block &obj) const
      {
        std::cout << obj.name << '\n';

        std::cout << "Declarations:\n";
        for(const auto& n: obj.vars)
          std::cout << "Var decl: " << n << '\n';
        for(const auto& n: obj.parameters)
          std::cout << "Param decl: " << n << '\n';

        if(!obj.statements.empty()){
          std::cout << "Statements: {\n";
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
    for(const auto& n: task.vars)
      std::cout << "Var decl: " << n << '\n';
    for(const auto& n: task.parameters)
      std::cout << "Param decl: " << n << '\n';

    os << "Statements: {\n";
    boost::apply_visitor(printer(), task.statement);
    os << "}\n";

    return os;
  }

}
#endif


