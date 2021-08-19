#include <iostream>

#include "verilog_driver.hpp"

#include <boost/format.hpp>
#include <botan/bigint.h>

#include <bitset>
#include <charconv>

inline std::ostream &indent(std::ostream &out)
{
    return out << "    ";
}

struct Module
{
    Module() = default;

    Module(std::string name)
        : name(std::move(name))
    {
    }

    std::string name;
    std::vector<verilog::Net> nets;
    std::vector<verilog::Instance> instances;

    std::vector<verilog::Var> variables;
    std::vector<verilog::Parameter> parameters;
    std::vector<verilog::Task> tasks;
};


// A custom parser struct
struct SampleParser : public verilog::ParserVerilogInterface {
  virtual ~SampleParser(){}

  Module testbenchModule;

  void add_module(std::string&& name){
    if (!testbenchModule.name.empty())
      throw std::runtime_error("Testbench file should have only one module");
    testbenchModule.name = std::move(name);
  }

  void add_port(verilog::Port&& port) {
    std::cout << "Port: " << port << '\n';
  }

  void add_net(verilog::Net&& net) {
    std::cout << "Net: " << net << '\n';
    if (net.beg == -1) { // 1-bit width net
        net.beg = 0;
        net.end = 0;
    }
    testbenchModule.nets.push_back(std::move(net));
  }

  void add_assignment(verilog::Assignment&& ast) {
    std::cout << "Assignment: " << ast << '\n';
  }

  void add_instance(verilog::Instance&& inst) {
    testbenchModule.instances.emplace_back(std::move(inst));
  }

  void add_var(verilog::Var&& var) {
    testbenchModule.variables.emplace_back(std::move(var));
  }

  void add_parameter(verilog::Parameter&& param) {
    testbenchModule.parameters.emplace_back(std::move(param));
  }

  void add_task(verilog::Task&& task) {
    testbenchModule.tasks.emplace_back(task);
  }

};

static std::string toBinaryStr(std::string_view pinArgName)
  {
      // Only such form of constants is analyzed correctly: [0-9]*'[b|h|d][0-9a-f]+
      // But there are more cases that are allowed by Verilog syntax
      // See chapter 3.5 Numbers in Verilog HDL IEEE.1364-2005
      std::string binaryChars;
      size_t numeralSys;
      // Remove underscores in bitstring beforehand
      if (numeralSys = pinArgName.find('h'); numeralSys != std::string::npos) {
          for (size_t i = numeralSys + 1; i < pinArgName.size(); ++i) {
              if (pinArgName[i] == 'x') {
                      binaryChars += "xxxx";
              } else if (pinArgName[i] == 'z') {
                  binaryChars += "zzzz";
              } else {
                  int value;
                  auto [position, errorCode] = std::from_chars(pinArgName.data() + i, pinArgName.data() + i + 1, value, 16);
                  if (errorCode != std::errc())
                      throw std::system_error(std::make_error_code(errorCode));
                  binaryChars += std::bitset<4>(value).to_string();
              }
          }
      } else if (numeralSys = pinArgName.find('d'); numeralSys != std::string::npos) {
          std::string_view decStr = pinArgName.substr(numeralSys + 1);
          Botan::BigInt dec = Botan::BigInt(decStr.data());
          binaryChars.reserve(decStr.size());
          for (size_t bitIdx = 0; bitIdx < dec.bits(); ++bitIdx)
              binaryChars += (dec.get_bit(bitIdx) ? '1' : '0');
      } else if (numeralSys = pinArgName.find('b'); numeralSys != std::string::npos) {
          binaryChars = pinArgName.substr(numeralSys + 1);
      } else {
          throw std::runtime_error(str(boost::format("ERROR while processing constant: %1%\n") % pinArgName));
      }

      size_t constantBitNum; // The actual number of bits in constant
      auto [position, errorCode] = std::from_chars(pinArgName.data(), pinArgName.data() + numeralSys - 1, constantBitNum);
      if (errorCode != std::errc())
          throw std::system_error(std::make_error_code(errorCode));

      // Align with the actual number of bits if necessary (prepend or cut)
      if (binaryChars.size() > constantBitNum)
          binaryChars.erase(0, binaryChars.size() - constantBitNum);
      else if (binaryChars.size() < constantBitNum)
          binaryChars.insert(0, constantBitNum - binaryChars.size(), '0');

      return binaryChars;
  }

inline std::string findPortByNet(std::vector<verilog::Instance>::const_iterator dut, const std::string &netName)
{
  for (size_t pinIdx = 0; pinIdx < dut->pin_names.size(); ++pinIdx) {
    verilog::NetConcat pinArg = dut->net_names[pinIdx][0];
    if (std::holds_alternative<std::string>(pinArg)) {
      std::string pinArgName = std::get<std::string>(pinArg);
      if (pinArgName == netName && std::holds_alternative<std::string>(dut->pin_names[pinIdx]))
        return std::get<std::string>(dut->pin_names[pinIdx]);
    }
  }
  throw std::runtime_error(netName + " net was not found in DUT connections.");
}

/*
  Class builds C++ code out of Verilog parsed `Statement` structures.
  Naturally, these structures are recursive, so translator creates code
  in recursive manner as well.
*/
class translator : public boost::static_visitor<std::string>
{
  public:
    translator() = default;
    translator(const std::vector<std::string> &taskArgs,
               std::vector<verilog::Instance>::const_iterator dut,
               const size_t indentLevel = 1)
    : m_taskArgs(taskArgs)
    , m_dut(dut)
    , m_indent(std::string(indentLevel * 4, ' '))
    , m_indentLevel(indentLevel)
    {}

    std::string operator()(const std::string &obj) const
    {
        return obj;
    }

    std::string operator()(const verilog::BlockingAssignment &obj) const
    {
      // Handle pragma `fiq secretdata` and `fiq userdata`
      auto it = std::find(m_taskArgs.begin(), m_taskArgs.end(), obj.rval.string);
      if (it != m_taskArgs.end()) {
        int index = it - m_taskArgs.begin();
        if (index == 0)
          return m_indent + "setData(" + obj.lval.string + ", m_keyBits);\n";
        if (index == 1)
          return m_indent + "setData(" + obj.lval.string + ", m_inputDataBits);\n";
      }

      // Handle pragma `fiq resultdata`
      it = std::find(m_taskArgs.begin(), m_taskArgs.end(), obj.lval.string);
      if (it != m_taskArgs.end())
        return std::string();

      // Handle bitstring assignments
      if (obj.rval.isBitString) {
        const std::string binaryChars = toBinaryStr(obj.rval.string);
        if (binaryChars.size() == 1)
          return m_indent + obj.lval.string
                 + (binaryChars[0] == '1' ? ".setValue(true);\n" : ".setValue(false);\n");
        return "bistring vector handling here";
      }

      // Simple(integer, real, exp) value assignment
      return m_indent + obj.rval.string + " = " + obj.lval.string + ";\n";
    }

    std::string operator()(const verilog::ConditionalStatement &obj) const
    {
      return m_indent + std::string();
    }

    std::string operator()(const verilog::WhileStatement &obj) const
    {
      return m_indent +
             boost::apply_visitor(translator(m_taskArgs, m_dut, m_indentLevel + 1), obj.while_block);
    }

    std::string operator()(const verilog::ForStatement &obj) const
    {
      return m_indent +
             boost::apply_visitor(translator(m_taskArgs, m_dut, m_indentLevel + 1), obj.for_block);
    }

    std::string operator()(const verilog::RepeatStatement &obj) const
    {
      std::string code = "for (int i=0; i < " + obj.expr.string + "; ++i) {\n";
      code += boost::apply_visitor(translator(m_taskArgs, m_dut, m_indentLevel + 1), obj.stmt);
      code += m_indent + "}\n";
      return m_indent + code;
    }

    std::string operator()(const verilog::WaitStatement &obj) const
    {
      std::string code = "do {\n" + std::string((m_indentLevel + 1) * 4, ' ') + "moveclock(true);\n";
      code += boost::apply_visitor(translator(m_taskArgs, m_dut, m_indentLevel + 1), obj.statement);

      code += m_indent + "} while ";
      if (obj.expr.isBitString) {
        const std::string binaryChars = toBinaryStr(obj.expr.string);
        if (binaryChars.size() == 1) {
          std::string base = str(boost::format("(m_topModule->output(%1%::output_%2%))")
                                 % m_dut->module_name % findPortByNet(m_dut, obj.expr.left_op));
          binaryChars[0] == '1' ? code += "(!" + base + ");\n" : (code += base + ";\n");
        }
      }
      return m_indent + code;
    }

    std::string operator()(const verilog::EventControl &obj) const
    {
      std::string code = "moveclock(true);\n";
      code += boost::apply_visitor(translator(m_taskArgs, m_dut, m_indentLevel), obj.statement);
      return m_indent + code;
    }

    std::string operator()(const verilog::Block &obj) const
    {
      std::string code;
      for(const auto& statement: obj.statements)
        code += boost::apply_visitor(translator(m_taskArgs, m_dut, m_indentLevel), statement);
      return code; // indentation for blocks is not needed in C++ code
    }

    std::vector<std::string> m_taskArgs;
    std::vector<verilog::Instance>::const_iterator m_dut;
    std::string m_indent;
    size_t m_indentLevel;
};

struct TestbenchGenerator
{
  TestbenchGenerator() = default;

  TestbenchGenerator(const std::filesystem::path &testbenchFile, const std::string &outFileStem)
    : m_testbenchFile(testbenchFile)
    , m_outFileStem(outFileStem)
  {}

  void generate()
  {
    m_parser.read(m_testbenchFile);

    generateHeader(m_parser.testbenchModule);
    generateSource(m_parser.testbenchModule);
  }
  void generateHeader(const Module &module)
  {
    std::ofstream header(m_outFileStem + ".h");
    if (!header.is_open()) {
        std::cerr << boost::format("Error: Unable to open header file\n");
        return;
    }

    // Preprocessor directives
    header << "#pragma once\n";
    header << '\n';
    header << "#include \"AbstractTestbench.h\"\n";
    header << '\n';
    header << "#include <boost/dll/alias.hpp>\n";
    header << '\n';
    header << "#include <array>\n";
    header << '\n';

    // Forward declaration
    header << "class IniParser;\n";
    header << '\n';

    header << boost::format("class %1% : public AbstractTestbench\n") % m_outFileStem;
    header << "{\n";
    header << "public:\n";
    header << indent << boost::format("%1%();\n") % m_outFileStem;
    header << '\n';
    header << indent << "void simulate() override;\n";
    header << '\n';
    header << indent
           << "static std::shared_ptr<AbstractTestbench> createPlugin(const IniParser &);\n";
    header << '\n';
    // header << indent << "void assign(Dummy &var, const std::string &bitString);\n";
    header << indent << "template<typename Container, Container>\n";
    header << indent << "void assign(Container &larray, Container &rarray);\n";
    header << indent << "template<typename Container, std::string>\n";
    header << indent << "void assign(Container &array, std::string &bitString);\n";

    header << '\n';
    header << "private:\n";

    for (const verilog::Parameter &param : module.parameters) {
      header << indent << "double " << param.name << ";\n";
    }
    header << '\n';

    for (const verilog::Var &var: module.variables) {
      if (var.name == "fiq_gclk") { // Special variable
        header << indent << "Dummy fiq_gclk{true};\n";
        continue;
      }
      std::string cppType;
      switch (var.type)
      {
      case verilog::VarType::LOGIC:
      case verilog::VarType::REG:
        cppType = "Dummy";
        break;
      case verilog::VarType::INTEGERKEY:
        cppType = "int";
        break;
      }
      if (var.beg.empty())
        header << indent << boost::format("%1% %2%;\n") % cppType % var.name;
      else
        header << indent << boost::format("std::array<%1%, %2%-%3%+1> %4%;\n") % cppType % var.beg % var.end %var.name;
    }

    for (const verilog::Net &net : module.nets) {
      if (net.beg == 0)
        header << indent << boost::format("Dummy %1%;\n") % net.names[0];
      else
        header << indent << boost::format("std::array<Dummy, %1%-%2%+1> %3%;\n") % net.beg % net.end %net.names[0];
    }

    header << "};\n";

    header << '\n';
    header << "// NOLINTNEXTLINE: Boost boilerplate code\n";
    header << boost::format("BOOST_DLL_ALIAS(%1%::createPlugin, createPlugin)\n") % m_outFileStem;
  }

  void generateSource(const Module &module)
  {
    std::ofstream source(m_outFileStem + ".cpp");
    if (!source.is_open()) {
        std::cerr << boost::format("Error: Unable to open source file\n");
        return;
    }

    source << boost::format("#include \"%1%.h\"\n") % m_outFileStem;

    auto it = std::find_if(module.instances.begin(), module.instances.end(), [](const verilog::Instance &inst) {
        return inst.inst_name == "dut";
    });
    if (it != module.instances.end())
      source << boost::format("#include \"%1%.h\"\n") % it->module_name;
    else
      throw std::runtime_error("Device under test was not found\n");

    source << '\n';

    source << "#include <iostream>\n";
    source << '\n';

    size_t inputBitLength=0, outputBitLength=0, keySize=0;
    getDesignInitSizes(module, keySize, inputBitLength, outputBitLength);
    if (!(inputBitLength && outputBitLength && keySize))
      throw std::runtime_error("Size of input, output or key was not found");
    source << m_outFileStem << "::" << m_outFileStem << "()\n";
    source << indent
           << boost::format(": AbstractTestbench(new %1%, %2%, %3%, %4%)\n")
              % it->module_name
              % inputBitLength
              % outputBitLength
              % keySize;

    std::string dutOutput;

    // Constructor body
    source << "{\n";
    // wrap this part into generateConstructorBody(std::ofstream &, std::vector<verilog::Instance>::const_iterator &)
    // Choose only input ports using netlist data later
    // outputPortNames = func();
    for (size_t pinIdx = 0; pinIdx < it->pin_names.size(); ++pinIdx) {
      std::string pinName;
      // Get name of the pin
      if (std::holds_alternative<std::string>(it->pin_names[pinIdx]))
          pinName = std::get<std::string>(it->pin_names[pinIdx]);
      else if (std::holds_alternative<verilog::NetBit>(it->pin_names[pinIdx]))
          pinName = std::get<verilog::NetBit>(it->pin_names[pinIdx]).name;
      else if (std::holds_alternative<verilog::NetRange>(it->pin_names[pinIdx]))
          pinName = std::get<verilog::NetRange>(it->pin_names[pinIdx]).name;

      //if pinName is not in outputPortNames ...
      // Handle unassigned pins
      if (it->net_names[pinIdx].empty())
          continue;

      // Get input of the current pin
      verilog::NetConcat pinArg = it->net_names[pinIdx][0];
      if (std::holds_alternative<std::string>(pinArg)) {
        std::string pinArgName = std::get<std::string>(pinArg);
        // Handle pragma `fiq resultdata`
        if (pinArgName == "dataout")
          dutOutput = pinName;
        source << indent;
        source << boost::format("m_topModule->setInputSource(%1%::input_%2%, %3%);\n")
                  % it->module_name
                  % pinName
                  % (pinArgIsScalar(module, pinArgName) ? "&" + pinArgName : pinArgName);
      }
      else if (std::holds_alternative<verilog::NetBit>(pinArg)) {
        std::string pinArgName = std::get<verilog::NetBit>(pinArg).name;
        source << indent;
        source << boost::format("m_topModule->setInputSource(%1%::input_%2%, &%3%[%4%]);\n)")
                  % it->module_name
                  % pinName
                  % pinArgName
                  % std::get<verilog::NetBit>(pinArg).bit;
      }
      else if (std::holds_alternative<verilog::NetRange>(pinArg)) {
        const verilog::NetRange range = std::get<verilog::NetRange>(pinArg);
        for(int i = range.end; i < range.beg; ++i) {
          source << indent;
          source << boost::format("m_topModule->setInputSource(%1%::input_%2%+%3%, &%5%[%6%]);\n)")
                    % it->module_name
                    % pinName
                    % (i - range.end)
                    % range.name
                    % i;
        }
      }
      else {
        const std::string binaryChars = toBinaryStr(std::get<verilog::Constant>(pinArg).value);
        size_t reverseIdx = binaryChars.size() - 1;
        for (size_t i = 0; i < binaryChars.size(); ++i) {
          source << indent;
          source << boost::format("m_topModule->setInputSource(%1%::input_%2%+%3%, %4%);\n")
                    % it->module_name
                    % pinName
                    % i
                    % toCodeStr(binaryChars[reverseIdx - i]);
        }
      }
    }
    source << "}\n";

    // Simulation function
    source << '\n';
    source << "void " + m_outFileStem + "::simulate()\n";
    source << "{\n";
    source << indent << "AbstractTestbench::simulate();\n";
    for (const verilog::Task &task : module.tasks) {
      std::vector<std::string> taskArgs;
      for (const verilog::Expression &expr : task.args)
        taskArgs.push_back(expr.string);
      source << boost::apply_visitor(translator(taskArgs, it), task.statement);
    }
    source << indent << boost::format("readOutput(%1%::output_%2%);\n") % it->module_name % dutOutput;
    source << "}\n";

    source << '\n';
    source << "std::shared_ptr<AbstractTestbench> " + m_outFileStem + "::createPlugin(const IniParser &)\n";
    source << "{\n";
    source << indent << "return std::make_shared<" + m_outFileStem + ">();\n";
    source << "}\n";
  }

  void getDesignInitSizes(const Module &module, size_t &keySize, size_t &inputSize, size_t &outputSize)
  {
    auto it = std::find_if(module.tasks.begin(), module.tasks.end(), [](const verilog::Task &task) {
        return task.name == "fiq_singlestep";
    });
    if (it != module.tasks.end()) {
      keySize = std::stoi(it->args[0].range_beg) - std::stoi(it->args[0].range_end) + 1;
      inputSize = std::stoi(it->args[1].range_beg) - std::stoi(it->args[1].range_end) + 1;
      outputSize = std::stoi(it->args[2].range_beg) - std::stoi(it->args[2].range_end) + 1;
    } else {
      throw std::runtime_error("`fiq_singlestep` task was not found\n");
    }
  }

  bool pinArgIsScalar(const Module &module, const std::string &pinArgName)
  {
    auto it = std::find_if(module.variables.begin(), module.variables.end(), [&pinArgName](const verilog::Var &var) {
      return var.name == pinArgName;
    });
    if (it != module.variables.end())
      return it->beg.empty();

    auto it2 = std::find_if(module.nets.begin(), module.nets.end(), [&pinArgName](const verilog::Net &net) {
      return net.names[0] == pinArgName;
    });
    if (it2 != module.nets.end())
      return it2->beg;

    throw std::logic_error("Input for DUT port was not declared.");
  }

  std::string toCodeStr(char binaryChar)
  {
      if (binaryChar == '0')
        return "&Dummy::zero()";
      if (binaryChar == '1')
          return "&Dummy::one()";
      return "&literal_x_or_z";
  }

  std::filesystem::path m_testbenchFile;
  std::string m_outFileStem;
  SampleParser m_parser;
  SampleParser m_netlistParser;
};

int main(const int argc, const char **argv){
  if(argc < 2) {
    std::cerr << "Usage: ./sample_parser verilog_file\n";
    return EXIT_FAILURE;
  }

  if(std::filesystem::exists(argv[1])) {
    TestbenchGenerator generator(argv[1], std::filesystem::path(argv[1]).stem());
    generator.generate();
  }
  return EXIT_SUCCESS;
}
