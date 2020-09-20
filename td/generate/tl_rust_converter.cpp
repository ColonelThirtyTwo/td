//
// Copyright Aliaksei Levin (levlam@telegram.org), Arseny Smirnov (arseny30@gmail.com) 2014-2020
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//
#include "tl_json_converter.h"

#include "td/tl/tl_simple.h"
#include "td/tl/tl_config.h"
#include "td/tl/tl_generate.h"

#include "td/utils/buffer.h"
#include "td/utils/common.h"
#include "td/utils/filesystem.h"
#include "td/utils/logging.h"
#include "td/utils/Slice.h"
#include "td/utils/StringBuilder.h"

#include <utility>
#include <algorithm>

static std::string remove_prefix(std::string &str, std::string &prefix) {
  if(prefix.size() >= str.size()) {
    return str;
  }
  if(str.substr(0, prefix.size()) == prefix && isupper(str[prefix.size()])) {
    return str.substr(prefix.size());
  }
  return str;
}

static std::string capitalize_first(std::string str) {
  std::transform(str.begin(), str.begin()+1, str.begin(), ::toupper);
  return str;
}

static bool needs_lifetime(const td::tl::simple::Type &type);

static bool needs_lifetime(const std::vector<td::tl::simple::Arg> &arglist) {
  for (auto &arg : arglist) {
    if(needs_lifetime(*arg.type))
      return true;
  }
  return false;
}

static bool needs_lifetime(const std::vector<const td::tl::simple::Constructor*> &conslist) {
  for (auto *cons : conslist) {
    if(needs_lifetime(cons->args))
      return true;
  }
  return false;
}

static bool needs_lifetime(const std::vector<const td::tl::simple::Function*> &conslist) {
  for (auto *cons : conslist) {
    if(needs_lifetime(cons->args))
      return true;
  }
  return false;
}

static bool needs_lifetime(const td::tl::simple::Type &type) {
  switch(type.type) {
    case td::tl::simple::Type::Bytes:
    case td::tl::simple::Type::String:
      return true;
    case td::tl::simple::Type::Vector:
      return needs_lifetime(*type.vector_value_type);
    case td::tl::simple::Type::Custom:
      return needs_lifetime(type.custom->constructors);
    default:
      return false;
  }
}

static std::string rust_type(const td::tl::simple::Type &type, const td::tl::simple::CustomType* parent=nullptr) {
  switch(type.type) {
      case td::tl::simple::Type::Bytes:
        return "Option<&'a [u8]>";
      case td::tl::simple::Type::Bool:
        return "bool";
      case td::tl::simple::Type::Int64:
      case td::tl::simple::Type::Int53:
        return "i64";
      case td::tl::simple::Type::Int32:
        return "i32";
      case td::tl::simple::Type::Double:
        return "f64";
      case td::tl::simple::Type::String:
        return "Option<Cow<'a, str>>";
      case td::tl::simple::Type::Vector:
        return "Vec<" + rust_type(*type.vector_value_type) + ">";
      case td::tl::simple::Type::Custom:
        std::string name = type.custom->name;
        if(needs_lifetime(type)) {
          name += "<'a>";
        }
        if(parent != nullptr && type.custom == parent) {
          name = "Box<"+name+">";
        }
        name = "Option<"+name+">";
        return name;
  }
  return "unimplemented";
}
static std::string rust_type(const td::tl::simple::Type &type, const td::tl::simple::Type* parent=nullptr) {
  const td::tl::simple::CustomType *pc_type = nullptr;
  if(parent && parent->type == td::tl::simple::Type::Custom)
    pc_type = parent->custom;
  return rust_type(type, pc_type);
}

static std::string rust_field_attr(const td::tl::simple::Type &type) {
  if(type.type == td::tl::simple::Type::String)
    return "#[serde(borrow, deserialize_with=\"crate::cow_de::de_opt_cow_str\")]";
  if(needs_lifetime(type))
    return "#[serde(borrow)]";
  return "";
}

namespace td {

template <class T>
void gen_rust_struct(StringBuilder &sb, const T &constructor) {
  auto capitalized = capitalize_first(constructor.name);
  
  sb << "\t#[derive(Serialize, Deserialize, Clone, Debug)]\n";
  sb << "\tpub struct " << capitalized;
  if(constructor.args.size() == 0) {
    sb << ";\n\n";
  } else {
    if(needs_lifetime(constructor.args)) {
      sb << "<'a>";
    }
    sb << " {\n";
    for (auto &arg : constructor.args) {
      std::string field_name = tl::simple::gen_cpp_name(arg.name);
      
      if(field_name == "type") {
        sb << "\t#[serde(rename=\"type\")]\n";
        field_name = "typ";
      }
      
      std::string type_attrs = rust_field_attr(*arg.type);
      if(type_attrs != "") {
        sb << "\t\t" << type_attrs << "\n";
      }
      
      sb << "\t\tpub " << field_name << ": " << rust_type(*arg.type, constructor.type) << ",\n";
    }
    sb << "\t}\n\n";
  }
}

void gen_rust_structs(StringBuilder &sb, const tl::simple::Schema &schema) {
  sb << "/// API Types\n";
  sb << "pub mod types {\n\tuse super::{*, dynamic::*};\n";
  for (auto *custom_type : schema.custom_types) {
    for (auto *constructor : custom_type->constructors) {
      sb << "\t/// Super type: " << constructor->type->name << "\n";
      gen_rust_struct(sb, *constructor);
    }
  }
  sb << "}\n\n";
  sb << "/// API functions\n";
  sb << "pub mod functions {\n\tuse super::{*, dynamic::*, types::*};\n";
  for (auto *function : schema.functions) {
    gen_rust_struct(sb, *function);
  }
  sb << "}\n\n";
}

template<class T>
void gen_rust_enums(StringBuilder &sb, std::string name, const std::vector<const T*> &vec, bool generate_from_object=false) {
  sb << "\t#[derive(Serialize, Deserialize, Clone, Debug)]\n";
  sb << "\t#[serde(tag=\"@type\")]\n";
  sb << "\tpub enum " << name;

  bool ty_needs_lifetime = needs_lifetime(vec);

  if(ty_needs_lifetime) {
    sb << "<'a>";
  }

  sb << " {\n";

  // Enum definition
  for (auto *cons : vec) {
    sb << "\t\t#[serde(rename=\"" << cons->name << "\")]\n";
    auto capitalized = capitalize_first(cons->name);
    auto variant_name = remove_prefix(capitalized, name);
    bool arg_needs_lifetime = needs_lifetime(cons->args);
    
    sb << "\t\t" << variant_name << "(";
    if(arg_needs_lifetime)
      sb << "#[serde(borrow)]";
    sb << capitalized;
    if(needs_lifetime(cons->args))
      sb << "<'a>";
    sb << "),\n";
  }
  sb << "\t}\n";

  // From impls for each child
  for(auto *cons : vec) {
    auto capitalized = capitalize_first(cons->name);
    auto variant_name = remove_prefix(capitalized, name);
    bool arg_needs_lifetime = needs_lifetime(cons->args);

    sb << "\timpl" << (ty_needs_lifetime ? "<'a>" : "") << " From<"
      << capitalized << (arg_needs_lifetime ? "<'a>" : "") << "> for "
      << name << (ty_needs_lifetime ? "<'a>" : "") << " { fn from(v: "
      << capitalized << (arg_needs_lifetime ? "<'a>" : "") << ") -> Self { Self::"
      << variant_name << "(v) }}\n";
  }
  
  // TryFrom<Object> and Into<Object>
  if(generate_from_object) {
    sb << "\timpl<'a> TryFrom<Object<'a>> for " << name << (ty_needs_lifetime ? "<'a>" : "") << "{\n";
    sb << "\t\ttype Error = Object<'a>;\n";
    sb << "\t\tfn try_from(v: Object<'a>) -> Result<Self, Object<'a>> {\n";
    sb << "\t\t\tmatch v {\n";
    for(auto *cons : vec) {
      auto capitalized = capitalize_first(cons->name);
      auto variant_name = remove_prefix(capitalized, name);

      sb << "\t\t\t\tObject::" << capitalized << "(v) => Result::Ok(Self::" << variant_name << "(v)),\n";
    }
    sb << "\t\t\t\tv @ _ => Result::Err(v),\n";
    sb << "\t\t\t}\n";
    sb << "\t\t}\n";
    sb << "\t}\n";
    
    sb << "\timpl<'a> Into<Object<'a>> for " << name << (ty_needs_lifetime ? "<'a>" : "") << "{\n";
    sb << "\t\tfn into(self) -> Object<'a> {\n";
    sb << "\t\t\tmatch self {\n";
    for(auto *cons : vec) {
      auto capitalized = capitalize_first(cons->name);
      auto variant_name = remove_prefix(capitalized, name);
      
      sb << "\t\t\t\tSelf::" << variant_name << "(v) => Object::" << capitalized << "(v),\n";
    }
    sb << "\t\t\t}\n";
    sb << "\t\t}\n";
    sb << "\t}\n";
  }
  
  sb << "\n";
}

void gen_rust_enums(StringBuilder &sb, const tl::simple::Schema &schema) {
  sb << "/// Enums containing type markers and subclasses\n";
  sb << "pub mod dynamic {\n\tuse super::{*, types::*, functions::*};\n";
  
  std::vector<const tl::simple::Constructor*> vec_for_nullary;
  for (auto *custom_type : schema.custom_types) {
    std::vector<const tl::simple::Constructor*> vec;
    for (auto *constructor : custom_type->constructors) {
      vec.push_back(constructor);
      vec_for_nullary.push_back(constructor);
    }

    if (vec.size() > 1) {
      gen_rust_enums(sb, tl::simple::gen_cpp_name(custom_type->name), vec, true);
    }
  }
  gen_rust_enums(sb, "Object", vec_for_nullary);

  std::vector<const tl::simple::Function*> vec_for_function;
  for (auto *function : schema.functions) {
    vec_for_function.push_back(function);
  }
  gen_rust_enums(sb, "Function", vec_for_function);
  
  sb << "}\n\n";
}

void gen_rust_file(const tl::simple::Schema &schema, const std::string &file_name_base) {
  auto file_name = "auto/" + file_name_base;
  auto old_file_content = [&] {
    auto r_content = read_file(file_name);
    if (r_content.is_error()) {
      return BufferSlice();
    }
    return r_content.move_as_ok();
  }();

  std::string buf(2000000, ' ');
  StringBuilder sb(buf);

  sb << "//! Auto-generated JSON messages\n";
  sb << "// Auto-generated, do not edit\n";
  sb << "use serde::{Serialize, Deserialize};\n";
  sb << "use std::{borrow::Cow, convert::TryFrom};\n";

  gen_rust_enums(sb, schema);
  gen_rust_structs(sb, schema);

  CHECK(!sb.is_error());
  buf.resize(sb.as_cslice().size());
#if TD_WINDOWS
  string new_file_content;
  for (auto c : buf) {
    if (c == '\n') {
      new_file_content += '\r';
    }
    new_file_content += c;
  }
#else
  auto new_file_content = std::move(buf);
#endif
  if (new_file_content != old_file_content.as_slice()) {
    write_file(file_name, new_file_content).ensure();
  }
}

void gen_rust(const tl::tl_config &config, const std::string &file_name) {
  tl::simple::Schema schema(config);
  gen_rust_file(schema, file_name);
}

}  // namespace td

int main() {
  td::gen_rust(td::tl::read_tl_config_from_file("scheme/td_api.tlo"), "td/telegram/td_api_json.rs");
}

