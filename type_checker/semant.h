#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include <vector>
#include "tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class Class__class;
typedef Class__class *Class_;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

// Supports looking up ancestors and a class from symbols, does not support
// looking up children.
class ClassTable {
private:
  std::map<Symbol, Class_> symbol_to_class_;
  int semant_errors;
  Classes install_basic_classes(Classes classes);
  ostream& error_stream;
  bool try_add_class(Class_ c);
  void exit_if_errors();

public:
  ClassTable(Classes, Classes&);
  void log_classes();
  // Only use against test_lub_is_subtype.cl
  void log_lub_is_subtype();

  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  // If c not a class, returns empty vector.
  // Otherwise, first element is c, last element is Object
  std::vector<Symbol> get_ancestors(Symbol c);
  Class_ get_class(Symbol c);  // returns 0 if c is not a class
  bool is_subtype(Symbol d, Symbol a);
  bool is_subtype(Symbol d, Symbol a, Symbol cls);
  Symbol lub(Symbol a, Symbol b);
  Symbol lub(Symbol a, Symbol b, Symbol cls);
};

typedef std::pair<Symbol, Symbol> class_method;
typedef std::vector<Symbol> signature;

class MethodTable {
private:
  std::map<class_method, signature> signature_map_;
  ClassTable *classtable_;
public:
  MethodTable(Classes, Classes, ClassTable*);
  void log_methods(ostream& log);
  // If signature not found, returns signature size 0
  signature get_signature(Symbol c, Symbol m);
};

std::string sig_to_str(signature);

#endif

