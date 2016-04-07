//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H


#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include "semant.h"
#include <iostream>
#include <set>
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS      \
virtual void semant() = 0;  \
virtual void dump_with_types(ostream&, int) = 0; 

#define program_EXTRAS  \
void semant();          \
void dump_with_types(ostream&, int);            


#define Class__EXTRAS                 \
virtual Symbol get_filename() = 0;    \
virtual Symbol get_name() = 0;        \
virtual Symbol get_parent() = 0;      \
virtual Features get_features() = 0;  \
virtual std::vector<std::pair<Symbol, Symbol> > get_attributes() = 0;  \
virtual std::set<Symbol> get_attribute_names() = 0;  \
virtual void dump_with_types(ostream&,int) = 0;      \
virtual std::vector<std::pair<Symbol, signature> > get_formal_names() = 0;      \
virtual std::vector<std::pair<Symbol, signature> > get_method_signatures() = 0; \
virtual void annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*) = 0;


#define class__EXTRAS                         \
Symbol get_filename() { return filename; }    \
Symbol get_name() { return name; }            \
Symbol get_parent() { return parent; }        \
Features get_features() { return features; }  \
std::vector<std::pair<Symbol, Symbol> > get_attributes();  \
std::set<Symbol> get_attribute_names();       \
void dump_with_types(ostream&,int);           \
std::vector<std::pair<Symbol, signature> > get_formal_names();      \
std::vector<std::pair<Symbol, signature> > get_method_signatures(); \
void annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*);


#define Feature_EXTRAS                                     \
virtual void dump_with_types(ostream&,int) = 0;            \
virtual std::pair<Symbol, Symbol> get_attribute() = 0;        \
virtual std::pair<Symbol, signature> get_formal_names() = 0;  \
virtual std::pair<Symbol, signature> get_signature() = 0;     \
virtual void annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*, Symbol) = 0;


#define Feature_SHARED_EXTRAS                     \
void dump_with_types(ostream&,int);               \
std::pair<Symbol, signature> get_formal_names();  \
std::pair<Symbol, signature> get_signature();     \
void annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*, Symbol);

#define method_EXTRAS  \
std::pair<Symbol, Symbol> get_attribute() { return std::pair<Symbol, Symbol>(0, 0); }

#define attr_EXTRAS  \
std::pair<Symbol, Symbol> get_attribute() { return std::pair<Symbol, Symbol>(name, type_decl); }


#define Formal_EXTRAS                            \
virtual void dump_with_types(ostream&,int) = 0;  \
virtual Symbol get_type() = 0;  \
virtual Symbol get_name() = 0;


#define formal_EXTRAS                \
void dump_with_types(ostream&,int);  \
Symbol get_type() { return type_decl; }  \
Symbol get_name() { return name;      }


#define Case_EXTRAS  \
virtual void dump_with_types(ostream& ,int) = 0;  \
virtual Symbol annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*, Symbol) = 0;  \
virtual Symbol get_type() = 0;  \
virtual Symbol get_name() = 0;  \


#define branch_EXTRAS  \
void dump_with_types(ostream& ,int);     \
Symbol get_type() { return type_decl; }  \
Symbol get_name() { return name;      }  \
Symbol annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*, Symbol);


#define Expression_EXTRAS           \
Symbol type;                        \
Symbol get_type() { return type; }  \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);                \
Expression_class() { type = (Symbol) NULL; }  \
virtual bool is_no_expr() = 0;                \
virtual Symbol annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*, Symbol) = 0;


#define Expression_SHARED_EXTRAS     \
void dump_with_types(ostream&,int);  \
Symbol annotate_types(ClassTable*, MethodTable*, SymbolTable<Symbol, Entry>*, Symbol);


#define assign_EXTRAS  \
bool is_no_expr() { return false; }

#define static_dispatch_EXTRAS  \
bool is_no_expr() { return false; }

#define dispatch_EXTRAS         \
bool is_no_expr() { return false; }

#define cond_EXTRAS             \
bool is_no_expr() { return false; }

#define loop_EXTRAS             \
bool is_no_expr() { return false; }

#define typcase_EXTRAS          \
bool is_no_expr() { return false; }

#define block_EXTRAS            \
bool is_no_expr() { return false; }

#define let_EXTRAS              \
bool is_no_expr() { return false; }

#define plus_EXTRAS             \
bool is_no_expr() { return false; }

#define sub_EXTRAS              \
bool is_no_expr() { return false; }

#define mul_EXTRAS              \
bool is_no_expr() { return false; }

#define divide_EXTRAS           \
bool is_no_expr() { return false; }

#define neg_EXTRAS  \
bool is_no_expr() { return false; }

#define lt_EXTRAS  \
bool is_no_expr() { return false; }

#define eq_EXTRAS  \
bool is_no_expr() { return false; }

#define leq_EXTRAS  \
bool is_no_expr() { return false; }

#define comp_EXTRAS  \
bool is_no_expr() { return false; }

#define int_const_EXTRAS  \
bool is_no_expr() { return false; }

#define bool_const_EXTRAS  \
bool is_no_expr() { return false; }

#define string_const_EXTRAS  \
bool is_no_expr() { return false; }

#define new__EXTRAS  \
bool is_no_expr() { return false; }

#define isvoid_EXTRAS  \
bool is_no_expr() { return false; }

#define no_expr_EXTRAS \
bool is_no_expr() { return true; }

#define object_EXTRAS  \
bool is_no_expr() { return false; }


#endif
