#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <symtab.h>
#include <map>
#include <vector>
#include <set>
#include <algorithm>
#include <sstream>
#include <cassert>
#include "semant.h"
#include "utilities.h"
#include "cool-tree.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}


ClassTable::ClassTable(Classes classes, Classes &base_classes) : semant_errors(0), error_stream(cerr) {

    base_classes = install_basic_classes(classes);

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ currclass = classes->nth(i);
        Symbol parent = currclass->get_parent();
        if (parent == Int) {
            semant_error(currclass) << currclass->get_name()
                                    << " inherits from Int." << endl;
        } else if (parent == Bool) {
            semant_error(currclass) << currclass->get_name()
                                    << " inherits from Bool." << endl;
        } else if (parent == Str) {
            semant_error(currclass) << currclass->get_name()
                                    << " inherits from Str." << endl;
        } else if (parent == SELF_TYPE) {
            semant_error(currclass) << currclass->get_name()
                                    << " inherits from SELF_TYPE." << endl;
        } else if (currclass->get_name() == SELF_TYPE) {
            semant_error(currclass) << "SELF_TYPE cannot be defined by program."
                                    << endl;
        }
    }

    exit_if_errors();
    
    std::vector<Class_> not_in_heirarchy;
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        not_in_heirarchy.push_back(classes->nth(i));
    }

    bool try_to_grow_heirarchy = true;
    while (try_to_grow_heirarchy && !not_in_heirarchy.empty()) {
        try_to_grow_heirarchy = false;

        std::vector<Class_>::iterator v_it;
        for (v_it = not_in_heirarchy.begin(); v_it != not_in_heirarchy.end(); ++v_it) {
            if (try_add_class(*v_it)) {
                not_in_heirarchy.erase(v_it);
                try_to_grow_heirarchy = true;
                break;
            }
        }
    }

    if (!not_in_heirarchy.empty()) {
        std::vector<Class_>::iterator v_it;
        for (v_it = not_in_heirarchy.begin(); v_it != not_in_heirarchy.end(); ++v_it) {
            semant_error(*v_it) << (*v_it)->get_name() << " either belongs to "
                << "an inheritance cycle or has an undefined parent." << endl; 
        }
    }

    // "It is illegal to redefine attribute names."
    // An attribute cannot have name 'self'
    std::map<Symbol, Class_>::iterator m_it;
    for (m_it = symbol_to_class_.begin(); m_it != symbol_to_class_.end(); ++m_it) {
        Symbol descendant = m_it->first;
        std::vector<Symbol> ancestors = get_ancestors(descendant);
        std::set<Symbol> desc_attrs  = get_class(descendant)->get_attribute_names();
        if (desc_attrs.find(self) != desc_attrs.end()) {
            semant_error(get_class(descendant)) << "Class " << descendant
                << " has an attribute named self" << endl;
        }

        std::vector<Symbol>::iterator a_it = ancestors.begin();
        for (++a_it; a_it != ancestors.end(); ++a_it) {
            Symbol ancestor = *a_it;
            std::set<Symbol> anctr_attrs = get_class(ancestor)->get_attribute_names();
            std::set<Symbol> intersection;
            set_intersection(desc_attrs.begin(), desc_attrs.end(),
                             anctr_attrs.begin(), anctr_attrs.end(),
                             std::inserter(intersection, intersection.begin()));

            if (intersection.size() > 0) {
                semant_error(get_class(descendant)) << "Class " << descendant
                    << " overwrites the attributes";
                std::set<Symbol>::iterator e_it;
                for (e_it = intersection.begin(); e_it != intersection.end(); ++e_it) {
                    error_stream << " " << *e_it;
                }
                error_stream << " already defined by its ancestor " << ancestor
                             << "." << endl;
            }
        }
    }
    error_stream << endl;

    /* "Every program must have a class Main." */
    if (!get_class(Main)) {
        semant_error() << "Class Main is not defined." << endl;
    }

    exit_if_errors();
}


bool ClassTable::try_add_class(Class_ c) {
    if (symbol_to_class_.find(c->get_parent()) == symbol_to_class_.end()) {
        return false;
    } else if (symbol_to_class_.find(c->get_name()) != symbol_to_class_.end()) {
        // "All class names are globally visible... Classes may not be redefined."
        semant_error(c) << "class " << c->get_name() << " declared more "
                        << "than once." << endl;
        return true;
    }

    symbol_to_class_[c->get_name()] = c;
    return true;
}

void ClassTable::exit_if_errors() {
    if (errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

Classes ClassTable::install_basic_classes(Classes classes) {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    symbol_to_class_[Object] = Object_class;
    symbol_to_class_[IO]     = IO_class;
    symbol_to_class_[Int]    = Int_class;
    symbol_to_class_[Bool]   = Bool_class;
    symbol_to_class_[Str]    = Str_class;

    return append_Classes(
        classes,
        append_Classes(
            append_Classes(
                append_Classes(
                    append_Classes(
                        single_Classes(Object_class),
                        single_Classes(IO_class)
                    ),
                    single_Classes(Int_class)
                ),
                single_Classes(Bool_class)
            ),
            single_Classes(Str_class)
        )
    );
}


std::vector<Symbol> ClassTable::get_ancestors(Symbol c) {
    std::vector<Symbol> ancestors;

    for (c; get_class(c); c = get_class(c)->get_parent()) {
        ancestors.push_back(c);
    }

    return ancestors;
}

Class_ ClassTable::get_class(Symbol c) {
    std::map<Symbol, Class_>::iterator it;
    it = symbol_to_class_.find(c);
    if (it == symbol_to_class_.end()) {
        return 0;
    } else {
        return it->second;
    }
}

bool ClassTable::is_subtype(Symbol d, Symbol a) {
    assert(get_class(d) && get_class(a));

    std::vector<Symbol> ancestors = get_ancestors(d);
    std::vector<Symbol>::iterator it;
    for (it = ancestors.begin(); it != ancestors.end(); ++it) {
        if (*it == a) {
            return true;
        }
    }
    return false;
}

bool ClassTable::is_subtype(Symbol d, Symbol a, Symbol cls) {
    
    if (d == SELF_TYPE && a == SELF_TYPE) {
        return true;
    } else if (d != SELF_TYPE && a == SELF_TYPE) {
        return false;
    } else if (d == SELF_TYPE) {
        return is_subtype(cls, a);
    } else {
        return is_subtype(d, a);
    }

}

Symbol ClassTable::lub(Symbol a, Symbol b) {
    assert(get_class(a) && get_class(b));
    std::vector<Symbol> a_anctrs = get_ancestors(a);
    std::vector<Symbol> b_anctrs = get_ancestors(b);

    Symbol lub;
    for (int a_idx = a_anctrs.size() - 1, b_idx = b_anctrs.size() - 1;
         a_idx >= 0 && b_idx >= 0 && a_anctrs[a_idx] == b_anctrs[b_idx];
         --a_idx, --b_idx) {
        lub = a_anctrs[a_idx];
    }

    return lub;
}

Symbol ClassTable::lub(Symbol a, Symbol b, Symbol cls) {
    if (a == SELF_TYPE && b == SELF_TYPE) {
        return SELF_TYPE;
    }

    if (a == SELF_TYPE) {
        a = cls;
    }
    if (b == SELF_TYPE) {
        b = cls;
    }
    return lub(a, b);
}

void ClassTable::log_lub_is_subtype() {
    Symbol c   = idtable.add_string("C");
    Symbol c1  = idtable.add_string("C1");
    Symbol z   = idtable.add_string("Z");
    Symbol zx  = idtable.add_string("Z_X");
    Symbol zy  = idtable.add_string("Z_Y");
    Symbol zyv = idtable.add_string("Z_Y_V");
    cout << "lub(C1, Z_Y_V) = " << lub(c1, zyv) << ", expecting Object." << endl;
    cout << "lub(C1, C1) = " << lub(c1, c1) << ", expecting C1." << endl;
    cout << "lub(C1, C) = " << lub(c, c1) << ", expecting C." << endl;
    cout << "lub(Z_X, Z_Y_V) = " << lub(zx, zyv) << ", expecting Z." << endl;
    cout << endl;

    cout << "is_subtype(C1, C) = " << is_subtype(c1, c) << ", expecting true" << endl;
    cout << "is_subtype(C1, C1) = " << is_subtype(c1, c1) << ", expecting true" << endl;
    cout << "is_subtype(C1, Object) = " << is_subtype(c1, Object) << ", expecting true" << endl;
    cout << "is_subtype(C1, Z) = " << is_subtype(c1, z) << ", expecting false" << endl;
    cout << "is_subtype(Z_X, Z_Y) = " << is_subtype(zx, zy) << ", expecting false" << endl;
    cout << endl;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

void ClassTable::log_classes() {
    error_stream << "CLASSES AND THEIR PARENTS" << endl;
    std::map<Symbol, Class_>::iterator it;
    for (it = symbol_to_class_.begin(); it != symbol_to_class_.end(); ++it) {
        error_stream << it->second->get_name() << " inherits from "
                     << it->second->get_parent() << endl;
    }
}


MethodTable::MethodTable(Classes classes, Classes base_classes,
                         ClassTable *classtable) : classtable_(classtable) {

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ curr_class = classes->nth(i);
        
        std::vector<std::pair<Symbol, signature> > formal_names =
            curr_class->get_formal_names();
        std::vector<std::pair<Symbol, signature> >::iterator f_n_it;
        for (f_n_it = formal_names.begin(); f_n_it != formal_names.end(); ++f_n_it) {
            Symbol method_name = f_n_it->first;
            signature method_formal_names = f_n_it->second;
            std::set<Symbol> unique_names(method_formal_names.begin(),
                                          method_formal_names.end());
            if (unique_names.size() < method_formal_names.size()) {
                classtable_->semant_error(curr_class)
                    << "Class " << curr_class->get_name() << " has method "
                    << method_name << ", whose formal parameter names are not unique."
                    << endl;
            }
            if (unique_names.find(self) != unique_names.end()) {
                classtable_->semant_error(curr_class)
                    << "Class " << curr_class->get_name() << " has method "
                    << method_name << ", whose formal parameter names include self."
                    << endl;
            }
        }
        


        std::vector<std::pair<Symbol, signature> > signatures =
            curr_class->get_method_signatures();
        std::vector<std::pair<Symbol, signature> >::iterator it;
        for (it = signatures.begin(); it != signatures.end(); ++it) {
            signature_map_[class_method(curr_class->get_name(), it->first)] = it->second;
        }
    }

    for(int i = base_classes->first(); base_classes->more(i); i = base_classes->next(i)) {
        Class_ curr_class = base_classes->nth(i);
        std::vector<std::pair<Symbol, signature> > signatures =
            curr_class->get_method_signatures();
        std::vector<std::pair<Symbol, signature> >::iterator it;
        for (it = signatures.begin(); it != signatures.end(); ++it) {
            signature_map_[class_method(curr_class->get_name(), it->first)] = it->second;
        }
    }

    // "If a class C inherits a method f from an ancestor class P, then C may
    // override the inherited definition of f provided the number of arguments,
    // the types of the formal parameters, and the return type are exactly
    // the same in both definitions."
    //  &
    // SELF_TYPE cannot be the type of a formal.
    std::map<class_method, signature>::iterator s_it;
    for (s_it = signature_map_.begin(); s_it != signature_map_.end(); ++s_it) {
        Symbol c = s_it->first.first;
        Symbol m = s_it->first.second;
        signature sig = s_it->second;
        
        for (size_t sig_idx = 0; sig_idx + 1 < sig.size(); ++sig_idx) {
            if (sig[sig_idx] == SELF_TYPE) {
                classtable_->semant_error(classtable_->get_class(c))
                    << "Class " << c << " has method " << m << ", whose "
                    << "formal parameters contains SELF_TYPE." << endl;
            }
        }

        Symbol return_type = sig[sig.size() - 1];
        if (return_type != SELF_TYPE && !(classtable->get_class(return_type))) {
            classtable_->semant_error(classtable_->get_class(c))
                << "Class " << c << " has method " << m << ", whose return type "
                << "is undefined." << endl;
        }

        std::vector<Symbol> ancestors = classtable_->get_ancestors(c);
        std::vector<Symbol>::iterator a_it = ancestors.begin()++;
        for (; a_it != ancestors.end(); ++a_it) {
            std::map<class_method, signature>::iterator m_it;
            m_it = signature_map_.find(class_method(*a_it, m));
            if (m_it != signature_map_.end() && m_it->second != sig) {
                classtable_->semant_error(classtable_->get_class(c))
                    << "Class " << c << " has method " << m << " with "
                    << "signature " << sig_to_str(sig) << " which illegally "
                    << " overwrites the same method in class " << *a_it
                    << " with signature " << sig_to_str(m_it->second) << "."
                    << endl;
            }
        }
    }

    /* "Every program must have a class Main... the Main class must
       have a method main that takes no formal parameters. The main method must
       be defined in class Main (not inherited from another class)." */
    std::map<class_method, signature>::iterator main_it;
    main_it = signature_map_.find(class_method(Main, main_meth));
    if (main_it == signature_map_.end() || (main_it->second).size() > 1) {
        classtable_->semant_error() << "Main class must have a method main "
            << "that takes no formal parameters. The main method must be "
            << "defined in class Main (not inherited from another class)."
            << endl;
    }
}

signature MethodTable::get_signature(Symbol c, Symbol m) {
    std::vector<Symbol> ancestors = classtable_->get_ancestors(c);
    std::vector<Symbol>::iterator a_it;
    for (a_it = ancestors.begin(); a_it != ancestors.end(); ++a_it) {
        std::map<class_method, signature>::iterator m_it;
        m_it = signature_map_.find(class_method(*a_it, m));
        if (m_it != signature_map_.end()) {
            return m_it->second;
        }
    }

    return signature();
}

void MethodTable::log_methods(ostream& log) {
    log << "LIST OF METHODS" << endl;
    std::map<class_method, signature>::iterator it;
    for (it = signature_map_.begin(); it != signature_map_.end(); ++it) {
        log << "Class: " << (it->first).first << ", method: "
            << (it->first).second << ", signature:";

        signature sig = it->second;
        signature::iterator s_it;
        for (s_it = sig.begin(); s_it != sig.end(); ++s_it) {
            log << " " << *s_it;
        }

        log << endl;
    }
}


std::set<Symbol> class__class::get_attribute_names() {
    // REWRITE THIS USING get_attributes()
    std::set<Symbol> attributes;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        std::pair<Symbol, Symbol> next_attribute = features->nth(i)->get_attribute();
        if (next_attribute.first) {
            attributes.insert(next_attribute.first);
        }
    }
    return attributes;
}

std::vector<std::pair<Symbol, Symbol> > class__class::get_attributes() {
    std::vector<std::pair<Symbol, Symbol> > attributes;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        std::pair<Symbol, Symbol> next_attribute = features->nth(i)->get_attribute();
        if (next_attribute.first) {
            attributes.push_back(next_attribute);
        }
    }
    return attributes;    
}


std::vector<std::pair<Symbol, signature> > class__class::get_formal_names() {
    std::vector<std::pair<Symbol, signature> > formal_names;
    std::pair<Symbol, signature> next_formals;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        next_formals = features->nth(i)->get_formal_names();
        if (next_formals.second.size() > 0) {  // don't include attrs
            formal_names.push_back(next_formals);
        }
    }

    return formal_names;
}


std::pair<Symbol, signature> method_class::get_formal_names() {
    signature formal_names;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        formal_names.push_back(formals->nth(i)->get_name());
    }
    return std::pair<Symbol, signature>(name, formal_names);
}


std::pair<Symbol, signature> attr_class::get_formal_names() {
    return std::pair<Symbol, signature>(0, signature());
}


std::vector<std::pair<Symbol, signature> > class__class::get_method_signatures() {
    std::vector<std::pair<Symbol, signature> > signatures;
    std::pair<Symbol, signature> next_signature;
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        next_signature = features->nth(i)->get_signature();
        if (next_signature.second.size() > 0) {  // don't include attrs
            signatures.push_back(next_signature);
        }
    }

    return signatures;
}

std::pair<Symbol, signature> method_class::get_signature() {
    signature sig;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        sig.push_back(formals->nth(i)->get_type());
    }
    sig.push_back(return_type);
    return std::pair<Symbol, signature>(name, sig);
}


std::pair<Symbol, signature> attr_class::get_signature() {
    return std::pair<Symbol, signature>(0, signature());
}


std::string sig_to_str(signature sig) {
    std::stringstream ss;
    ss << "(";
    signature::iterator it;
    for (it = sig.begin(); it + 1 != sig.end(); ++it) {
        ss << *it << ", ";
    }
    ss << *it << ")";
    return ss.str();
}


void class__class::annotate_types(ClassTable *classtable, MethodTable *methodtable,
                                  SymbolTable<Symbol, Entry> *type_env) {
    type_env->enterscope();

    // Setup the object environment
    std::vector<Symbol> ancestors = classtable->get_ancestors(get_name());
    std::vector<Symbol>::iterator anct_it;
    for (anct_it = ancestors.begin(); anct_it != ancestors.end(); ++anct_it) {
        std::vector<std::pair<Symbol,Symbol> > attributes =
            classtable->get_class(*anct_it)->get_attributes();
        std::vector<std::pair<Symbol,Symbol> >::iterator attr_it;
        for (attr_it = attributes.begin(); attr_it != attributes.end(); ++attr_it) {
            type_env->addid(attr_it->first, attr_it->second);
        }
    }
    type_env->addid(self, SELF_TYPE);
    

    for(int i = features->first(); features->more(i); i = features->next(i)) {
        features->nth(i)->annotate_types(classtable, methodtable, type_env, get_name());
    }

    type_env->exitscope();
}


void method_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                  SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    type_env->enterscope();
    // Note return type is not part of the formals list.
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal formal = formals->nth(i);
        type_env->addid(formal->get_name(), formal->get_type());
    }

    Symbol expr_type = expr->annotate_types(classtable, methodtable,
                                            type_env, cls);

    if (!classtable->is_subtype(expr_type, return_type, cls)) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Return type " << return_type << " of method " << name << " in "
            << "class " << cls << " does not match the type of the method body "
            << expr_type << endl;
    }

    type_env->exitscope();
}


void attr_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    // Nothing to do if init is no_expr()
    if (init->is_no_expr()) {
        return;
    }

    Symbol init_type = init->annotate_types(classtable, methodtable,
                                            type_env, cls);

    if (!classtable->is_subtype(init_type, type_decl, cls)) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Type " << type_decl << " of attribute " << name << " of class "
            << cls << " does not match the type " << init_type << " of the "
            << "initialisation expression." << endl;
    }
}


Symbol typcase_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                     SymbolTable<Symbol, Entry>* type_env, Symbol cls) {

    expr->annotate_types(classtable, methodtable, type_env, cls);

    std::set<Symbol> branch_types;
    std::vector<Symbol> return_types;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Symbol branch_type = cases->nth(i)->get_type();
        if (branch_types.find(branch_type) != branch_types.end()) {
            classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
                << "Type " << branch_type << " is the type of more than one "
                << "branch in this case expression. The declared type of each "
                << "branch must be unique." << endl;
        }
        branch_types.insert(branch_type);

        type_env->enterscope();
        type_env->addid(cases->nth(i)->get_name(), branch_type);
        Symbol return_type = cases->nth(i)->annotate_types(
            classtable, methodtable, type_env, cls
        );
        type_env->exitscope();
        return_types.push_back(return_type);
    }

    Symbol case_type = return_types[0];
    std::vector<Symbol>::iterator it;
    for (it = return_types.begin() + 1; it != return_types.end(); ++it) {
        case_type = classtable->lub(case_type, *it, cls);
    }
    this->set_type(case_type);
    return case_type;
}


Symbol branch_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                    SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    // "it is an error to assign to... bind self in a... case"
    if (name == self) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Cannot bind self in a branch." << endl;
    }
    return expr->annotate_types(classtable, methodtable, type_env, cls);
}


Symbol object_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                    SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol this_type = type_env->lookup(name);
    if (this_type == NULL) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Object " << name << " has not been declared, so has no "
            << "type." << endl;
        this_type = Object;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol assign_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                    SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    // "it is an error to assign to self"
    if (name == self) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Cannot assign to self." << endl;
    }

    Symbol lhs_type = type_env->lookup(name);
    Symbol expr_type = expr->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;

    if (lhs_type == NULL) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Variable " << name << " not previously declared." << endl;
        this_type = Object;
    } else if (!classtable->is_subtype(expr_type, lhs_type, cls)) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "The type of the right-hand side, " << expr_type << ", must be "
            << "a subtype of the type of the left-hand side type, "
            << lhs_type << ", of the assignment." << endl;
        this_type = Object;
    } else {
        this_type = expr_type;
    }

    this->set_type(this_type);
    return this_type;
}


Symbol bool_const_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                        SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    this->set_type(Bool);
    return Bool;
}

Symbol int_const_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                        SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    this->set_type(Int);
    return Int;
}

Symbol string_const_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                          SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    this->set_type(Str);
    return Str;
}


Symbol new__class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                  SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    this->set_type(type_name);
    return type_name;
}


Symbol dispatch_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                      SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol expr_type = expr->annotate_types(classtable, methodtable, type_env, cls);
    Symbol dispatch_type;
    if (expr_type == SELF_TYPE) {
        dispatch_type = cls;
    } else {
        dispatch_type = expr_type;
    }

    std::vector<Symbol> actuals;
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actuals.push_back(
            actual->nth(i)->annotate_types(classtable, methodtable, type_env, cls)
        );
    }

    Symbol this_type;
    signature sig = methodtable->get_signature(dispatch_type, name);
    if (sig.size() == 0) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Class " << dispatch_type << " has no method " << name << "." << endl;
        this_type = Object;
    } else if (actuals.size() != sig.size() - 1) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Method " << name << " of class " << dispatch_type << " "
            << "expects " << sig.size() - 1 << " arguments. " << actuals.size()
            << " arguments were supplied." << endl;
        this_type = Object;
    } else {
        if (sig[sig.size() - 1] == SELF_TYPE) {
            this_type = expr_type;
        } else {
            this_type = sig[sig.size() - 1];
        }
        
        for (size_t i = 0; i < actuals.size(); i++) {
            if (!classtable->is_subtype(actuals[i], sig[i], cls)) {
                classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
                    << "Method " << name << " of class " << dispatch_type << " "
                    << "expects argument " << i + 1 << " to be a subtype of "
                    << sig[i] << ". Actual type is " << actuals[i] << endl;
                this_type = Object;
            }
        }
    }

    this->set_type(this_type);
    return this_type;
}


Symbol static_dispatch_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                             SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol expr_type = expr->annotate_types(classtable, methodtable, type_env, cls);
    Symbol dispatch_type = type_name;

    std::vector<Symbol> actuals;
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        actuals.push_back(
            actual->nth(i)->annotate_types(classtable, methodtable, type_env, cls)
        );
    }

    Symbol this_type;
    signature sig = methodtable->get_signature(dispatch_type, name);
    if (!classtable->is_subtype(expr_type, dispatch_type, cls)) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Static dispatch expects a subtype of " << dispatch_type << ". "
            << "It received type " << expr_type << endl;
        this_type = Object;
    } else if (sig.size() == 0) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Class " << dispatch_type << " has no method " << name << "." << endl;
        this_type = Object;
    } else if (actuals.size() != sig.size() - 1) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Method " << name << " of class " << dispatch_type << " "
            << "expects " << sig.size() - 1 << " arguments. " << actuals.size()
            << " arguments were supplied." << endl;
        this_type = Object;
    } else {
        if (sig[sig.size() - 1] == SELF_TYPE) {
            this_type = expr_type;
        } else {
            this_type = sig[sig.size() - 1];
        }
        
        for (size_t i = 0; i < actuals.size(); i++) {
            if (!classtable->is_subtype(actuals[i], sig[i], cls)) {
                classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
                    << "Method " << name << " of class " << dispatch_type << " "
                    << "expects argument " << i + 1 << " to be a subtype of "
                    << sig[i] << ". Actual type is " << actuals[i] << endl;
                this_type = Object;
            }
        }
    }

    this->set_type(this_type);
    return this_type;
}


Symbol cond_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                           SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol pred_type, then_type, else_type;
    pred_type = pred->annotate_types(classtable, methodtable, type_env, cls);
    then_type = then_exp->annotate_types(classtable, methodtable, type_env, cls);
    else_type = else_exp->annotate_types(classtable, methodtable, type_env, cls);

    Symbol this_type;
    if (pred_type == Bool) {
        this_type = classtable->lub(then_type, else_type, cls);
    } else {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "The predicate has type " << pred_type << " instead of "
            << "type Bool." << endl;
        this_type = Object;
    }

    this->set_type(this_type);
    return this_type;
}


Symbol block_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                   SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol last_type;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        last_type = body->nth(i)->annotate_types(classtable, methodtable, type_env, cls);
    }
    this->set_type(last_type);
    return last_type;
}


Symbol let_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    // "it is an error to assign to... bind self in a let"
    if (identifier == self) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Cannot bind to self in a let." << endl;
    }

    type_env->enterscope();
    type_env->addid(identifier, type_decl);
    Symbol this_type = body->annotate_types(classtable, methodtable, type_env, cls);
    type_env->exitscope();
    
    if (!init->is_no_expr()) {
        Symbol init_type = init->annotate_types(classtable, methodtable, type_env, cls);
        if (!classtable->is_subtype(init_type, type_decl, cls)) {
            classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
                << "Let initializer should be a subtype of " << type_decl << ". "
                << "Actual type is " << init_type << endl;
            this_type = Object;
        }
    }

    this->set_type(this_type);
    return this_type;
}


Symbol loop_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                  SymbolTable<Symbol, Entry>* type_env, Symbol cls) {

    Symbol pred_type = pred->annotate_types(classtable, methodtable, type_env, cls);
    body->annotate_types(classtable, methodtable, type_env, cls);

    if (pred_type != Bool) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "The loop predicate has type " << pred_type << " instead of "
            << "type Bool." << endl;
    }

    this->set_type(Object);
    return Object;
}


Symbol isvoid_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                    SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    e1->annotate_types(classtable, methodtable, type_env, cls);

    this->set_type(Bool);
    return Bool;
}

Symbol comp_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                  SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (e_type != Bool) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "The argument to the not operator should have type Bool, "
            << "but it has type " << e_type << " instead." << endl;
        this_type = Object;
    } else {
        this_type = e_type;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol lt_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);

    Symbol this_type;
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Both arguments to the less than '<' operator should have type "
            << "Int. The first has type " << e1_type << " and the second has "
            << "type " << e2_type << "." << endl;
        this_type = Object;     
    } else {
        this_type = Bool;
    }
    this->set_type(this_type);
    return this_type;
}

Symbol leq_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);

    Symbol this_type;
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Both arguments to the leq '<=' operator should have type "
            << "Int. The first has type " << e1_type << " and the second has "
            << "type " << e2_type << "." << endl;
        this_type = Object;
    } else {
        this_type = Bool;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol neg_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (e_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "The argument to the negation operator should have type Int, "
            << "but it has type " << e_type << " instead." << endl;
        this_type = Object;
    } else {
        this_type = e_type;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol plus_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Both arguments to the plus '+' operator should have type "
            << "Int. The first has type " << e1_type << " and the second has "
            << "type " << e2_type << "." << endl;
        this_type = Object;     
    } else {
        this_type = Int;
    }
    this->set_type(this_type);
    return this_type;
}

Symbol sub_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Both arguments to the minus '-' operator should have type "
            << "Int. The first has type " << e1_type << " and the second has "
            << "type " << e2_type << "." << endl;
        this_type = Object;     
    } else {
        this_type = Int;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol mul_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Both arguments to the multiplication '*' operator should have type "
            << "Int. The first has type " << e1_type << " and the second has "
            << "type " << e2_type << "." << endl;
        this_type = Object;
    } else {
        this_type = Int;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol divide_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                 SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Both arguments to the division '/' operator should have type "
            << "Int. The first has type " << e1_type << " and the second has "
            << "type " << e2_type << "." << endl;
        this_type = Object;     
    } else {
        this_type = Int;
    }
    this->set_type(this_type);
    return this_type;
}


Symbol eq_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    Symbol e1_type = e1->annotate_types(classtable, methodtable, type_env, cls);
    Symbol e2_type = e2->annotate_types(classtable, methodtable, type_env, cls);
    Symbol this_type;
    if (
        (e1_type == Int || e1_type == Str || e1_type == Bool ||
         e2_type == Int || e2_type == Str || e2_type == Bool) &&
        (e1_type != e2_type)
    ) {
        classtable->semant_error(classtable->get_class(cls)->get_filename(), this)
            << "Cannot check the equality of Int, String or Bool against a "
            << "different type. First argument has type " << e1_type << "; "
            << "second argument has type " << e2_type << "." << endl;
        this_type = Object;
    } else {
        this_type = Bool;
    }
    this->set_type(this_type);
    return this_type;
}

Symbol no_expr_class::annotate_types(ClassTable* classtable, MethodTable* methodtable,
                                SymbolTable<Symbol, Entry>* type_env, Symbol cls) {
    assert(1);
    return Object;
}


/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    Classes base_classes;
    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes, base_classes);

    /* MethodTable constructor does some semantic analysis */
    MethodTable *methodtable = new MethodTable(classes, base_classes, classtable);

    //classtable->log_classes();
    //methodtable->log_methods(cout);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    
    SymbolTable<Symbol, Entry> *type_env = new SymbolTable<Symbol, Entry>();

    //classtable->log_lub_is_subtype();
    
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        classes->nth(i)->annotate_types(classtable, methodtable, type_env);
    }
    

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
