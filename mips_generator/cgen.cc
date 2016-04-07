
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes, os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

static void emit_pop(ostream& str)
{
  emit_addiu(SP, SP, 4, str); 
}

static void emit_pop_into(char *reg, ostream& str)
{
  emit_load(reg, 1, SP, str);  // Note that this offset is in words
  emit_addiu(SP, SP, 4, str);  // and this offset in bytes
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_copy_proto_into_ACC(CgenNodeP cls, ostream &s)
{
  s << LA << ACC << " "; cls->code_ref(s); s << endl;
  emit_jal("Object.copy", s);
  s << JAL; cls->init_ref(s); s << endl;
}

static void emit_copy_proto_into_ACC(Symbol cls_sym, ostream &s)
{
  s << LA << ACC << " "; emit_protobj_ref(cls_sym, s); s << endl;
  emit_jal("Object.copy", s);
  s << JAL; emit_init_ref(cls_sym, s); s << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD; emit_disptable_ref(Str, s); s << endl;  // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; emit_disptable_ref(Int, s); s << endl;     // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD; emit_disptable_ref(Bool, s); s << endl;      // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s) :
  nds(NULL), str(s), class_name_tbl(this), max_label_num(0)
{
  stringclasstag = 0;
  class_name_tbl.push_back(Str);
  intclasstag    = 1;
  class_name_tbl.push_back(Int);
  boolclasstag   = 2;
  class_name_tbl.push_back(Bool);


  max_class_tag = 2;

  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  if (cgen_debug) {
    cout << "Finished building inheritance tree, as DFS:" << endl;
    root()->print_inheritance_tree(2);
  }

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name)) {
      return;
  }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  if (name == Bool) {
    nd->set_cls_tag(boolclasstag);
  } else if (name == Int) {
    nd->set_cls_tag(intclasstag);
  } else if (name == Str) {
    nd->set_cls_tag(stringclasstag);
  } else {
    max_class_tag++;
    nd->set_cls_tag(max_class_tag);
    class_name_tbl.push_back(nd->name);
  }
  nds = new List<CgenNode>(nd, nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
  }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
      set_relations(l->hd());
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

int CgenNode::parent_cls_tag()
{
  return(parentnd->cls_tag());
}

int CgenClassTable::get_new_label_num()
{
  max_label_num++;
  return(max_label_num);
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "coding class_nameTab" << endl;
  class_name_tbl.code_def(str);

  if (cgen_debug) cout << "coding prototype objects" << endl;
  AttrTbl attrs(No_class);
  DispTbl meths(No_class);
  root()->code_def(str, attrs, meths);
  
//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    l->hd()->code_init(str, this);
    l->hd()->code_methods(str, this);
  }

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
  class__class((const class__class &) *nd),
  parentnd(NULL),
  children(NULL),
  basic_status(bstatus),
  methods(((const class__class &) *nd).name),
  attrs(((const class__class &) *nd).name)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

void CgenNode::print_inheritance_tree(int indentation) {
  for (int i = 0; i < indentation; i++) cout << " ";
  cout << name
       << " (basic_status: " << basic_status << ", "
       << "class_tag: " << class_tag << ")" << endl;
  for (List<CgenNode> *l = children; l; l = l->tl()) {
    l->hd()->print_inheritance_tree(indentation + 2);
  }
}

vector<Symbol> CgenNode::get_attr_names()
{
  return(attrs.get_attr_names());
}

void CgenNode::code_def(ostream& s, AttrTbl& inherited_attrs,
                        DispTbl& inherited_methods) {
  
  attrs.inherit_attrs(inherited_attrs);
  methods.inherit_methods(inherited_methods);

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_attr()) {
      attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
      attrs.add_attr(attr->name, attr);
    } else {
      method_class *meth = dynamic_cast<method_class*>(features->nth(i));
      methods.add_method(name, meth->name, meth);
    }
  }

  // Generate prototype object
  s << WORD << "-1" << endl;                            // Add -1 eye catcher

  code_ref(s);  s << LABEL                              // label
      << WORD << class_tag << endl                      // tag
      << WORD << (DEFAULT_OBJFIELDS + attrs.size()) << endl  // size
      << WORD; emit_disptable_ref(name, s); s << endl;  // dispatch table
  
  attrs.code_prototype_attrs(s, name);
  // Set attribute defaults
  // if (name == Int) {
  //   s << WORD << 0 << endl;
  // } else if (name == Str) {
  //   IntEntryP int_zero_sym = inttable.lookup_string("0");
  //   s << WORD; int_zero_sym->code_ref(s); s << endl;
  //   s << WORD << 0 << endl;  // terminating 0 and 3 bytes of 0 padding in one
  // } else if (name == Bool) {
  //   s << WORD << FALSE << endl;
  // } else {
  //   for (size_t i = 0; i < attrs.size(); i++) {
  //     s << WORD << VOID_VAL << endl;
  //   }
  // }

  if (cgen_debug) cout << name << " " << methods << endl;

  // Generate dispatch table
  methods.code_def(s);

  for (List<CgenNode> *l = children; l; l = l->tl()) {
    l->hd()->code_def(s, attrs, methods);
  }
}

void CgenNode::code_ref(ostream& s)
{
  emit_protobj_ref(name, s);
}

void CgenNode::code_init(ostream& s, CgenClassTableP cls_tbl)
{
  init_ref(s); s << LABEL;

  if (basic()) {
    emit_return(s);
    return;
  }

  // $s0 <- $a0. Only if Main.main?
  emit_move(SELF, ACC, s);

   // Initialise environment
  CGenEnv env = CGenEnv();
  env.enterscope();
  vector<Symbol> attr_names = get_attr_names();
  for (size_t attr_idx = 0; attr_idx < attr_names.size(); attr_idx++) {
    // first attribute is at byte offset 12
    env.addid(attr_names[attr_idx], StoreLocation(Self_Obj, attr_idx + 3));
  }

  // set $fp <- $sp
  emit_move(FP, SP, s);
  // push SELF ($s0) which has current self object
  env.emit_push2(SELF, s);
  env.addid(self, StoreLocation(Act_Rec, 0));
  // push $ra
  env.emit_push2(RA, s);

  // Create new objects for string and int attributes (excluding those
  // of ancestor classes as those have already been initialised).
  // This is probably not necessary for strings because they are only
  // modified by trap.handler functions, and those look like they
  // return new strings (they don't modify their arguments). It could
  // also be unnecessary for ints if arithmetic is written so that it
  // does not modify any arguments in place.
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_attr()) {
      attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
      if (attr->type_decl == Int || attr->type_decl == Str) {
        // Load address of prototype objects into ACC
        s << LA << ACC << " ";
        emit_protobj_ref(attr->type_decl, s);
        s << endl;

        env.emit_push2(FP, s);
        emit_jal("Object.copy", s);
        env.emit_pop_into2(FP, s);

        // Save new object (address in ACC) into appropriate SELF attribute
        StoreLocation *loc = env.lookup(attr->name);
        if (cgen_debug && loc == NULL) {
          cout << "BUG: could not find object " << attr->name << "!!!" <<endl;
        }
        loc->emit_store_into(ACC, T1, s);
      }
    }
  }

  // Initialise parent's attributes
  if (name != Object) {
    emit_load(ACC, 0, FP, s);  // just to be sure
    emit_move(SELF, ACC, s);   // just to be sure
    env.emit_push2(FP, s);
    s << JAL << " "; parentnd->init_ref(s); s << endl;
    env.emit_pop_into2(FP, s);
  }

  // Execute any (nonempty) initialisation expressions
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (features->nth(i)->is_attr()) {
      attr_class *attr = dynamic_cast<attr_class*>(features->nth(i));
      if (!attr->init->is_no_expr()) {
        attr->init->code(s, this, env, cls_tbl);
        // Save result (address in ACC) into appropriate SELF attribute
        StoreLocation *loc = env.lookup(attr->name);
        if (cgen_debug && loc == NULL) {
          cout << "BUG: could not find object " << attr->name << "!!!" <<endl;
        }
        loc->emit_store_into(ACC, T1, s);
      }
    }
  }

  // Pop activation record, i.e. ra then self. init function
  // does not take arguments, so we don't need to worry about
  // those
  env.emit_pop_into2(RA, s);
  // We return self object, so load it into ACC
  env.emit_pop_into2(ACC, s);

  emit_return(s);
}

void CgenNode::init_ref(ostream& s)
{
  emit_init_ref(name, s);
}

void CgenNode::disp_tbl_ref(ostream& s)
{
  methods.code_ref(s);
}

void CgenNode::code_methods(ostream& s, CgenClassTableP cls_tbl)
{
  if (basic()) {
    return;
  }

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    features->nth(i)->code_method(s, this, cls_tbl);
  }
}

int CgenNode::get_method_offset(Symbol name)
{
  return(methods.get_method_offset(name));
}


// ------------- ClassNameTbl -------------

ClassNameTbl::ClassNameTbl(CgenClassTableP cgen_class_table) :
  cgen_class_table_(cgen_class_table)
{}

void ClassNameTbl::push_back(Symbol name)
{
  class_names_.push_back(name);
}

void ClassNameTbl::code_def(ostream& s)
{
  code_ref(s);  s  << LABEL;
  for (size_t idx = 0; idx < class_names_.size(); idx++) {
    char *curr_class_name = class_names_[idx]->get_string();
    s << WORD;
    stringtable.lookup_string(curr_class_name)->code_ref(s);
    s << endl;
  }

  s << CLASSOBJTAB << LABEL;
  for (size_t idx = 0; idx < class_names_.size(); idx++) {
    CgenNodeP curr_class = cgen_class_table_->probe(class_names_[idx]);
    s << WORD; curr_class->code_ref(s); s << endl;
    s << WORD; curr_class->init_ref(s); s << endl;
  }

  s << CLASSPARENTTAB << LABEL;
  for (size_t idx = 0; idx < class_names_.size(); idx++) {
    if (class_names_[idx] == Object) {
      s << WORD << NOCLASSTAG << endl;
    } else {
      CgenNodeP curr_class = cgen_class_table_->probe(class_names_[idx]);
      s << WORD << curr_class->parent_cls_tag() << endl;  
    }    
  }
}

void ClassNameTbl::code_ref(ostream& s)
{
  s << CLASSNAMETAB;
}


// ------------- DispTblEntry -------------

DispTblEntry::DispTblEntry(Symbol class_name, Symbol method_name,
                           method_class *method_ast) :
  class_name_(class_name), method_name_(method_name), method_ast_(method_ast)
{}

ostream& operator<<(ostream& os, const DispTblEntry& ent)
{
  os << "{ mth: \"" << ent.method_name_ << "\", "
     << "cls: \"" << ent.class_name_ << "\" }";
  return os;
}


// ----------- DispTbl -------------

//DispTbl::DispTbl() : entries_() { }

DispTbl::DispTbl(Symbol class_name) :
  class_name_(class_name)
{}

void DispTbl::inherit_methods(DispTbl& inherited_methods)
{
  entries_ = inherited_methods.entries_;
}

void DispTbl::add_method(Symbol class_name, Symbol method_name,
                           method_class *method_ast) {
  DispTblEntry new_method = DispTblEntry(class_name, method_name,
                                             method_ast);
  for(size_t idx = 0; idx < entries_.size(); idx++) {
    if (entries_[idx].method_name_ == method_name) {
      entries_[idx] = new_method;
      return;
    }
  }
  entries_.push_back(new_method);
}

void DispTbl::code_def(ostream& s)
{
  code_ref(s);  s << LABEL;
  for (size_t idx = 0; idx < entries_.size(); idx++) {
    s << WORD;
    emit_method_ref(entries_[idx].class_name_, entries_[idx].method_name_, s);
    s << endl;
  }
}

void DispTbl::code_ref(ostream& s)
{
  emit_disptable_ref(class_name_, s);
}

ostream& operator<<(ostream& os, const DispTbl& tbl)
{
  os << "[ ";
  for (size_t idx = 0; idx < tbl.entries_.size(); idx++) {
    if (idx != 0) {
      os << ", ";
    }
    os << tbl.entries_[idx];
  }  

  os << " ]";
  return os;
}

int DispTbl::get_method_offset(Symbol name)
{
  for (size_t idx = 0; idx < entries_.size(); idx++) {
    if (entries_[idx].method_name_ == name) {
      return(idx);
    }
  }
  if (cgen_debug) {
    cout << "BUG IN GET_METHOD_OFFSET!!!" << endl
    << "Couldn't find method '" << name << "' in class '"
    << class_name_ << "'." << endl;
  }
  return(0);
}


// ------------- AttrTblEntry -------------

AttrTblEntry::AttrTblEntry(Symbol attr_name, attr_class *attr_ast) :
  attr_name_(attr_name), attr_ast_(attr_ast)
{}


// ------------- AttrTbl -------------

AttrTbl::AttrTbl(Symbol class_name) : class_name_(class_name) {}

size_t AttrTbl::size()
{
  return(entries_.size());
}

void AttrTbl::inherit_attrs(AttrTbl& inherited_attrs)
{
  entries_ = inherited_attrs.entries_;
}

void AttrTbl::add_attr(Symbol attr_name, attr_class *attr_ast)
{
  entries_.push_back(AttrTblEntry(attr_name, attr_ast));
}

void AttrTbl::code_prototype_attrs(ostream& s, Symbol class_name)
{
  // Set attribute defaults
  if (class_name == Int) {

    s << WORD << 0 << endl;

  } else if (class_name == Str) {

    IntEntryP int_zero_sym = inttable.lookup_string("0");
    s << WORD; int_zero_sym->code_ref(s); s << endl;
    s << WORD << 0 << endl;  // terminating 0 and 3 bytes of 0 padding in one

  } else if (class_name == Bool) {

    s << WORD << FALSE << endl;

  } else {

    for (size_t i = 0; i < entries_.size(); i++) {
      Symbol attr_type = entries_[i].attr_ast_->type_decl;
      s << WORD;
      if (attr_type == Bool) {
        falsebool.code_ref(s);
      } else if (attr_type == Int) {
        (inttable.lookup_string("0"))->code_ref(s);
      } else if (attr_type == Str) {
        (stringtable.lookup_string(""))->code_ref(s);
      } else {
        s << VOID_VAL;
      }
      s << endl;
    }

  }
}

vector<Symbol> AttrTbl::get_attr_names()
{
  vector<Symbol> ret;
  for (size_t idx = 0; idx < entries_.size(); idx++) {
    ret.push_back(entries_[idx].attr_name_);
  }
  return(ret);
}


// ------------- StoreLocation -------------

StoreLocation::StoreLocation(Location location, int offset) :
  location_(location), offset_(offset)
{}

void StoreLocation::emit_load_into(char *dest_reg, ostream& s)
{
  switch(location_) {
  case Self_Obj:
    emit_load(dest_reg, 0, FP, s);
    emit_load(dest_reg, offset_, dest_reg, s);
    break;
  case Act_Rec:
  case Stack:
    emit_load(dest_reg, offset_, FP, s);
    break;
  }
}

// temp_reg is used to store the address of the SELF object
void StoreLocation::emit_store_into(char *source_reg, char *temp_reg, ostream& s)
{
  switch(location_) {
  case Self_Obj:
    emit_load(temp_reg, 0, FP, s);
    emit_store(source_reg , offset_, temp_reg, s);
    break;
  case Act_Rec:
  case Stack:
    // Almost certain you can write to formals
    emit_store(source_reg, offset_, FP, s);
    break;
  }
}

ostream& operator<<(ostream& os, const StoreLocation& loc)
{
  os << "{ Loc ";
  switch(loc.location_) {
  case Self_Obj:
    os << "Self_Obj";
    break;
  case Act_Rec:
    os << "Act_Rec";
    break;
  case Stack:
    os << "Stack";
    break;
  default:
    os << "WTF!!!";
    break;
  }
  os << " " << loc.offset_ << " }";
  return os;
}

// ------------- CGenEnv -------------

CGenEnv::CGenEnv() : locations_(), offset_from_fp_to_sp_(0)
{
  // if (cgen_debug) {
  //   cout << "Creating new environment..." << endl;
  // }
}

void CGenEnv::addid(Symbol sym, StoreLocation loc)
{
  StoreLocation *new_loc = new StoreLocation(loc.location_, loc.offset_);
  SymbolTable<Symbol, StoreLocation>::addid( sym, new_loc);
  // if (cgen_debug) {
  //   cout << "Added location " << *new_loc
  //        << " under symbol " << sym << "." << endl;
  // }
}

int CGenEnv::request_word_on_stack()
{
  offset_from_fp_to_sp_--;
  return(offset_from_fp_to_sp_ + 1);
}

void CGenEnv::relinquish_word_on_stack()
{
  offset_from_fp_to_sp_++;
}

void CGenEnv::emit_push2(char *reg, ostream& s)
{
  request_word_on_stack();
  emit_push(reg, s); 
}

void CGenEnv::emit_push2(Symbol sym, char *reg, ostream& s)
{
  int new_offset = request_word_on_stack();
  addid(sym, StoreLocation(Stack, new_offset));
  emit_push(reg, s);
}

void CGenEnv::emit_pop2(ostream& s)
{
  emit_pop(s);
  relinquish_word_on_stack();
}

void CGenEnv::emit_pop_into2(char *reg, ostream& s)
{
  emit_pop_into(reg, s);
  relinquish_word_on_stack();
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void attr_class::code_method(ostream& s, CgenNodeP self_cls,
                             CgenClassTableP cls_tbl)
{ /* do nothing */ }

void method_class::code_method(ostream& s, CgenNodeP self_cls,
                               CgenClassTableP cls_tbl)
{
  // Initalise the environment
  
  CGenEnv env = CGenEnv();
  env.enterscope();
  
  vector<Symbol> attr_names = self_cls->get_attr_names();
  for (size_t attr_idx = 0; attr_idx < attr_names.size(); attr_idx++) {
    // formals->first() == 0 and first attribute is at byte offset 12
    env.addid(attr_names[attr_idx], StoreLocation(Self_Obj, attr_idx + 3));
  }

  int num_formals = formals->len();
  for (int idx = formals->first(); formals->more(idx); idx = formals->next(idx)) {
    Symbol formal_name = formals->nth(idx)->get_name();
    // formals->first() == 0
    env.addid(formal_name, StoreLocation(Act_Rec, num_formals - idx));
  }

  env.addid(self, StoreLocation(Act_Rec, 0));

  // Begin assembly generation

  emit_method_ref(self_cls->get_name(), name, s); s << LABEL;
  
  // If this is Main.main, copy self object from $a0 to $s0
  if (self_cls->get_name() == Main && name == main_meth) {
    emit_move(SELF, ACC, s);
  }

  // set $fp <- $sp
  emit_move(FP, SP, s);

  // push SELF ($s0) which has current self object
  // (already added symbol to environment)
  env.emit_push2(SELF, s);
  // push $ra (already added symbol to environment)
  env.emit_push2(RA, s);

  // Descend AST
  expr->code(s, self_cls, env, cls_tbl);
  // Result is in ACC

  // pop activation record minus old frame pointer, i.e.
  // ra, self, num args
  env.emit_pop_into2(RA, s);
  env.emit_pop2(s);
  emit_addiu(SP, SP, 4 * (formals->len()), s);
  
  emit_return(s);
}

void assign_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                        CgenClassTableP cls_tbl)
{
  // NOTIFY GARBAGE COLLECTOR, BUT ONLY IF USING GARBAGE COLLECTOR
  // SET REGISTER BIT MASK
   
  expr->code(s, self_cls, env, cls_tbl);

  // Save new object (address in ACC) into appropriate SELF attribute
  StoreLocation *loc = env.lookup(name);
  if (cgen_debug && loc == NULL) {
    cout << "BUG: could not find object " << name << "!!!" <<endl;
  }
  loc->emit_store_into(ACC, T1, s);

  // ACC should still contain result of expr
}

void static_dispatch_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                                 CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);

  for (int idx = actual->first(); actual->more(idx); idx = actual->next(idx)) {
    actual->nth(idx)->code(s, self_cls, env, cls_tbl);
    env.emit_push2(ACC, s);
  }

  // Self object gets passed in ACC ($a0) and SELF ($s0)
  expr->code(s, self_cls, env, cls_tbl);
  emit_move(SELF, ACC, s);

  // Check if ACC is void, call _dispatch_abort if true
  int not_void_label = cls_tbl->get_new_label_num();
  emit_bne(ACC, ZERO, not_void_label, s);
  // load filename into ACC and line number into T1
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(self_cls->get_filename()->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1, 999, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(not_void_label, s);

  // Get CgenNode for dispatch type
  Symbol dispatch_type = type_name;
  if (dispatch_type == SELF_TYPE) {
    dispatch_type = self_cls->get_name();
  }
  CgenNodeP dispatch_cgen_node = cls_tbl->lookup(dispatch_type);
  if (dispatch_cgen_node == NULL && cgen_debug) {
    cout << "BUG: could not find type " << dispatch_type << "!"
         << endl;
  }

  // get dispatch table
  emit_partial_load_address(T2, s);
  dispatch_cgen_node->disp_tbl_ref(s);
  s << endl;

  emit_load(T1, dispatch_cgen_node->get_method_offset(name), T2, s);

  // jalr to function, result is in ACC
  emit_jalr(T1, s);

  for (int idx = actual->first(); actual->more(idx); idx = actual->next(idx)) {
    env.relinquish_word_on_stack();
  }
  env.emit_pop_into2(FP, s);
}

void dispatch_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                          CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);

  for (int idx = actual->first(); actual->more(idx); idx = actual->next(idx)) {
    actual->nth(idx)->code(s, self_cls, env, cls_tbl);
    env.emit_push2(ACC, s);
  }

  // Self object gets passed in ACC ($a0) and SELF ($s0)
  expr->code(s, self_cls, env, cls_tbl);
  emit_move(SELF, ACC, s);

  // Check if ACC is void, call _dispatch_abort if true
  int not_void_label = cls_tbl->get_new_label_num();
  emit_bne(ACC, ZERO, not_void_label, s);
  // load filename into ACC and line number into T1
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(self_cls->get_filename()->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1, 999, s);
  emit_jal("_dispatch_abort", s);

  emit_label_def(not_void_label, s);

  // Get address of function
  Symbol expr_type = expr->get_type();
  if (expr_type == SELF_TYPE) {
    expr_type = self_cls->get_name();
  }
  CgenNodeP expr_cgen_node = cls_tbl->lookup(expr_type);
  if (expr_cgen_node == NULL && cgen_debug) {
    cout << "BUG: could not find type " << expr_type << "!"
         << endl;
  }
  emit_load(T2, DISPTABLE_OFFSET, ACC, s);  // get dispatch table
  emit_load(T1, expr_cgen_node->get_method_offset(name), T2, s);

  // jalr to function
  emit_jalr(T1, s);
  // Result is in ACC

  for (int idx = actual->first(); actual->more(idx); idx = actual->next(idx)) {
    env.relinquish_word_on_stack();
  }
  env.emit_pop_into2(FP, s);
}

void cond_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  pred->code(s, self_cls, env, cls_tbl);

  int false_label_num = cls_tbl->get_new_label_num();
  int end_cond_label_num = cls_tbl->get_new_label_num();

  // Predicate result is ACC
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_load_imm(T2, FALSE, s);
  emit_beq(T1, T2, false_label_num, s);

  then_exp->code(s, self_cls, env, cls_tbl);
  emit_branch(end_cond_label_num, s);

  emit_label_def(false_label_num, s);
  else_exp->code(s, self_cls, env, cls_tbl);

  emit_label_def(end_cond_label_num, s);
}

void loop_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  int start_loop_label = cls_tbl->get_new_label_num();
  int end_loop_label = cls_tbl->get_new_label_num();

  emit_label_def(start_loop_label, s);

  pred->code(s, self_cls, env, cls_tbl);

  // Predicate result is in ACC
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_load_imm(T2, FALSE, s);
  emit_beq(T1, T2, end_loop_label, s);

  body->code(s, self_cls, env, cls_tbl);
  emit_branch(start_loop_label, s);

  emit_label_def(end_loop_label, s);

  // while loops return void; load void into ACC
  emit_load_imm(ACC, VOID_VAL, s);
}

void typcase_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {

  expr->code(s, self_cls, env, cls_tbl);

  // Check if ACC is void, call _case_abort2 if true
  int not_void_label = cls_tbl->get_new_label_num();
  emit_bne(ACC, ZERO, not_void_label, s);
  // load filename into ACC and line number into T1
  emit_partial_load_address(ACC, s);
  stringtable.lookup_string(self_cls->get_filename()->get_string())->code_ref(s);
  s << endl;
  emit_load_imm(T1, 888, s);
  emit_jal("_case_abort2", s);


  // If not void, ACC still holds value of expr
  emit_label_def(not_void_label, s);  

  // Find the closest ancestor to ACC
  int scan_branch_cls_tags_label = cls_tbl->get_new_label_num();
  int begin_scan_label = cls_tbl->get_new_label_num();
  int end_case_label = cls_tbl->get_new_label_num();
  
  emit_load(T1, TAG_OFFSET, ACC, s);

  // Scan branch class tags in turn to see if match T1
  emit_label_def(scan_branch_cls_tags_label, s);
  emit_load_imm(T2, NOCLASSTAG, s);
  emit_bne(T1, T2, begin_scan_label, s);
  // Throw error if case statement has no matching branch.
  // ACC already holds switch object.
  emit_jal("_case_abort", s);

  emit_label_def(begin_scan_label, s);
  std::vector<int> branch_labels;
  for (int branch_idx = cases->first(); cases->more(branch_idx); branch_idx = cases->next(branch_idx)) {
    CgenNodeP curr_cls = cls_tbl->lookup(cases->nth(branch_idx)->get_type_decl());
    int branch_label = cls_tbl->get_new_label_num();
    branch_labels.push_back(branch_label);

    emit_load_imm(T2, curr_cls->cls_tag(), s);
    emit_beq(T1, T2, branch_label, s);
  }

  // No class tag matched, get T1's parent's class tag
  emit_load_address(T2, CLASSPARENTTAB, s);
  emit_load_imm(T3, 4, s);
  emit_mul(T3, T1, T3, s);
  emit_add(T2, T2, T3, s);
  emit_load(T1, 0, T2, s);
  emit_branch(scan_branch_cls_tags_label, s);

  // Generate the code for each of the branches
  for (int branch_idx = cases->first(); cases->more(branch_idx); branch_idx = cases->next(branch_idx)) {
    emit_label_def(branch_labels[branch_idx], s);
    cases->nth(branch_idx)->code(s, self_cls, env, cls_tbl);
    emit_branch(end_case_label, s); 
  }

  emit_label_def(end_case_label, s);  
}

void branch_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                        CgenClassTableP cls_tbl)
{
  env.enterscope();
  int new_offset = env.request_word_on_stack();
  env.addid(name, StoreLocation(Stack, new_offset));
  emit_push(ACC, s);

  expr->code(s, self_cls, env, cls_tbl);
  
  emit_pop(s);
  env.relinquish_word_on_stack();
  env.exitscope();
}

void block_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  for (int idx = body->first(); body->more(idx); idx = body->next(idx)) {
    body->nth(idx)->code(s, self_cls, env, cls_tbl);
  }
  // ACC holds the result of the last expression
}

void let_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  if (init->is_no_expr()) {
    // Default initialisation
    if (type_decl == Int || type_decl == Str) {
      emit_push(FP, s);
      emit_copy_proto_into_ACC(type_decl, s);
      emit_pop_into(FP, s);
    } else if (type_decl == Bool) {
      emit_partial_load_address(ACC, s);
      falsebool.code_ref(s);
      s << endl;
    } else {
      emit_load_imm(ACC, VOID_VAL, s);
    }
  } else {
    init->code(s, self_cls, env, cls_tbl);
  }

  env.enterscope();
  int new_offset = env.request_word_on_stack();
  env.addid(identifier, StoreLocation(Stack, new_offset));
  emit_push(ACC, s);

  body->code(s, self_cls, env, cls_tbl);
  // Result is now in ACC

  emit_pop(s);
  env.relinquish_word_on_stack();
  env.exitscope();
}

void plus_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);
  emit_copy_proto_into_ACC(Int, s);
  env.emit_pop_into2(FP, s);

  env.emit_push2(ACC, s);

  // Load operand (unboxed) integers into T1 and T2
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

  env.emit_pop_into2(ACC, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

  emit_add(T3, T1, T2, s);

  env.emit_pop_into2(ACC, s);

  emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
  // Result is in ACC
}

void sub_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);
  emit_copy_proto_into_ACC(Int, s);
  env.emit_pop_into2(FP, s);

  env.emit_push2(ACC, s);

  // Load operand (unboxed) integers into T1 and T2
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

  env.emit_pop_into2(ACC, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

  emit_sub(T3, T1, T2, s);

  env.emit_pop_into2(ACC, s);

  emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
  // Result is in ACC
}

void mul_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);
  emit_copy_proto_into_ACC(Int, s);
  env.emit_pop_into2(FP, s);

  env.emit_push2(ACC, s);

  // Load operand (unboxed) integers into T1 and T2
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

  env.emit_pop_into2(ACC, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

  emit_mul(T3, T1, T2, s);

  env.emit_pop_into2(ACC, s);

  emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
  // Result is in ACC
}

void divide_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);
  emit_copy_proto_into_ACC(Int, s);
  env.emit_pop_into2(FP, s);

  env.emit_push2(ACC, s);

  // Load operand (unboxed) integers into T1 and T2
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

  env.emit_pop_into2(ACC, s);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

  emit_div(T3, T1, T2, s);

  env.emit_pop_into2(ACC, s);

  emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
  // Result is in ACC
}

void neg_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  env.emit_push2(FP, s);
  emit_copy_proto_into_ACC(Int, s);
  env.emit_pop_into2(FP, s);

  env.emit_push2(ACC, s);

  // Load operand (unboxed) integer into T1
  e1->code(s, self_cls, env, cls_tbl);
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

  emit_neg(T2, T1, s);

  env.emit_pop_into2(ACC, s);

  emit_store(T2, DEFAULT_OBJFIELDS, ACC, s);
  // Result is in ACC
}

void lt_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                    CgenClassTableP cls_tbl) {
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

  env.emit_pop_into2(T3, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T3, s);

  int true_label = cls_tbl->get_new_label_num();
  int end_label = cls_tbl->get_new_label_num();

  emit_blt(T1, T2, true_label, s);

  emit_partial_load_address(ACC, s); falsebool.code_ref(s); s << endl;
  emit_branch(end_label, s);

  emit_label_def(true_label, s);
  emit_partial_load_address(ACC, s); truebool.code_ref(s); s << endl;

  emit_label_def(end_label, s);
}

void eq_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                    CgenClassTableP cls_tbl) {
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  // Move the result of e1 into T1, the result of e2 into T2.
  env.emit_pop_into2(T1, s);
  emit_move(T2, ACC, s);

  // Load TRUE into ACC, FALSE into A1
  emit_load_bool(ACC, truebool, s);
  emit_load_bool(A1, falsebool, s);
  
  // Compare T1 and T2
  //   1. By pointer by value. If this fails,
  //   2. Call equality_test. This will compare runtime types. If runtime
  //      types are different, it will return $a1. If runtime types are
  //      not Int, String or Bool, it will return $a1. Otherwise,
  //      compares values and returns $a0 if the same, $a1 if different,
  //      i.e. TRUE -> $a0 (ACC), FALSE -> $a1
  
  int end_cond_label_num = cls_tbl->get_new_label_num();

  emit_beq(T1, T2, end_cond_label_num, s);  // ACC already has TRUE
  
  emit_push(FP, s);
  emit_jal("equality_test", s);
  emit_pop_into(FP, s);

  emit_label_def(end_cond_label_num, s);
}

void leq_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  e1->code(s, self_cls, env, cls_tbl);
  env.emit_push2(ACC, s);
  e2->code(s, self_cls, env, cls_tbl);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

  env.emit_pop_into2(T3, s);
  emit_load(T1, DEFAULT_OBJFIELDS, T3, s);

  int true_label = cls_tbl->get_new_label_num();
  int end_label = cls_tbl->get_new_label_num();

  emit_bleq(T1, T2, true_label, s);

  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);

  emit_label_def(true_label, s);
  emit_load_bool(ACC, truebool, s);
  
  emit_label_def(end_label, s);
}

void comp_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  e1->code(s, self_cls, env, cls_tbl);
  emit_load_bool(T1, truebool, s);
  
  int negate_true_label = cls_tbl->get_new_label_num();
  int end_label = cls_tbl->get_new_label_num();

  emit_beq(ACC, T1, negate_true_label, s);

  // ACC is false, make it true
  emit_move(ACC, T1, s);
  emit_branch(end_label, s);

  emit_label_def(negate_true_label, s);
  // ACC is true, make it false
  emit_load_bool(ACC, falsebool, s);
  
  emit_label_def(end_label, s);
  // ACC holds result
}

void int_const_class::code(ostream& s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl)
{
  emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl)
{
  // SET REGISTER MASK FOR GARBAGE COLLECTOR (WHEN TESTING, WILL BE
  // CALLED ON EVERY MEMORY ALLOCATION)
  
  if (type_name == SELF_TYPE) {
    // Copy prototype object for the dynamic class of SELF
    emit_load(T3, 0, SELF, s);
    // 4 for word length, 2 for word offset
    emit_load_imm(T2, 2 * 4, s);
    emit_mul(T1, T2, T3, s);
    emit_load_address(T2, CLASSOBJTAB, s);
    emit_add(T1, T1, T2, s);
    emit_load(ACC, 0, T1, s);

    env.emit_push2(FP, s);
    emit_jal("Object.copy", s);
    env.emit_pop_into2(FP, s);
    // New copy of prototype object is in ACC

    // Run init procedure on new object copy
    emit_load(T3, 0, SELF, s);
    // 4 for word length, 2 for word offset
    emit_load_imm(T2, 2 * 4, s);
    emit_mul(T1, T2, T3, s);
    // add 1 word
    emit_addiu(T1, T1, 4, s);
    emit_load_address(T2, CLASSOBJTAB, s);
    emit_add(T1, T2, T1, s);
    emit_load(T2, 0, T1, s);

    env.emit_push2(FP, s);
    emit_jalr(T2, s);
    env.emit_pop_into2(FP, s);
    // Initialised new object is in ACC

  } else {
    CgenNodeP new_cgen_node = cls_tbl->lookup(type_name);
    if (new_cgen_node == NULL && cgen_debug) {
      cout << "BUG: could not find new type " << type_name << "!"
           << endl;
    }

    // Put address of prototype object into ACC, then copy and init
    emit_push(FP, s);
    emit_copy_proto_into_ACC(new_cgen_node, s);
    emit_pop_into(FP, s);
    // The new object is returned in ACC
  }
}

void isvoid_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  e1->code(s, self_cls, env, cls_tbl);
  emit_load_imm(T1, VOID_VAL, s);

  int true_label = cls_tbl->get_new_label_num();
  int end_label = cls_tbl->get_new_label_num();

  emit_beq(ACC, T1, true_label, s);

  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);

  emit_label_def(true_label, s);
  emit_load_bool(ACC, truebool, s);

  emit_label_def(end_label, s);
}

void no_expr_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                      CgenClassTableP cls_tbl) {
  if (cgen_debug) {
    cout << "BUG: trying to generate code for no_expr_class" << endl;
  }
}

void object_class::code(ostream &s, CgenNodeP self_cls, CGenEnv& env,
                        CgenClassTableP cls_tbl) {
  StoreLocation *loc = env.lookup(name);
  if (cgen_debug && loc == NULL) {
    cout << "BUG: could not find object " << name << "!!!" <<endl;
  }
  // if (cgen_debug) {
  //   cout << "In object_class::code, loaded '" << name
  //        << "' location " << (*loc) << endl;
  // }
  loc->emit_load_into(ACC, s);
}
