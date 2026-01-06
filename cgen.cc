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

// Global label counter
int labelnum = 0;
CgenClassTable *codegen_classtable = NULL;

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
  codegen_classtable = new CgenClassTable(classes,os);

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
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
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
      << WORD;


 /***** Add dispatch information for class String ******/
      emit_disptable_ref(Str, s);

      s << endl;                                              // dispatch table
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
      << WORD; 

 /***** Add dispatch information for class Int ******/
      emit_disptable_ref(Int, s);

      s << endl;                                          // dispatch table
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
      << WORD;

 /***** Add dispatch information for class Bool ******/
      emit_disptable_ref(Bool, s);

      s << endl;                                            // dispatch table
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


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   // We need to set the tags for basic classes
   intclasstag = probe(Int)->class_tag;
   stringclasstag = probe(Str)->class_tag;
   boolclasstag = probe(Bool)->class_tag;

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

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
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



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  code_class_nameTab();
  code_class_objTab();
  code_dispatchTabs();
  code_protObjs(str); // Helper call from GetClassNodes iteration

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  code_class_inits();
  code_class_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

// Re-implement GetClassNodes using DFS traversal from Object
void GetClassNodesDFS(CgenNode* node, std::vector<CgenNode*>& out_list) {
    if (!node) return;
    out_list.push_back(node);
    for (List<CgenNode>* l = node->get_children(); l; l = l->tl()) {
        GetClassNodesDFS(l->hd(), out_list);
    }
}

std::vector<CgenNode*> CgenClassTable::GetClassNodes() {
    if (m_class_nodes.empty()) {
        GetClassNodesDFS(root(), m_class_nodes);
        for(int i=0; i<(int)m_class_nodes.size(); ++i) {
            m_class_nodes[i]->class_tag = i;
            m_class_tags[m_class_nodes[i]->name] = i;
        }
    }
    return m_class_nodes;
}

void CgenClassTable::code_class_nameTab() {
    str << CLASSNAMETAB << LABEL;
    std::vector<CgenNode*> nodes = GetClassNodes();
    for (CgenNode* node : nodes) {
        str << WORD;
        stringtable.lookup_string(node->name->get_string())->code_ref(str);
        str << endl;
    }
}

void CgenClassTable::code_class_objTab() {
    str << CLASSOBJTAB << LABEL;
    std::vector<CgenNode*> nodes = GetClassNodes();
    for (CgenNode* node : nodes) {
        str << WORD; emit_protobj_ref(node->name, str); str << endl;
        str << WORD; emit_init_ref(node->name, str); str << endl;
    }
}

void CgenClassTable::code_dispatchTabs() {
    std::vector<CgenNode*> nodes = GetClassNodes();
    for (CgenNode* node : nodes) {
        emit_disptable_ref(node->name, str);
        str << LABEL;
        std::vector<method_class*> methods = node->GetFullMethods();
        std::map<Symbol, Symbol> class_map = node->GetDispatchClassTab();
        
        for (method_class* method : methods) {
             str << WORD;
             emit_method_ref(class_map[method->name], method->name, str);
             str << endl;
        }
    }
}

void CgenClassTable::code_protObjs(ostream& s) {
    std::vector<CgenNode*> nodes = GetClassNodes();
    for (CgenNode* node : nodes) {
        node->code_protObj(s);
    }
}

void CgenClassTable::code_class_inits() {
    std::vector<CgenNode*> nodes = GetClassNodes();
    for (CgenNode* node : nodes) {
        node->code_init(str);
    }
}

void CgenClassTable::code_class_methods() {
    std::vector<CgenNode*> nodes = GetClassNodes();
    for (CgenNode* node : nodes) {
        if (!node->basic()) {
            node->code_methods(str);
        }
    }
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
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

std::vector<CgenNode*> CgenNode::GetInheritance() {
    if (inheritance.empty()) {
        CgenNode* curr = this;
        while (curr && curr->name != No_class) {
            inheritance.push_back(curr);
            curr = curr->get_parentnd();
        }
        std::reverse(inheritance.begin(), inheritance.end());
    }
    return inheritance;
}

std::vector<attr_class*> CgenNode::GetFullAttribs() {
    if (m_full_attribs.empty()) {
        std::vector<CgenNode*> chain = GetInheritance();
        for (CgenNode* node : chain) {
            Features feats = node->features;
            for (int i = feats->first(); feats->more(i); i = feats->next(i)) {
                if (!feats->nth(i)->IsMethod()) {
                    m_full_attribs.push_back((attr_class*)feats->nth(i));
                }
            }
        }
        for (size_t i = 0; i < m_full_attribs.size(); ++i) {
            m_attrib_idx_tab[m_full_attribs[i]->name] = i;
        }
    }
    return m_full_attribs;
}

std::vector<method_class*> CgenNode::GetFullMethods() {
    if (m_full_methods.empty()) {
        std::vector<CgenNode*> chain = GetInheritance();
        for (CgenNode* node : chain) {
            Features feats = node->features;
            for (int i = feats->first(); feats->more(i); i = feats->next(i)) {
                if (feats->nth(i)->IsMethod()) {
                    method_class* m = (method_class*)feats->nth(i);
                    if (m_dispatch_idx_tab.find(m->name) == m_dispatch_idx_tab.end()) {
                        m_full_methods.push_back(m);
                        m_dispatch_idx_tab[m->name] = m_full_methods.size() - 1;
                        m_dispatch_class_tab[m->name] = node->name;
                    } else {
                        int idx = m_dispatch_idx_tab[m->name];
                        m_full_methods[idx] = m;
                        m_dispatch_class_tab[m->name] = node->name;
                    }
                }
            }
        }
    }
    return m_full_methods;
}

std::map<Symbol, int> CgenNode::GetAttribIdxTab() {
    if (m_attrib_idx_tab.empty()) GetFullAttribs();
    return m_attrib_idx_tab;
}
std::map<Symbol, int> CgenNode::GetDispatchIdxTab() {
    if (m_dispatch_idx_tab.empty()) GetFullMethods();
    return m_dispatch_idx_tab;
}
std::map<Symbol, Symbol> CgenNode::GetDispatchClassTab() {
    if (m_dispatch_class_tab.empty()) GetFullMethods();
    return m_dispatch_class_tab;
}
std::vector<attr_class*> CgenNode::GetAttribs() {
    std::vector<attr_class*> ret;
    for(int i=features->first(); features->more(i); i=features->next(i))
        if (!features->nth(i)->IsMethod()) ret.push_back((attr_class*)features->nth(i));
    return ret;
}
std::vector<method_class*> CgenNode::GetMethods() {
    std::vector<method_class*> ret;
    for(int i=features->first(); features->more(i); i=features->next(i))
        if (features->nth(i)->IsMethod()) ret.push_back((method_class*)features->nth(i));
    return ret;
}

void CgenNode::code_protObj(ostream& s) {
    s << WORD << "-1" << endl;
    emit_protobj_ref(name, s); s << LABEL;
    s << WORD << class_tag << endl;
    std::vector<attr_class*> attrs = GetFullAttribs();
    s << WORD << (DEFAULT_OBJFIELDS + attrs.size()) << endl;
    s << WORD; emit_disptable_ref(name, s); s << endl;
    
    for (attr_class* attr : attrs) {
        s << WORD;
        if (attr->type_decl == Int) {
            inttable.lookup_string("0")->code_ref(s);
        } else if (attr->type_decl == Bool) {
            falsebool.code_ref(s);
        } else if (attr->type_decl == Str) {
            stringtable.lookup_string("")->code_ref(s);
        } else {
            s << "0";
        }
        s << endl;
    }
}

void CgenNode::code_init(ostream& s) {
    emit_init_ref(name, s); s << LABEL;
    // Prologue
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);
    emit_move(SELF, ACC, s);
    
    if (parentnd->name != No_class) {
        s << JAL; emit_init_ref(parentnd->name, s); s << endl;
    }
    
    std::vector<attr_class*> attrs = GetAttribs();
    // Re-fetch mapping to ensure we have offsets
    std::map<Symbol, int> idx_tab = GetAttribIdxTab();
    
    for (attr_class* attr : attrs) {
        if (!attr->init->IsEmpty()) {
            CgenEnvironment env;
            env.m_class_node = this;
            attr->init->code(s, env);
            int idx = idx_tab[attr->name];
            emit_store(ACC, idx + 3, SELF, s);
            // GC Assign if needed?
            // emit_addiu(A1, SELF, 4*(idx+3), s);
            // emit_jal("_GenGC_Assign", s);
            if (cgen_Memmgr == 1) { // GenGC
                 emit_addiu(A1, SELF, 4 * (idx + 3), s);
                 emit_jal("_GenGC_Assign", s);
            }
        }
    }
    
    emit_move(ACC, SELF, s);
    // Epilogue
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, 12, s);
    emit_return(s);
}

void CgenNode::code_methods(ostream& s) {
    std::vector<method_class*> methods = GetMethods();
    for (method_class* method : methods) {
        emit_method_ref(name, method->name, s); s << LABEL;
        // Prologue
        emit_addiu(SP, SP, -12, s);
        emit_store(FP, 3, SP, s);
        emit_store(SELF, 2, SP, s);
        emit_store(RA, 1, SP, s);
        emit_addiu(FP, SP, 4, s);
        emit_move(SELF, ACC, s);
        
        CgenEnvironment env;
        env.m_class_node = this;
        // Add formals to environment
        for(int i = method->formals->first(); method->formals->more(i); i = method->formals->next(i)) {
            env.AddParam(((formal_class*)method->formals->nth(i))->GetName());
        }
        
        method->expr->code(s, env);
        
        // Epilogue
        emit_load(FP, 3, SP, s);
        emit_load(SELF, 2, SP, s);
        emit_load(RA, 1, SP, s);
        emit_addiu(SP, SP, 12, s);
        // Pop args
        int num_args = 0;
        for(int i=method->formals->first(); method->formals->more(i); i=method->formals->next(i)) num_args++;
        emit_addiu(SP, SP, num_args * 4, s);
        
        emit_return(s);
    }
}

// Env Lookup Helper
int CgenEnvironment::LookUpAttrib(Symbol s) {
    if (!m_class_node) return -1;
    std::map<Symbol, int> idxs = m_class_node->GetAttribIdxTab();
    if (idxs.find(s) != idxs.end()) return idxs[s];
    return -1;
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

void assign_class::code(ostream &s, CgenEnvironment env) {
    expr->code(s, env);
    
    int idx;
    if ((idx = env.LookUpVar(name)) != -1) {
        emit_store(ACC, idx + 1, SP, s);
        if (cgen_Memmgr == 1) {
             emit_addiu(A1, SP, 4 * (idx + 1), s);
             emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpParam(name)) != -1) {
        emit_store(ACC, idx + 3, FP, s);
         if (cgen_Memmgr == 1) {
             emit_addiu(A1, FP, 4 * (idx + 3), s);
             emit_jal("_GenGC_Assign", s);
        }
    } else if ((idx = env.LookUpAttrib(name)) != -1) {
        emit_store(ACC, idx + 3, SELF, s);
         if (cgen_Memmgr == 1) {
             emit_addiu(A1, SELF, 4 * (idx + 3), s);
             emit_jal("_GenGC_Assign", s);
        }
    }
}

void static_dispatch_class::code(ostream &s, CgenEnvironment env) {
    std::vector<Expression> actuals = GetActuals();
    for (Expression e : actuals) {
        e->code(s, env);
        emit_push(ACC, s);
        env.AddObstacle();
    }
    
    expr->code(s, env);
    
    int label_not_void = labelnum++;
    emit_bne(ACC, ZERO, label_not_void, s);
    emit_load_address(ACC, "str_const0", s); // file name usually
    emit_load_imm(T1, 1, s); // line number
    emit_jal("_dispatch_abort", s);
    emit_label_def(label_not_void, s);
    
    CgenNode* node = codegen_classtable->GetClassNode(type_name);
    std::string dispTab = std::string(type_name->get_string()) + DISPTAB_SUFFIX;
    emit_load_address(T1, (char*)dispTab.c_str(), s);
    
    int idx = node->GetDispatchIdxTab()[name];
    emit_load(T1, idx, T1, s);
    emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s, CgenEnvironment env) {
    std::vector<Expression> actuals = GetActuals();
    for (Expression e : actuals) {
        e->code(s, env);
        emit_push(ACC, s);
        env.AddObstacle();
    }
    
    expr->code(s, env);
    
    int label_not_void = labelnum++;
    emit_bne(ACC, ZERO, label_not_void, s);
    emit_load_address(ACC, "str_const0", s);
    emit_load_imm(T1, 1, s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(label_not_void, s);
    
    emit_load(T1, 2, ACC, s); // dispatch table
    
    // Determine method offset
    // The offset is determined by the compile-time type of expr.
    // However, for dynamic dispatch, we look up the method index 
    // in the class hierarchy of the *expression's static type*.
    Symbol type = expr->get_type();
    if (type == SELF_TYPE) type = env.m_class_node->name;
    
    CgenNode* node = codegen_classtable->GetClassNode(type);
    int idx = node->GetDispatchIdxTab()[name];
    
    emit_load(T1, idx, T1, s);
    emit_jalr(T1, s);
}

void cond_class::code(ostream &s, CgenEnvironment env) {
    pred->code(s, env);
    emit_fetch_int(T1, ACC, s);
    
    int label_else = labelnum++;
    int label_end = labelnum++;
    
    emit_beq(T1, ZERO, label_else, s);
    then_exp->code(s, env);
    emit_branch(label_end, s);
    emit_label_def(label_else, s);
    else_exp->code(s, env);
    emit_label_def(label_end, s);
}

void loop_class::code(ostream &s, CgenEnvironment env) {
    int label_loop = labelnum++;
    int label_end = labelnum++;
    
    emit_label_def(label_loop, s);
    pred->code(s, env);
    emit_fetch_int(T1, ACC, s);
    emit_beq(T1, ZERO, label_end, s);
    body->code(s, env);
    emit_branch(label_loop, s);
    emit_label_def(label_end, s);
    emit_move(ACC, ZERO, s); // Loop returns void
}

void typcase_class::code(ostream &s, CgenEnvironment env) {
    expr->code(s, env);
    
    int label_not_void = labelnum++;
    int label_end = labelnum++;
    
    emit_bne(ACC, ZERO, label_not_void, s);
    emit_load_address(ACC, "str_const0", s);
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort2", s);
    emit_label_def(label_not_void, s);
    
    emit_load(T1, 0, ACC, s); // T1 = class tag
    
    // We need to iterate over cases and find the closest ancestor.
    // PA5 trick: We can generate code to check each case.
    // To handle "closest", we should ideally sort branches by hierarchy depth descending?
    // Or, we check each branch. If it matches, we record the "distance".
    // MIPS implementation of finding closest match is tricky.
    // Alternative: Iterate branches. If tag matches branch_type (subclass check), 
    // jump to code block. BUT cool case requires *best* match.
    // So we must check *all* branches and pick best.
    
    // Easier approach used in simple compilers:
    // Sort the cases by type depth (most specific first).
    // Then the first match is the best match.
    std::vector<branch_class*> cases = GetCases();
    std::sort(cases.begin(), cases.end(), [](branch_class* a, branch_class* b) {
        // We need codegen_classtable to check depth.
        // Assuming higher tag = deeper is FALSE.
        // We need depth info. Let's assume codegen_classtable is available.
        int d1 = 0, d2 = 0;
        CgenNode* n1 = codegen_classtable->GetClassNode(a->type_decl);
        CgenNode* n2 = codegen_classtable->GetClassNode(b->type_decl);
        while(n1->name != Object) { d1++; n1 = n1->get_parentnd(); }
        while(n2->name != Object) { d2++; n2 = n2->get_parentnd(); }
        return d1 > d2; 
    });
    
    // For each sorted case, check inheritance
    for (branch_class* b : cases) {
        int label_next_case = labelnum++;
        int tag = codegen_classtable->GetClassTags()[b->type_decl];
        
        // Check if T1 (obj tag) is subclass of 'tag'
        // This requires a runtime check or a range check if we did DFS tagging.
        // Since we did simple tagging, we might not have ranges.
        // Standard COOL Runtime has a class_objTab. We can traverse parents up.
        // Or, we can use the tagging if we implemented DFS range.
        
        // Simple inefficient way compatible with any tagging:
        // Traverse parent pointers of T1 until we hit 'tag' or Object.
        // This is slow in MIPS.
        // The skeleton suggests we should rely on what we have.
        // We assigned tags 0..N.
        
        // Let's implement the specific logic:
        // Range Check is best if tags are DFS.
        // With DFS: [tag, tag + max_child_index].
        // CgenClassTable DFS implementation ensures this property IF we track subtree range.
        
        // Let's assume standard linear tagging doesn't support range check easily without a table.
        // But wait, `_case_abort` exists.
        
        // Let's emit code:
        // blt T1, min_tag, next
        // bgt T1, max_tag, next
        // execute branch
        
        // Calculate min/max tag for this type
        CgenNode* p = codegen_classtable->GetClassNode(b->type_decl);
        int min_tag = codegen_classtable->GetClassTags()[p->name];
        int max_tag = min_tag;
        // Find max tag in subtree (needs helper, but let's assume simple approach)
        // Actually, we can just do a helper function to find max tag.
        std::vector<CgenNode*> nodes = codegen_classtable->GetClassNodes();
        // Since nodes are DFS ordered in my implementation:
        // Subtree is contiguous range.
        for(size_t i = min_tag + 1; i < nodes.size(); ++i) {
             // Check if nodes[i] is descendant of p
             CgenNode* curr = nodes[i];
             bool is_desc = false;
             while(curr) {
                 if (curr == p) { is_desc = true; break; }
                 curr = curr->get_parentnd();
             }
             if (!is_desc) break; // End of contiguous range
             max_tag = i;
        }
        
        emit_blti(T1, min_tag, label_next_case, s);
        emit_bgti(T1, max_tag, label_next_case, s);
        
        // Match found!
        emit_push(ACC, s); // Bind variable
        env.EnterScope();
        env.AddVar(b->name);
        b->expr->code(s, env);
        env.ExitScope();
        emit_addiu(SP, SP, 4, s); // Pop variable
        emit_branch(label_end, s);
        
        emit_label_def(label_next_case, s);
    }
    
    emit_jal("_case_abort", s); // No match
    emit_label_def(label_end, s);
}

void block_class::code(ostream &s, CgenEnvironment env) {
    for (int i = body->first(); body->more(i); i = body->next(i))
        body->nth(i)->code(s, env);
}

void let_class::code(ostream &s, CgenEnvironment env) {
    init->code(s, env);
    if (init->IsEmpty()) {
        if (type_decl == Str) {
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        } else if (type_decl == Int) {
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        } else if (type_decl == Bool) {
            emit_load_bool(ACC, BoolConst(0), s);
        } else {
            emit_move(ACC, ZERO, s);
        }
    }
    emit_push(ACC, s);
    env.EnterScope();
    env.AddVar(identifier);
    body->code(s, env);
    env.ExitScope();
    emit_addiu(SP, SP, 4, s);
}

void plus_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_move(T2, ACC, s);
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    emit_add(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void sub_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_move(T2, ACC, s);
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    emit_sub(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void mul_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_move(T2, ACC, s);
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    emit_mul(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void divide_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_move(T2, ACC, s);
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, T2, s);
    emit_div(T3, T1, T2, s);
    emit_store(T3, 3, ACC, s);
}

void neg_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_jal("Object.copy", s);
    emit_load(T1, 3, ACC, s);
    emit_neg(T1, T1, s);
    emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, ACC, s); // ACC has e2
    
    int label_true = labelnum++;
    int label_end = labelnum++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_blt(T1, T2, label_end, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_end, s);
}

void eq_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_move(T2, ACC, s);
    
    int label_end = labelnum++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, T2, label_end, s);
    emit_load_bool(A1, BoolConst(0), s);
    emit_jal("equality_test", s); // runtime helper
    emit_label_def(label_end, s);
}

void leq_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_push(ACC, s);
    env.AddObstacle();
    e2->code(s, env);
    emit_load(T1, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_load(T1, 3, T1, s);
    emit_load(T2, 3, ACC, s);
    
    int label_end = labelnum++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_bleq(T1, T2, label_end, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_end, s);
}

void comp_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_load(T1, 3, ACC, s);
    int label_end = labelnum++;
    emit_load_bool(ACC, BoolConst(1), s);
    emit_beq(T1, ZERO, label_end, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_end, s);
}

void int_const_class::code(ostream& s, CgenEnvironment env)  
{
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenEnvironment env)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenEnvironment env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenEnvironment env) {
    if (type_name == SELF_TYPE) {
        emit_load_address(T1, "class_objTab", s);
        emit_load(T2, 0, SELF, s); // class tag
        emit_sll(T2, T2, 3, s); // * 8
        emit_addu(T1, T1, T2, s);
        emit_push(T1, s);
        emit_load(ACC, 0, T1, s); // load protObj
        emit_jal("Object.copy", s);
        emit_load(T1, 1, SP, s);
        emit_addiu(SP, SP, 4, s);
        emit_load(T1, 1, T1, s); // load init
        emit_jalr(T1, s);
    } else {
        std::string prot = std::string(type_name->get_string()) + PROTOBJ_SUFFIX;
        std::string init = std::string(type_name->get_string()) + CLASSINIT_SUFFIX;
        emit_load_address(ACC, (char*)prot.c_str(), s);
        emit_jal("Object.copy", s);
        emit_jal((char*)init.c_str(), s);
    }
}

void isvoid_class::code(ostream &s, CgenEnvironment env) {
    e1->code(s, env);
    emit_move(T1, ACC, s);
    emit_load_bool(ACC, BoolConst(1), s);
    int label_end = labelnum++;
    emit_beq(T1, ZERO, label_end, s);
    emit_load_bool(ACC, BoolConst(0), s);
    emit_label_def(label_end, s);
}

void no_expr_class::code(ostream &s, CgenEnvironment env) {
    // No code needed, usually handled by caller or returns void
    emit_move(ACC, ZERO, s);
}

void object_class::code(ostream &s, CgenEnvironment env) {
    if (name == self) {
        emit_move(ACC, SELF, s);
        return;
    }
    
    int idx;
    if ((idx = env.LookUpVar(name)) != -1) {
        emit_load(ACC, idx + 1, SP, s);
    } else if ((idx = env.LookUpParam(name)) != -1) {
        emit_load(ACC, idx + 3, FP, s);
    } else if ((idx = env.LookUpAttrib(name)) != -1) {
        emit_load(ACC, idx + 3, SELF, s);
    }
}
