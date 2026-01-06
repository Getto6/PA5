#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <map>
#include <algorithm>

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

extern Symbol No_type; // Fixed: Added external declaration

// Add Environment for PA5
class CgenEnvironment {
private:
    SymbolTable<Symbol, int> *vars;
    int param_offset_base; 
    int var_offset_base;   
    int cur_param_offset;
    int cur_var_offset;
    
public:
    CgenNode* m_class_node;

    CgenEnvironment() {
        vars = new SymbolTable<Symbol, int>();
        vars->enterscope();
        param_offset_base = 3; 
        var_offset_base = 0; 
    }

    void EnterScope() { vars->enterscope(); }
    void ExitScope() { vars->exitscope(); }

    void AddVar(Symbol sym) {
        vars->addid(sym, new int(0)); 
    }
    
    std::vector<Symbol> var_stack;
    
    void AddVarNode(Symbol s) {
        var_stack.push_back(s);
    }
    
    void RemoveVarNode() {
        var_stack.pop_back();
    }
    
    int LookUpVar(Symbol s) {
        for (int i = var_stack.size() - 1; i >= 0; --i) {
            if (var_stack[i] == s) return var_stack.size() - 1 - i;
        }
        return -1;
    }

    std::vector<Symbol> param_stack;
    void AddParam(Symbol s) {
        param_stack.push_back(s);
    }
    
    int LookUpParam(Symbol s) {
        for (int i = 0; i < (int)param_stack.size(); ++i) { // Fixed: Cast to int
            if (param_stack[i] == s) return param_stack.size() - 1 - i;
        }
        return -1;
    }
    
    int LookUpAttrib(Symbol s);
    void AddObstacle() { var_stack.push_back(No_type); } // Fixed: No_type now declared
};

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

   std::vector<CgenNode*> m_class_nodes;
   std::map<Symbol, int> m_class_tags;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   
   void code_class_nameTab();
   void code_class_objTab();
   void code_dispatchTabs();
   void code_class_inits();
   void code_class_methods();
   void code_protObjs(ostream& s); // Fixed: Added declaration

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   CgenNode* GetClassNode(Symbol name) { return probe(name); }
   std::vector<CgenNode*> GetClassNodes();
   std::map<Symbol, int> GetClassTags() { return m_class_tags; }
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   
   std::vector<CgenNode*> inheritance;
   std::vector<attr_class*> m_full_attribs;
   std::map<Symbol, int> m_attrib_idx_tab;
   
   std::vector<method_class*> m_full_methods;
   std::map<Symbol, int> m_dispatch_idx_tab;
   std::map<Symbol, Symbol> m_dispatch_class_tab; // method name -> class name

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   
   int class_tag;
   
   std::vector<CgenNode*> GetInheritance();
   std::vector<attr_class*> GetFullAttribs();
   std::map<Symbol, int> GetAttribIdxTab();
   
   std::vector<method_class*> GetFullMethods();
   std::map<Symbol, int> GetDispatchIdxTab();
   std::map<Symbol, Symbol> GetDispatchClassTab();
   
   std::vector<attr_class*> GetAttribs();
   std::vector<method_class*> GetMethods();
   
   void code_protObj(ostream& s);
   void code_init(ostream& s);
   void code_methods(ostream& s);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};
