# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this file,
# You can obtain one at http://mozilla.org/MPL/2.0/.
#
# Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

import re
import datetime
import os
import subprocess

class Node:
    def __init__(self, name=None, kind=None, generic=None, children=None):
        self.name = name
        self.kind = kind
        self.children = list() if children is None else children
        self.generic = generic
        self.generic_variants = list()

def to_snake_case(name):
    s = re.sub("[<>]", "", name)
    s = re.sub("(.)([A-Z][a-z]+)", r"\1_\2", s)
    return re.sub("([a-z0-9])([A-Z])", r"\1_\2", s).lower()


# Types that should not have the Visit traits implemented
no_visit = [
    # Rust built in types
    "bool",
    "u8",
    "u32",
    "Option<u32>",
    "i64",
    "f64",
    "usize",
    "Latin1String",
    "Arc<Latin1String>",

    # Custom types
    "BaseSpecifier",
    "Unary",
    "Binary",
    "ExternalObjectClass"
]

# Parse all AST nodes
script_dir = os.path.dirname(os.path.realpath(__file__))
with open(os.path.join(script_dir, "../src/ast/mod.rs")) as f:
    lines = f.readlines()
modrs = list()
for line in lines:
    l = line.strip()
    if not (len(l) == 0 or re.match("^(mod|use|pub use|/|#)",l)):
        l = l.split("//")[0].strip()
        modrs.append(l)

nodes = list()
nodes.append(Node(name="Symbol", kind="struct", generic="", children=["id:usize","name:Arc<Latin1String>"]))
i = iter(modrs)
for l in i:
    node = Node()
    match = re.match(r"pub (enum|struct|type) ([a-zA-Z]+)\s*(<(.*?)>)?", l)
    node.kind = match.group(1)
    node.name = match.group(2)
    node.generic = match.group(3) if match.group(3) else ""
    if node.name in no_visit:
        while node.kind != "type" and next(i) != "}" : pass
        continue
    elif node.kind == "enum":
        while True:
            l = next(i)
            if l == "}": break
            match = re.match(r"([a-zA-Z]+)(\((.*?)\))?", l)
            child = match.group(1)
            if match.group(2):
                child += ":" + match.group(2)[1:-1].replace(" ", "")
                
            node.children.append(child)
    elif node.kind == "struct":
        while True:
            l = next(i)
            if l == "}": break
            match = re.match(r"pub ([a-zA-Z_]+)\s*:\s*(\S+)", re.sub(",$", "", l))
            child = match.group(1) + ":" + match.group(2)
            node.children.append(child)
    elif node.kind == "type":
        node.children.append(l.split("=")[1].strip().strip(";"))
    else:
        print("ERROR!!!")
        exit(1)
    nodes.append(node)



# Expand all nodes with generics, assumes single generic for the type
node_exp = list()
for node in nodes:
    if node.generic != "":
        # Find all references to the node
        gens = dict()
        generic_variants = set()
        for n in nodes:
            for child in n.children:
                match = re.search(r"%s<((.*?))>" % node.name, child)
                if match and match.group(1):
                    gen_type = match.group(1)
                    if "<" in gen_type:
                        gen_type += ">"
                    generic_variants.add(gen_type)
                    gen_node = Node(name=node.name + "<" + gen_type + ">", kind=node.kind, generic="")
                    gen_node.children = node.children
                    gens[gen_node.name] = gen_node
        node.generic_variants = list(generic_variants)
        for name, gen_node in gens.items():
            node_exp.append(gen_node)
    else:
        node_exp.append(node)

# Expand generics in node variants
resolved = False
while not resolved:
    resolved = True
    for node in nodes:
        if "T" in node.generic_variants:
            for parent in nodes:
                for child in parent.children:
                    if node.name + "<T" in child:
                        node.generic_variants.remove("T")
                        for pg in parent.generic_variants:
                            if pg not in node.generic_variants:
                                node.generic_variants.append(pg)
                        if "T" in node.generic_variants:
                            resolved = False

    

nodes_expanded = list()
for node in nodes:
    if len(node.generic_variants) == 0:
        nodes_expanded.append(node)
    else:
        for g in node.generic_variants:
            nodes_expanded.append(Node(name=node.name + "<" + g + ">", kind=node.kind, generic="", children = node.children))

# Find expanded nodes which are used as types
types = dict()
for node in nodes_expanded:
    if node.kind == "type":
        types[node.children[0]] = node.name
nodes_expanded = [x for x in nodes_expanded if x.kind != "type"]
for node in nodes_expanded:
    if node.name in types.keys():
        node.name = types[node.name]


# ===================================================
# Write file
# ---------------------------------------------------
outfile = 'visitor.rs'
with open(outfile, 'w') as f:

    f.write("""\
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) {year}, Olof Kraigher olof.kraigher@gmail.com

// This file is generated by the /vhdl_parser/scripts/generate_visitor.py python script
// and should not be edited manually.

use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Borrow;

use crate::message::Message;
use crate::ast::*;
use crate::source::{{WithPos, SrcPos}};
use crate::symbol_table::Symbol;

#[allow(unused_variables)] // Default implementations for events are empty and do not use any parameters
""".format(year=datetime.datetime.now().year))
    
    visitor_trait = """\
trait Visitor<'a, 'b> {
    
    // ===================================
    // Results
    // -----------------------------------
    fn messages(&self) -> &Vec<Message>;

    // ===================================
    // Context
    // -----------------------------------
    fn set_context_provider(&mut self, context_provider: &'b AstContextProvider){}
    fn enter_src_pos(&mut self, src_pos: &'b SrcPos){}
    fn exit_src_pos(&mut self, src_pos: &'b SrcPos){}

    // ===================================
    // AST events
    // -----------------------------------
"""
    for node in nodes_expanded:
        fn = "    fn {{prefix}}_{name}(&mut self, node: &{type}){brackets}\n".format(name=to_snake_case(node.name), type=node.name, brackets="{{}}")
        visitor_trait += fn.format(prefix="enter")
        visitor_trait += fn.format(prefix="exit")
    visitor_trait += "}"
    f.write("pub " + visitor_trait + "\n")
    visit_trait = """
trait Visit<'a,'b> {
    fn visit<T: Visitor<'a, 'b>>(&'b self, visitor : &mut T);
}"""
    f.write(visit_trait)
    f.write("""

impl<'a,'b,U> Visit<'a,'b> for Vec<U>
where U: Visit<'a, 'b> { 
    fn visit<T: Visitor<'a, 'b>>(&'b self, visitor : &mut T) {
        for node in self {
            node.visit(visitor);
        }
    }
}

impl<'a,'b,U,V> Visit<'a,'b> for Vec<(U,V)> 
where U: Visit<'a,'b>,
      V: Visit<'a,'b>
{
    fn visit<T: Visitor<'a,'b>>(&'b self, visitor : &mut T) {
        for nodes in self {
            nodes.0.visit(visitor);
            nodes.1.visit(visitor);
        }
    }
}

impl<'a,'b, U> Visit<'a,'b> for WithPos<U>
where U: Visit<'a,'b> {
    fn visit<T: Visitor<'a,'b>>(&'b self, visitor : &mut T) {
        visitor.enter_src_pos(&self.pos);
        self.item.visit(visitor);
        visitor.exit_src_pos(&self.pos);
    }
}

impl<'a,'b, U> Visit<'a, 'b> for Option<U>
where U: Visit<'a, 'b> {
    fn visit<T: Visitor<'a, 'b>>(&'b self, visitor : &mut T) {
        match self {
            Some(node) => node.visit(visitor),
            None => (),
        }
    }
}

impl<'a,'b, U> Visit<'a,'b> for Box<U>
where U: Visit<'a,'b> {
    fn visit<T: Visitor<'a,'b>>(&'b self, visitor : &mut T) {
        let node : &U = self.borrow();
        node.visit(visitor);
    }
}

trait AstContextProvider {
    fn pos(&self) -> Option<&SrcPos>;
}

struct NoneContextProvider {}

impl AstContextProvider for NoneContextProvider {
    fn pos(&self) -> Option<&SrcPos> {
        None
    }
}

pub struct AstWalker<'a, 'b> {
    messages: Vec<Message>,
    visitors: Vec<Box<&'a mut Visitor<'a, 'b>>>,
    context:  Rc<AstContextProvider>,
}

impl<'a, 'b> AstWalker<'a, 'b> {
    pub fn new() -> Self {
        AstWalker {
            messages: Vec::new(),
            visitors: Vec::new(),
            src_pos: None,
        }
    }
    pub fn register<T: Visitor<'a, 'b>>(&mut self, visitor: &'a mut T) {
        self.visitors.push(Box::new(visitor));
    }
}

impl<'a, 'b> AstContextProvider for AstWalker<'a, 'b> {
    fn pos(&self) -> Option<&SrcPos> {
        self.src_pos
    }
}

""")
    ast_visitor = visitor_trait.replace("trait Visitor<'a, 'b> {", "impl<'a,'b> Visitor<'a,'b> for AstWalker<'a,'b> {")
    ast_visitor = re.sub(r"(fn messages\(&self\) -> &Vec<Message>);",
                         r"\1 {\n        &self.messages\n    }",
                         ast_visitor)
    ast_visitor = re.sub(r"(enter|exit)(_src_pos\(&mut self, src_pos: &'b SrcPos\)){}",
                         r"\1\2 {\n        self.src_pos = Some(src_pos);\n    }",
                         ast_visitor)
    ast_visitor = re.sub(r"(enter|exit)(_[a-z_]+)(\(&mut self, node: &)([A-Za-z<>]+\)){}",
                         """\\1\\2\\3\\4 {
        for visitor in &mut self.visitors {
            visitor.\\1\\2(node);
        }
    }""",
                         ast_visitor)
    f.write(ast_visitor + "\n")

    for node in nodes_expanded:
        if node.name in no_visit: continue
        f.write("\nimpl<'a,'b> Visit<'a,'b> for " + node.name + " {\n")
        f.write("    fn visit<T: Visitor<'a,'b>>(&'b self, visitor : &mut T) {\n")
        f.write("        visitor.enter_" + to_snake_case(node.name) + "(self);\n")
        if node.kind == "enum":
            f.write("        match self {\n")
            for e in node.children:
                esplit = e.split(":")
                child_name = esplit[0]
                qname = node.name.split("<")[0] + "::" + child_name
                if qname == "DelayMechanism::Inertial": # Currently only enum with anon struct
                    f.write("        DelayMechanism::Inertial{reject} => reject.visit(visitor),\n")
                elif len(esplit) > 1:
                    values = esplit[1].split(",")
                    if len(values) == 1:
                        val = values[0]
                        if val in no_visit:
                            f.write("            " + qname + "(..) => (),\n")
                        else:
                            f.write("            " + qname + "(ref node) => node.visit(visitor),\n")
                    else:
                        variables = list()
                        for ind in range(0, len(values)):
                            if values[ind] not in no_visit:
                                variables.append("ref node" + str(ind))
                            else:
                                variables.append("ref _node" + str(ind))
                        f.write("            " + qname + "({variables}) => {{\n".format(variables = ", ".join(variables)))
                        for ind in range(0, len(values)):
                            if values[ind] not in no_visit:
                                f.write("                node{ind}.visit(visitor);\n".format(ind = ind))
                        f.write("            }\n")
                else:
                    f.write("            " + qname + " => (),\n")
            f.write("        }\n")
        else:
            for child in node.children:
                child_name, child_type = child.split(":")
                if child_type not in no_visit:
                    f.write("        self." + child_name.strip() + ".visit(visitor);\n")

        f.write("        visitor.exit_" + to_snake_case(node.name) + "(self);\n    }\n}\n")

    f.write("""










// =====================================================================
// TESTS
// ---------------------------------------------------------------------
#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use crate::design_unit::parse_design_file;
    use crate::test_util::{check_no_messages, Code};

    fn parse_str(code: &str) -> (Code, DesignFile, Vec<Message>) {
        let code = Code::new(code);
        let mut messages = vec![];
        let design_file = code.with_stream(|stream| parse_design_file(stream, &mut messages));
        (code, design_file, messages)
    }

    fn parse_ok(code: &str) -> (Code, DesignFile) {
        let (code, design_file, messages) = parse_str(code);
        check_no_messages(&messages);
        (code, design_file)
    }


    struct TestVisitor {
        messages         : Vec<Message>,
        visits           : HashMap<String, u32>,
        context_provider : AstContextProvider,
    }

    impl<'a> TestVisitor {
        pub fn new() -> Self {
            let mut visits = HashMap::new();
""")
    for node in nodes_expanded:
        f.write('''\
            visits.insert(String::from("enter_{snake_name}"), 0);
            visits.insert(String::from("exit_{snake_name}"), 0);
'''.format(snake_name=to_snake_case(node.name)))

    f.write("""
            TestVisitor {
                messages: Vec::new(),
                visits: visits
            }
        }

        fn visits(self) -> HashMap<String, u32> {
            self.visits
        }
    }

""")
    f.write("    #[allow(unused_variables)]\n")
    test_visitor = visitor_trait.replace("trait Visitor<'a, 'b> {", "impl<'a,'b> Visitor<'a,'b> for TestVisitor {")
    test_visitor = re.sub(r"(fn messages\(&self\) -> &Vec<Message>);",
                         r"\1 {\n        &self.messages\n    }",
                         test_visitor)
    test_visitor = re.sub(r"(enter|exit)(_[a-z_]+)(\(&mut self, node: &)([A-Za-z<>]+\)){}",
                         """\\1\\2\\3\\4 {
        *self.visits.get_mut("\\1\\2").unwrap() += 1;
    }""",
                         test_visitor)
    for l in test_visitor.splitlines():
        f.write("    " + l + "\n")



    f.write(r"""


    #[test]
    fn listen_to_design_file() {
        
        #[allow(unused_mut)]
        let (_code, mut design_file) = parse_ok(
            "

package mypkg is
    generic(
        file file_gen : text; 
        pkg_gen : boolean;
        type gen_type;
        function gen_func(int : integer) return integer is <>;
        procedure gen_proc is def_proc
    );
    function myfunc(int : integer range 0 to 1) return integer;
    procedure myprocedure(constant i : integer; signal io : inout integer; variable o : out integer);
    type CAPACITY is range 0 to 1E5 units
        pF; -- picofarad
        nF = 1000 pF; -- nanofarad
        uF = 1000 nF; -- microfarad
        mF = 1000 uF; -- milifarad
        F = 1000 mF; -- farad
    end units CAPACITY;
end;

package mypkg_inst is new mypkg generic map (true);

context mycontext is
    library ieee;
    use ieee.numeric_std.all;
end;

context work.mycontext;
package body mypkg is
    
    function myfunc(int : integer range 0 to 1) return integer is
        variable retval : integer;
    begin
        retval := (int + 1) mod 2;
        return retval;
    end function myfunc;

    procedure myprocedure(constant i : integer; signal io : inout integer; signal o : out integer) is
    begin
        o <= i + io;
    end;

end;


entity mycomp is
    generic (
        N : integer := 2;
        package p is new work.p_template generic map (pkg_gen => N = 2)
    );
    port(i: in std_logic;
         o: out std_logic);
end entity mycomp;

architecture rtl of mycomp is
begin
    passthrough: i <= o;
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
entity myent2 is
    generic(gen : boolean := false);
    port(i : in std_logic; o : out std_logic);
end entity;

architecture rtl of myent2 is

    type super_mytype is record
        rec_element : std_ulogic_vector;
        rec_element2 :  std_ulogic;
    end record;
    type super_ultra_mytype is record
        s : (rec_element resolved, rec_element2 resolved) super_mytype;
    end record;
    type mytype;
    subtype mytype is mytype(
        rec_element(3 downto 0)
    );
    type mytype_acc is access mytype;
    type enum is (ett, tva);
    type prot is protected
        impure function get return integer;
    end protected;
    type prot is protected body
        variable const : integer := 1+2;
        impure function get return integer is
        begin
            return const;
        end function;
    end protected body;
    type myarr is array(0 to 4) of integer;
    
    signal mysignal : mytype := (rec_element => std_ulogic_vector'(\"1111\") and 4x\"F\");
    signal inte : integer;
    
    shared variable myvariable : mytype;
    
    attribute myattr : string;
    attribute myattr of mysignal : signal is \"hello world\";

    component mycomp is
        port(i: in std_logic;
             o: out std_logic);
    end component mycomp;

    alias alias1 is << signal .tb.uut.o_n : std_logic >>;  -- hierarchical signal name
    --alias alias2 is << signal ^.^.a : std_logic >>;        -- signal a two levels above
    alias alias3 is << variable @lib.pack.v : bit >>;      -- variable in a package pack
    alias myfunc_a is myfunc [integer return integer];

    for all : mycomp use entity work.mycomp(rtl);

    file myfile : text open read_mode is \"temp.txt\";

begin
    assert true report \"assertion\" severity warning;
    conditional: o <= i when i else '0';
    with i select o <= '0' when '0', '1' when others;
    myprocedure(2, int, int);
    inte <= reject 5 ns inertial myfunc(to_integer(unsigned(mysignal.rec_element(0))));
    mysignal.rec_element <= std_logic_vector(to_unsigned(inte, mysignal.rec_element'length));
    myblock : block is 
    begin
        mycomp_i : mycomp
        port map (
            i => '0',
            o => open
        );
    end block;

    myproc: process(i) is
        variable var : std_logic;
        variable int : integer;
        variable mytype_acc_var : mytype_acc;
    begin
        report \"report\" severity warning;
        mytype_acc_var := new mytype;
        case gen is
            when true => var := '1';
            when false => var := '0';
        end case;
        o <= '1' when gen = true else '0' when gen = false else '0';
        var := '1' when gen = true else '0' when gen = false else '0';
        with gen select var := '1' when true, '0' when others;
        with gen select o <= '1' when true, '0' when others;
        if gen then
            o <= '1';
        elsif not gen then
            o <= '0';
        else
            o <= '1';
        end if;
        o <= var after 4 ns, '0' after 10 ns;
        cond: o <= i when i else '0';
        int := myfunc(2);
        myprocedure(2, int, int);

        wait for 3 ns;

        loop
            exit;
        end loop;

        while i = '0' loop
            next;
        end loop;

        for i in 0 to 1 loop
        end loop;

    end process;

    case_gen : case gen generate
        when alt1: true =>
                signal s : integer;
            begin 
                o <= '1';
        when other: others =>
            o <= '0';
    end generate;

    for_gen : for i in 0 to 3 generate
        o <= unaffected;
    end generate;

    if_gen : if gen generate
        o <= 'Z';
    end generate;

end;

library ieee;
configuration mycnf of myent2 is
    use ieee.numeric_std.all;
    for rtl
        for mycomp_i : mycomp use entity work.mycomp(rtl); use vunit myvunit; end for;
    end for;
end configuration mycnf;

",
        );
        let mut test_visitor = TestVisitor::new();
        {
            let mut walker = AstWalker::new();
            walker.register(&mut test_visitor);
            design_file.visit(&mut walker);
        }
        design_file.visit(&mut walker);
        let mut missed = Vec::new();
        for (k, v) in test_visitor.visits() {
            if v == 0 {
                if !(k == "enter_port_clause" || k == "exit_port_clause") { // PortClause is not used in ast mod
                    missed.push(k);
                }
            }
        }
        assert!(
            missed.len() == 0,
            format!("Not all events were called, missed the following {} events: {}", missed.len(), missed.join(", "))
        )
    }

""")
    f.write("}")



# Apply rustfmt
print(subprocess.check_output(['rustfmt', outfile]))