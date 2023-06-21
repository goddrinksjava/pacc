use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::symbols::{Expr, Funcion, Tipo, Variable, VariableDecl};

pub type AstNodeIndex = usize;

pub static GLOBAL_SCOPE_NODE: AstNodeIndex = 0;

#[derive(Debug, Clone)]
pub struct NodeData {
    pub name: String,
    pub expr: Option<Expr>,
    pub type_: Tipo,
}

#[derive(Debug, Default)]
pub struct AstNode {
    data: HashMap<String, NodeData>,
    outgoing_edges: Vec<AstNodeIndex>,
    incoming_edge: Option<AstNodeIndex>,
}

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub var_names: Vec<String>,
    pub functions: Vec<Funcion>,
}

impl Ast {
    pub fn new() -> Ast {
        Ast {
            nodes: vec![AstNode::default()], //nodo global
            var_names: Vec::new(),
            functions: Vec::new(),
        }
    }
    //se le pasa el padre
    pub fn new_node(&mut self, lexical_scope_index: AstNodeIndex) -> AstNodeIndex {
        let index = self.nodes.len(); // tamaño de los nodos

        let node = AstNode {
            data: HashMap::new(),
            outgoing_edges: Vec::new(),
            incoming_edge: Some(lexical_scope_index),
        };

        self.nodes.push(node);

        self.nodes[lexical_scope_index].outgoing_edges.push(index);

        index //devolvemos Identificador
    }

    //bucar variable y conocer su tipo y su nombre del codigo de PL0, ya que es diferente
    // entero a;
    // nombre de a en pl0 podria ser: a+var_int@1
    pub fn query_var(&self, node_index: AstNodeIndex, k: &str) -> Option<&NodeData> {
        self.nodes[node_index]
            .data
            .get(&k.chars().take_while(|&ch| ch != '+').collect::<String>())
            .or_else(|| {
                self.nodes[node_index]
                    .incoming_edge
                    .and_then(|parent_index| self.query_var(parent_index, k))
            })
    }

    //agregar variable
    pub fn add_var(&mut self, node_index: AstNodeIndex, v: &VariableDecl) -> Result<(), ()> {
        if self.nodes[node_index].data.get(&v.variable.name).is_some() {
            // Si la variable ya esta definida dentro del nodo, devuelve error.
            Err(())
        } else {
            // nombre+tipo
            let name = format!(
                "{}+{}",
                v.variable.name,
                match v.variable.var_type.clone() {
                    crate::symbols::Tipo::Nulo => unreachable!(),
                    crate::symbols::Tipo::Entero => "var_int",
                    crate::symbols::Tipo::Decimal => "var_dec",
                    crate::symbols::Tipo::Palabra => "var_str",
                    crate::symbols::Tipo::Logico => "var_bool",
                    crate::symbols::Tipo::Lista(t) => match *t {
                        Tipo::Nulo => unreachable!(),
                        Tipo::Entero => "pointer_int",
                        Tipo::Decimal => "pointer_dec",
                        Tipo::Palabra => "pointer_str",
                        Tipo::Logico => "pointer_bool",
                        Tipo::Lista(_) => unimplemented!(),
                    },
                },
            );

            // cantidad de variables definidas
            let count = self
                .var_names
                .iter()
                .find(|&s| *s.split('@').nth(0).unwrap() == name)
                .map(|name| name.split('@').nth(1).unwrap().parse::<u32>().unwrap() + 1)
                .unwrap_or(1);

            // nombre+tipo@cantidad de variables definidas
            let name = format!("{}@{}", name, count);

            self.var_names.push(name.clone()); // lista de variables

            self.nodes[node_index].data.insert(
                v.variable.name.clone(),
                NodeData {
                    name,
                    expr: v.initial_value.clone(),
                    type_: v.variable.var_type.clone(),
                },
            );

            Ok(())
        }
    }

    pub fn get_declared_vars(&self) -> &Vec<String> {
        &self.var_names
    }
    //retorna datos de una funcion
    pub fn query_fn(&self, k: &str) -> Option<&Funcion> {
        self.functions.iter().find(|&f| f.name == k)
    }
    //añade funcion
    pub fn add_fn(&mut self, fn_: Funcion) -> Result<(), ()> {
        self.functions.push(fn_);
        Ok(())
    }

    // Tipo de dato a partir del nombre que le damos en el PL0
    // a+var_int@1 -> el tipo es entero
    // a+var_dec@1 -> el tipo es decimal
    pub fn var_type(s: &str) -> Option<Tipo> {
        match s.split('@').nth(0).unwrap().split('+').nth(1).unwrap() {
            "var_int" => Some(Tipo::Entero),
            "var_dec" => Some(Tipo::Decimal),
            "var_str" => Some(Tipo::Palabra),
            "var_bool" => Some(Tipo::Logico),
            "pointer_int" => Some(Tipo::Lista(Box::new(Tipo::Entero))),
            "pointer_dec" => Some(Tipo::Lista(Box::new(Tipo::Decimal))),
            "pointer_str" => Some(Tipo::Lista(Box::new(Tipo::Palabra))),
            "pointer_bool" => Some(Tipo::Lista(Box::new(Tipo::Logico))),
            _ => None,
        }
    }
}
