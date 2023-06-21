mod pl0_instructions;

use core::fmt;
use std::{collections::HashMap, env, fs::File, io::Write};

use crate::{
    ast::{Ast, AstNodeIndex, NodeData, GLOBAL_SCOPE_NODE},
    symbols::{CodeBlock, CodeBlockElement, Expr, FnCall, Literal, NumBinOp, NumUnaryOp},
};

use self::pl0_instructions::{Mnemo, PL0Instruction, OPR};

pub struct CodeGenerator {
    pl0_decls: Vec<PL0Decl>,
    pl0_instructions: Vec<PL0Instruction>,
    fn_pos: HashMap<String, usize>,
    jump_list: Vec<usize>,
}

impl CodeGenerator {
    pub fn generate(ast: &Ast) {
        let mut cg = CodeGenerator {
            pl0_decls: Vec::new(),
            pl0_instructions: Vec::new(),
            fn_pos: HashMap::new(),
            jump_list: Vec::new(),
        };

        let pointer_list = cg.var_decls(ast);

        cg.generate_lists(pointer_list);

        cg.generate_functions(ast);

        cg.generate_jumptable_rtfn();

        cg.generate_entrypoint();

        cg.write_to_file();
    }

    pub fn write_to_file(&mut self) {
        let args: Vec<String> = env::args().collect();

        let mut f = File::create(args.get(2).unwrap_or(&"test/test.eje".to_string()))
            .expect("No se pudo crear el archivo .eje");

        for decl in &self.pl0_decls {
            writeln!(f, "{}", decl).unwrap();
        }

        writeln!(f, "@").unwrap();

        for (i, instruction) in self.pl0_instructions.iter().enumerate() {
            writeln!(f, "{} {}", i + 1, instruction).unwrap();
        }
    }

    fn generate_lists(&mut self, pointer_list: Vec<NodeData>) {
        for p in pointer_list.iter() {
            self.generate_list_getter(p);
            self.generate_list_setter(p);
        }
    }

    fn generate_list_getter(&mut self, nd: &NodeData) {
        let xs = match nd.expr.as_ref().unwrap() {
            Expr::Literal(Literal::Lista(v)) => v,
            _ => unreachable!(),
        };

        for (i, x) in xs.iter().enumerate() {
            self.pl0_decls.push(PL0Decl {
                name: format!("{}*{}", nd.name, i),
                v: PL0Value::from(x),
                class: PL0Class::Var,
            });
        }

        self.pl0_decls.push(PL0Decl {
            name: nd.name.clone(),
            v: PL0Value::Integer((self.pl0_instructions.len() + 1).try_into().unwrap()),
            class: PL0Class::Var,
        });

        self.jump_list.push(self.pl0_instructions.len() + 1);

        self.pl0_instructions
            .push(PL0Instruction::sto("ix+rtvar".to_string()));

        let offset: usize = (self.pl0_instructions.len() + 1) + (xs.len() * 2);

        for i in 0..xs.len() {
            self.pl0_instructions
                .push(PL0Instruction::lod("ix+rtvar".to_string()));
            self.pl0_instructions.push(PL0Instruction::jmc(
                i.to_string(),
                (offset + i * 6).to_string(),
            ));
        }

        for i in 0..xs.len() {
            self.pl0_instructions
                .push(PL0Instruction::lod(format!("{}*{}", nd.name, i)));

            self.pl0_instructions
                .push(PL0Instruction::sto("ax+rtvar".to_string()));

            self.pl0_instructions
                .push(PL0Instruction::sto("bx+rtvar".to_string()));

            self.pl0_instructions
                .push(PL0Instruction::lod("ax+rtvar".to_string()));

            self.pl0_instructions
                .push(PL0Instruction::lod("bx+rtvar".to_string()));

            self.pl0_instructions
                .push(PL0Instruction::jmp("_jumptable+rtfn".to_string()))
        }
    }

    fn generate_list_setter(&mut self, nd: &NodeData) {
        let xs = match nd.expr.as_ref().unwrap() {
            Expr::Literal(Literal::Lista(v)) => v,
            _ => unreachable!(),
        };

        self.pl0_decls.push(PL0Decl {
            name: nd.name.clone() + "=",
            v: PL0Value::Integer((self.pl0_instructions.len() + 1).try_into().unwrap()),
            class: PL0Class::Var,
        });

        self.jump_list.push(self.pl0_instructions.len() + 1);

        self.pl0_instructions
            .push(PL0Instruction::sto("ix+rtvar".to_string()));

        let offset: usize = (self.pl0_instructions.len() + 1) + (xs.len() * 2);

        for i in 0..xs.len() {
            self.pl0_instructions
                .push(PL0Instruction::lod("ix+rtvar".to_string()));
            self.pl0_instructions.push(PL0Instruction::jmc(
                i.to_string(),
                (offset + i * 2).to_string(),
            ));
        }

        for i in 0..xs.len() {
            self.pl0_instructions
                .push(PL0Instruction::sto(format!("{}*{}", nd.name, i)));

            self.pl0_instructions
                .push(PL0Instruction::jmp("_jumptable+rtfn".to_string()))
        }
    }

    fn var_decls(&mut self, ast: &Ast) -> Vec<NodeData> {
        self.pl0_decls.push(PL0Decl {
            name: "ix+rtvar".to_string(),
            v: PL0Value::Integer(0),
            class: PL0Class::Var,
        });

        self.pl0_decls.push(PL0Decl {
            name: "jx+rtvar".to_string(),
            v: PL0Value::Integer(0),
            class: PL0Class::Var,
        });

        self.pl0_decls.push(PL0Decl {
            name: "return_dir+rtvar".to_string(),
            v: PL0Value::Integer(0),
            class: PL0Class::Var,
        });

        self.pl0_decls.push(PL0Decl {
            name: "ax+rtvar".to_string(),
            v: PL0Value::Integer(0),
            class: PL0Class::Var,
        });

        self.pl0_decls.push(PL0Decl {
            name: "bx+rtvar".to_string(),
            v: PL0Value::Integer(0),
            class: PL0Class::Var,
        });

        let mut pointer_list: Vec<NodeData> = Vec::new();

        for s in ast.get_declared_vars() {
            let decl = ast.query_var(GLOBAL_SCOPE_NODE, s);

            let lit = if let Some(nd) = decl {
                if matches!(nd.type_, crate::symbols::Tipo::Lista(_)) {
                    pointer_list.push(nd.clone());
                    continue;
                }

                nd.expr
                    .clone()
                    .and_then(|e| e.get_const_value(ast, GLOBAL_SCOPE_NODE))
                    .unwrap_or_else(|| nd.type_.mk_default())
            } else {
                Ast::var_type(s).unwrap().mk_default()
            };

            let v = PL0Value::from(&lit);

            if s.contains("pointer") {
                self.pl0_decls.push(PL0Decl {
                    name: s.clone(),
                    v: v.clone(),
                    class: PL0Class::Var,
                });
                self.pl0_decls.push(PL0Decl {
                    name: s.clone() + "=",
                    v,
                    class: PL0Class::Var,
                })
            } else {
                self.pl0_decls.push(PL0Decl {
                    name: s.clone(),
                    v,
                    class: PL0Class::Var,
                })
            }
        }

        return pointer_list;
    }

    fn generate_entrypoint(&mut self) {
        let pos = *self.fn_pos.get("principal").unwrap();
        self.pl0_decls.push(PL0Decl {
            name: "_P".to_string(),
            v: PL0Value::Integer(pos.try_into().unwrap()),
            class: PL0Class::Inst,
        })
    }

    fn generate_jumptable_rtfn(&mut self) {
        self.pl0_decls.push(PL0Decl {
            name: "_jumptable+rtfn".to_string(),
            v: PL0Value::Integer((self.pl0_instructions.len() + 1).try_into().unwrap()),
            class: PL0Class::Inst,
        });

        self.pl0_instructions
            .push(PL0Instruction::sto("jx+rtvar".to_string()));

        for jump in &self.jump_list {
            self.pl0_instructions
                .push(PL0Instruction::lod("jx+rtvar".to_string()));
            self.pl0_instructions
                .push(PL0Instruction::jmc(jump.to_string(), jump.to_string()));
        }
    }

    fn generate_functions(&mut self, ast: &Ast) {
        for fn_ in ast.functions.iter() {
            self.fn_pos
                .insert(fn_.name.clone(), self.pl0_instructions.len() + 1);

            if fn_.name == "principal" {
                for element in fn_.body.elements.iter() {
                    Self::code_block_element(self, element, fn_.body.lexical_scope_index, ast)
                }
                self.pl0_instructions.push(PL0Instruction::opr(OPR::Fin))
            } else {
                self.pl0_instructions
                    .push(PL0Instruction::sto("return_dir+rtvar".to_string()));

                self.pl0_instructions
                    .push(PL0Instruction::sto("ax+rtvar".to_string()));

                for param in fn_.params.iter().rev() {
                    let iden = &ast
                        .query_var(fn_.body.lexical_scope_index, &param.name)
                        .unwrap_or_else(|| panic!("Variable {} no existe", &param.name))
                        .name;

                    if matches!(param.var_type, crate::symbols::Tipo::Lista(_)) {
                        self.pl0_instructions
                            .push(PL0Instruction::sto(iden.clone() + "="));
                        self.pl0_instructions
                            .push(PL0Instruction::sto(iden.clone()));
                    } else {
                        self.pl0_instructions
                            .push(PL0Instruction::sto(iden.clone()));
                    }
                }

                self.pl0_instructions
                    .push(PL0Instruction::lod("ax+rtvar".to_string()));

                self.code_block(&fn_.body, ast);

                self.pl0_instructions
                    .push(PL0Instruction::lod("return_dir+rtvar".to_string()));
                self.pl0_instructions
                    .push(PL0Instruction::jmp("_jumptable+rtfn".to_string()))
            }
        }
    }

    fn generate_call_instruction(&mut self, dir: String) {
        let return_dir: usize = self.pl0_instructions.len() + 4;
        self.jump_list.push(return_dir);

        self.pl0_instructions
            .push(PL0Instruction::lod("return_dir+rtvar".to_string()));

        self.pl0_instructions.push(PL0Instruction {
            mnemo: Mnemo::LIT,
            dir1: return_dir.to_string(),
            dir2: "0".to_string(),
        });

        self.pl0_instructions.push(PL0Instruction {
            mnemo: Mnemo::JMP,
            dir1: "0".to_string(),
            dir2: dir,
        });

        self.pl0_instructions
            .push(PL0Instruction::sto("return_dir+rtvar".to_string()));
    }

    fn code_block(&mut self, code_block: &CodeBlock, ast: &Ast) {
        for element in code_block.elements.iter() {
            Self::code_block_element(self, element, code_block.lexical_scope_index, ast)
        }
    }

    fn code_block_element(
        &mut self,
        cb_element: &CodeBlockElement,
        lexical_scope_index: AstNodeIndex,
        ast: &Ast,
    ) {
        match cb_element {
            crate::symbols::CodeBlockElement::VariableDecl(e) => {
                self.expr(
                    &e.initial_value
                        .clone()
                        .unwrap_or_else(|| Expr::Literal(e.variable.var_type.mk_default())),
                    lexical_scope_index,
                    ast,
                );

                let iden = &ast
                    .query_var(lexical_scope_index, &e.variable.name)
                    .unwrap_or_else(|| panic!("Variable {} no existe", &e.variable.name))
                    .name;

                let iden = ast
                    .query_var(lexical_scope_index, iden)
                    .unwrap()
                    .name
                    .clone();

                self.pl0_instructions.push(PL0Instruction::sto(iden));
            }

            crate::symbols::CodeBlockElement::Assignment(e) => {
                if let Some(index) = &e.index {
                    let iden = ast
                        .query_var(lexical_scope_index, &e.var)
                        .unwrap_or_else(|| panic!("Variable {} no existe", &e.var))
                        .name
                        .clone();

                    let mod_index = self.pl0_instructions.len();

                    self.pl0_instructions
                        .push(PL0Instruction::lit("".to_string()));

                    self.expr(&e.expr, lexical_scope_index, ast);

                    self.expr(index, lexical_scope_index, ast);

                    self.jump_list.push(self.pl0_instructions.len() + 3);

                    self.pl0_instructions[mod_index].dir1 =
                        (self.pl0_instructions.len() + 3).to_string();

                    self.pl0_instructions.push(PL0Instruction::lod(iden + "="));

                    self.pl0_instructions
                        .push(PL0Instruction::jmp("_jumptable+rtfn".to_string()));
                } else {
                    self.expr(&e.expr, lexical_scope_index, ast);

                    let nd = ast
                        .query_var(lexical_scope_index, &e.var)
                        .unwrap_or_else(|| panic!("Variable {} no existe", &e.var));

                    if matches!(nd.type_, crate::symbols::Tipo::Lista(_)) {
                        self.pl0_instructions
                            .push(PL0Instruction::sto(nd.name.clone() + "="));
                        self.pl0_instructions
                            .push(PL0Instruction::sto(nd.name.clone()));
                    } else {
                        self.pl0_instructions
                            .push(PL0Instruction::sto(nd.name.clone()));
                    }
                }
            }

            crate::symbols::CodeBlockElement::If(e) => {
                self.expr(&e.condition, e.body.lexical_scope_index, ast);

                let mod_index = self.pl0_instructions.len();
                self.pl0_instructions
                    .push(PL0Instruction::jmc("F".to_string(), "0".to_string()));

                self.code_block(&e.body, ast);

                if let Some(else_body) = &e.else_body {
                    let mod_index2 = self.pl0_instructions.len();
                    self.pl0_instructions
                        .push(PL0Instruction::jmp("0".to_string()));

                    self.pl0_instructions[mod_index].dir2 =
                        (self.pl0_instructions.len() + 1).to_string();

                    self.code_block(else_body, ast);

                    self.pl0_instructions[mod_index2].dir2 =
                        (self.pl0_instructions.len() + 1).to_string();
                } else {
                    self.pl0_instructions[mod_index].dir2 =
                        (self.pl0_instructions.len() + 1).to_string();
                }
            }

            crate::symbols::CodeBlockElement::While(e) => {
                let jmc_index = self.pl0_instructions.len() + 1;

                self.expr(&e.condition, e.body.lexical_scope_index, ast);

                let mod_index = self.pl0_instructions.len();

                self.pl0_instructions
                    .push(PL0Instruction::jmc("F".to_string(), "0".to_string()));

                self.code_block(&e.body, ast);

                self.pl0_instructions
                    .push(PL0Instruction::jmp(jmc_index.to_string()));

                self.pl0_instructions[mod_index].dir2 =
                    (self.pl0_instructions.len() + 1).to_string();
            }

            crate::symbols::CodeBlockElement::DoWhile(e) => {
                let jmc_index = self.pl0_instructions.len() + 1;
                self.code_block(&e.body, ast);
                self.expr(&e.condition, e.body.lexical_scope_index, ast);
                self.pl0_instructions
                    .push(PL0Instruction::jmc("F".to_string(), jmc_index.to_string()));
            }

            crate::symbols::CodeBlockElement::For(e) => {
                self.expr(&e.assignment.expr, e.body.lexical_scope_index, ast);
                let iden = ast
                    .query_var(lexical_scope_index, &e.assignment.var)
                    .unwrap_or_else(|| panic!("Variable {} no existe", &e.assignment.var))
                    .name
                    .clone();
                self.pl0_instructions
                    .push(PL0Instruction::sto(iden.clone()));

                let jmc_index = self.pl0_instructions.len() + 1;

                self.pl0_instructions
                    .push(PL0Instruction::lod(iden.clone()));
                self.expr(&e.condition, e.body.lexical_scope_index, ast);
                self.pl0_instructions
                    .push(PL0Instruction::opr(OPR::MenorIgualQue));

                let mod_index = self.pl0_instructions.len();

                self.pl0_instructions
                    .push(PL0Instruction::jmc("F".to_string(), "0".to_string()));

                self.code_block(&e.body, ast);

                self.expr(&e.incr, e.body.lexical_scope_index, ast);

                self.pl0_instructions
                    .push(PL0Instruction::lod(iden.clone()));
                self.pl0_instructions.push(PL0Instruction::opr(OPR::Suma));
                self.pl0_instructions.push(PL0Instruction::sto(iden));

                self.pl0_instructions
                    .push(PL0Instruction::jmp(jmc_index.to_string()));

                self.pl0_instructions[mod_index].dir2 =
                    (self.pl0_instructions.len() + 1).to_string();
            }

            crate::symbols::CodeBlockElement::Expr(e) => self.expr(e, lexical_scope_index, ast),

            crate::symbols::CodeBlockElement::FnReturn(e) => {
                if let Some(e) = e {
                    self.expr(e, lexical_scope_index, ast);

                    self.pl0_instructions
                        .push(PL0Instruction::sto("ax+rtvar".to_string()));

                    self.pl0_instructions
                        .push(PL0Instruction::sto("bx+rtvar".to_string()));

                    self.pl0_instructions
                        .push(PL0Instruction::lod("ax+rtvar".to_string()));

                    self.pl0_instructions
                        .push(PL0Instruction::lod("bx+rtvar".to_string()));
                }

                self.pl0_instructions
                    .push(PL0Instruction::lod("return_dir+rtvar".to_string()));

                self.pl0_instructions
                    .push(PL0Instruction::jmp("_jumptable+rtfn".to_string()))
            }
        }
    }

    //Genera el codigo para evaluar la expresión y carga el valor hasta arriba del stack
    fn expr(&mut self, expr: &Expr, lexical_scope_index: AstNodeIndex, ast: &Ast) {
        match expr {
            Expr::ListElement(iden, index) => {
                let iden = ast
                    .query_var(lexical_scope_index, iden)
                    .unwrap()
                    .name
                    .clone();

                let mod_index = self.pl0_instructions.len();

                self.pl0_instructions
                    .push(PL0Instruction::lit("".to_string()));

                self.expr(index, lexical_scope_index, ast);

                self.jump_list.push(self.pl0_instructions.len() + 3);

                self.pl0_instructions[mod_index].dir1 =
                    (self.pl0_instructions.len() + 3).to_string();

                self.pl0_instructions.push(PL0Instruction::lod(iden));

                self.pl0_instructions
                    .push(PL0Instruction::jmp("_jumptable+rtfn".to_string()));
            }
            Expr::Identifier(iden) => {
                let nd = ast.query_var(lexical_scope_index, iden).unwrap();

                if matches!(nd.type_, crate::symbols::Tipo::Lista(_)) {
                    self.pl0_instructions
                        .push(PL0Instruction::lod(nd.name.clone()));
                    self.pl0_instructions
                        .push(PL0Instruction::lod(nd.name.clone() + "="));
                } else {
                    self.pl0_instructions
                        .push(PL0Instruction::lod(nd.name.clone()));
                }
            }
            Expr::Literal(lit) => {
                self.pl0_instructions
                    .push(PL0Instruction::lit(lit.to_string()));
            }
            Expr::NumUnaryOp(op) => match op.op {
                NumUnaryOp::Positivo => {
                    self.expr(&op.expr, lexical_scope_index, ast);
                }
                NumUnaryOp::Negativo => {
                    self.expr(&op.expr, lexical_scope_index, ast);
                    self.pl0_instructions
                        .push(PL0Instruction::opr(OPR::MenosUnitario));
                }
            },
            Expr::NumBinOp(op) => {
                self.expr(&op.lhs, lexical_scope_index, ast);
                self.expr(&op.rhs, lexical_scope_index, ast);

                let opr = match op.op {
                    NumBinOp::Suma => OPR::Suma,
                    NumBinOp::Resta => OPR::Resta,
                    NumBinOp::Multiplicacion => OPR::Multiplicacion,
                    NumBinOp::Division => OPR::Division,
                    NumBinOp::Modulo => OPR::Modulo,
                    NumBinOp::Potenciacion => OPR::Exponente,
                };

                self.pl0_instructions.push(PL0Instruction::opr(opr));
            }
            Expr::BoolUnaryOp(op) => {
                self.expr(&op.expr, lexical_scope_index, ast);
                self.pl0_instructions.push(PL0Instruction::opr(OPR::No));
            }
            Expr::BoolBinOp(op) => {
                self.expr(&op.lhs, lexical_scope_index, ast);
                self.expr(&op.rhs, lexical_scope_index, ast);

                let opr = match op.op {
                    crate::symbols::BoolBinOp::Y => OPR::Y,
                    crate::symbols::BoolBinOp::O => OPR::O,
                };

                self.pl0_instructions.push(PL0Instruction::opr(opr));
            }
            Expr::Comp(comp) => {
                self.expr(&comp.lhs, lexical_scope_index, ast);
                self.expr(&comp.rhs, lexical_scope_index, ast);

                let opr = match comp.comp {
                    crate::symbols::Comp::Menor => OPR::MenorQue,
                    crate::symbols::Comp::Mayor => OPR::MayorQue,
                    crate::symbols::Comp::MenorIgual => OPR::MenorIgualQue,
                    crate::symbols::Comp::MayorIgual => OPR::MayorIgualQue,
                    crate::symbols::Comp::Igual => OPR::Igual,
                    crate::symbols::Comp::Diferente => OPR::Diferente,
                };

                self.pl0_instructions.push(PL0Instruction::opr(opr));
            }
            Expr::FnCall(fn_call) => {
                if self.handle_builtin_fn(fn_call, lexical_scope_index, ast) {
                    return;
                }

                let fn_pos = *self
                    .fn_pos
                    .get(&fn_call.fn_name)
                    .unwrap_or_else(|| panic!("Funcion con nombre {} no existe", &fn_call.fn_name));

                for arg in &fn_call.args {
                    self.expr(arg, lexical_scope_index, ast);
                }

                self.generate_call_instruction(fn_pos.to_string());
            }
        }
    }

    fn handle_builtin_fn(
        &mut self,
        fn_call: &FnCall,
        lexical_scope_index: AstNodeIndex,
        ast: &Ast,
    ) -> bool {
        match fn_call.fn_name.as_str() {
            "imprimenl" => {
                for arg in fn_call.args.iter().rev() {
                    self.expr(arg, lexical_scope_index, ast);
                }

                for _ in 0..fn_call.args.len() {
                    self.pl0_instructions.push(PL0Instruction::opr(OPR::Print));
                }

                self.pl0_instructions
                    .push(PL0Instruction::lit("".to_string()));

                self.pl0_instructions
                    .push(PL0Instruction::opr(OPR::PrintLn));

                true
            }
            "imprime" => {
                for arg in fn_call.args.iter().rev() {
                    self.expr(arg, lexical_scope_index, ast);
                }

                for _ in 0..fn_call.args.len() {
                    self.pl0_instructions.push(PL0Instruction::opr(OPR::Print));
                }

                true
            }
            "lee" => {
                let mut instruction = PL0Instruction::opr(OPR::LeeEntrada);

                instruction.dir1 = match fn_call.args.first() {
                    Some(Expr::Identifier(id)) => ast
                        .query_var(lexical_scope_index, id)
                        .unwrap_or_else(|| panic!("Variable {} no existe", &id))
                        .name
                        .clone(),
                    _ => panic!(),
                };

                self.pl0_instructions.push(instruction);

                true
            }
            "limpia" => {
                self.pl0_instructions
                    .push(PL0Instruction::opr(OPR::LimpiaPantalla));

                true
            }
            _ => false,
        }
    }
}

struct PL0Decl {
    name: String,
    v: PL0Value,
    class: PL0Class,
}

impl fmt::Display for PL0Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{},{},{},{},0,{},#",
            self.name,
            self.class,
            if self.class != PL0Class::Inst {
                match self.v {
                    PL0Value::Integer(_) => "E",
                    PL0Value::Decimal(_) => "D",
                    PL0Value::String(_) => "P",
                }
            } else {
                "I"
            },
            if self.class == PL0Class::Inst {
                match &self.v {
                    PL0Value::Integer(v) => v.to_string(),
                    PL0Value::Decimal(v) => v.to_string(),
                    PL0Value::String(v) => v.to_string(),
                }
            } else {
                "0".to_string()
            },
            if self.class != PL0Class::Inst {
                match &self.v {
                    PL0Value::Integer(v) => v.to_string(),
                    PL0Value::Decimal(v) => v.to_string(),
                    PL0Value::String(v) => v.to_string(),
                }
            } else {
                "0".to_string()
            }
        )
    }
}

#[derive(Debug, Clone)]
enum PL0Value {
    Integer(i32),
    Decimal(f32),
    String(String),
}

impl From<&Literal> for PL0Value {
    fn from(value: &Literal) -> Self {
        match value {
            crate::symbols::Literal::Entero(v) => PL0Value::Integer(*v),
            crate::symbols::Literal::Decimal(v) => PL0Value::Decimal(*v),
            crate::symbols::Literal::Palabra(v) => PL0Value::String(v.clone()),
            crate::symbols::Literal::Logico(v) => {
                PL0Value::String(if *v { "V".to_string() } else { "F".to_string() })
            }
            crate::symbols::Literal::Lista(_) => unimplemented!(),
        }
    }
}

#[derive(PartialEq, Eq)]
enum PL0Class {
    Var,
    Const,
    Inst,
}

impl fmt::Display for PL0Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PL0Class::Var => "V",
                PL0Class::Const => "C",
                PL0Class::Inst => "I",
            }
        )
    }
}
