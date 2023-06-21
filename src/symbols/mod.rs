use std::fmt;

use crate::ast::{Ast, AstNodeIndex};

#[derive(Debug)]
pub struct Funcion {
    pub name: String,
    pub return_type: Tipo,
    pub params: Vec<Variable>,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FnCall {
    pub fn_name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub var: String,
    pub index: Option<Expr>,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDecl {
    pub variable: Variable,
    pub initial_value: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub name: String,
    pub var_type: Tipo,
    pub is_const: bool,
}

impl Variable {
    fn get_symbol_name(&self) -> String {
        self.name.clone() + "@" + &self.var_type.to_string().to_lowercase()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Tipo {
    Nulo,
    Entero,
    Decimal,
    Palabra,
    Logico,
    Lista(Box<Tipo>),
}

impl Tipo {
    fn is_numeric(&self) -> bool {
        match self {
            Tipo::Entero | Tipo::Decimal => true,
            _ => false,
        }
    }
}

impl fmt::Display for Tipo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Tipo {
    pub fn mk_default(&self) -> Literal {
        match self {
            Tipo::Nulo => unreachable!(),
            Tipo::Entero => Literal::Entero(0),
            Tipo::Decimal => Literal::Decimal(0.0),
            Tipo::Palabra => Literal::Palabra("".to_string()),
            Tipo::Logico => Literal::Logico(false),
            Tipo::Lista(_) => Literal::Entero(0),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Entero(i32),
    Decimal(f32),
    Palabra(String),
    Logico(bool),
    Lista(Vec<Literal>),
}

impl Literal {
    pub fn get_type(&self) -> Option<Tipo> {
        match self {
            Literal::Entero(_) => Some(Tipo::Entero),
            Literal::Decimal(_) => Some(Tipo::Decimal),
            Literal::Palabra(_) => Some(Tipo::Palabra),
            Literal::Logico(_) => Some(Tipo::Logico),
            Literal::Lista(v) => v.first().unwrap().get_type(),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Entero(v) => write!(f, "{}", v),
            Literal::Decimal(v) => write!(f, "{}", v),
            Literal::Palabra(v) => write!(f, "{}", v.replace(',', ".")),
            Literal::Logico(v) => write!(f, "{}", if *v { 'V' } else { 'F' }),
            Literal::Lista(_) => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Comp {
    Menor,
    Mayor,
    MenorIgual,
    MayorIgual,
    Igual,
    Diferente,
}

macro_rules! apply_comp {
    ($comp:expr, $v1:expr, $v2:expr) => {
        match $comp {
            Comp::Menor => $v1 < $v2,
            Comp::Mayor => $v1 > $v2,
            &Comp::MenorIgual => $v1 <= $v2,
            Comp::MayorIgual => $v1 >= $v2,
            &Comp::Igual => $v1 == $v2,
            Comp::Diferente => $v1 != $v2,
        }
    };
}

impl Comp {
    fn apply(&self, v1: Literal, v2: Literal) -> Option<Literal> {
        if let Literal::Decimal(d1) = v1 {
            let d2 = match v2 {
                Literal::Entero(e) => e as f32,
                Literal::Decimal(d) => d,
                _ => return None,
            };
            return Some(Literal::Logico(self.apply_f32(d1, d2)));
        }

        if let Literal::Entero(e1) = v1 {
            match v2 {
                Literal::Entero(e2) => {
                    return Some(Literal::Logico(self.apply_i32(e1, e2)));
                }
                Literal::Decimal(d2) => {
                    let d1 = e1 as f32;
                    return Some(Literal::Logico(self.apply_f32(d1, d2)));
                }
                _ => return None,
            };
        }

        None
    }

    fn apply_i32(&self, v1: i32, v2: i32) -> bool {
        apply_comp!(self, v1, v2)
    }

    fn apply_f32(&self, v1: f32, v2: f32) -> bool {
        apply_comp!(self, v1, v2)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompExpr {
    pub lhs: Box<Expr>,
    pub comp: Comp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumUnaryOp {
    Positivo,
    Negativo,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumUnaryOpExpr {
    pub op: NumUnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum NumBinOp {
    Suma,
    Resta,
    Multiplicacion,
    Division,
    Modulo,
    Potenciacion,
}

impl NumBinOp {
    //funcion que recibe y comprueba que los calores sean de tipo decimal o enteros
    fn apply(&self, v1: Literal, v2: Literal) -> Option<Literal> {
        if let Literal::Decimal(d1) = v1 {
            let d2 = match v2 {
                Literal::Entero(e) => e as f32,
                Literal::Decimal(d) => d,
                _ => return None,
            };
            return Some(Literal::Decimal(self.apply_f32(d1, d2)));
        }

        if let Literal::Entero(e1) = v1 {
            match v2 {
                Literal::Entero(e2) => {
                    return Some(Literal::Entero(self.apply_i32(e1, e2)));
                }
                Literal::Decimal(d2) => {
                    let d1 = e1 as f32;
                    return Some(Literal::Decimal(self.apply_f32(d1, d2)));
                }
                _ => return None,
            };
        }

        None
    }

    //valores de entero / resultados de operadores
    fn apply_i32(&self, v1: i32, v2: i32) -> i32 {
        match self {
            NumBinOp::Suma => v1 + v2,
            NumBinOp::Resta => v1 - v2,
            NumBinOp::Multiplicacion => v1 * v2,
            NumBinOp::Division => v1 / v2,
            NumBinOp::Modulo => v1 % v2,
            NumBinOp::Potenciacion => match v2.try_into() {
                Ok(v) => v1.pow(v),
                Err(_) => 1 / v1.pow(v2.unsigned_abs()),
            },
        }
    }
    // flotantes resultados de operadores
    fn apply_f32(&self, v1: f32, v2: f32) -> f32 {
        match self {
            NumBinOp::Suma => v1 + v2,
            NumBinOp::Resta => v1 - v2,
            NumBinOp::Multiplicacion => v1 * v2,
            NumBinOp::Division => v1 / v2,
            NumBinOp::Modulo => v1 % v2,
            NumBinOp::Potenciacion => v1.powf(v2),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumBinOpExpr {
    pub lhs: Box<Expr>,
    pub op: NumBinOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolUnaryOpExpr {
    // Solo hay un operador de este tipo: "no"
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BoolBinOp {
    Y,
    O,
}

impl BoolBinOp {
    fn apply(&self, v1: bool, v2: bool) -> bool {
        match self {
            BoolBinOp::Y => v1 && v2,
            BoolBinOp::O => v1 || v2,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolBinOpExpr {
    pub lhs: Box<Expr>,
    pub op: BoolBinOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    ListElement(String, Box<Expr>),
    Identifier(String),
    Literal(Literal),
    NumUnaryOp(NumUnaryOpExpr),
    NumBinOp(NumBinOpExpr),
    BoolUnaryOp(BoolUnaryOpExpr),
    BoolBinOp(BoolBinOpExpr),
    Comp(CompExpr),
    FnCall(FnCall),
}

//operadores
impl Expr {
    pub fn get_type(&self, ast: &Ast, index: AstNodeIndex) -> Option<Tipo> {
        match self {
            Expr::Identifier(k) => ast.query_var(index, k)?.expr.as_ref()?.get_type(ast, index),
            Expr::Literal(l) => l.get_type(),
            Expr::NumUnaryOp(op) => match op.expr.get_type(ast, index) {
                v @ (Some(Tipo::Entero) | Some(Tipo::Decimal)) => v,
                _ => None,
            },
            Expr::NumBinOp(op) => {
                let lhs_t = op.lhs.get_type(ast, index)?;
                let rhs_t = op.rhs.get_type(ast, index)?;

                if !(lhs_t.is_numeric() && rhs_t.is_numeric()) {
                    return None;
                }

                if lhs_t == Tipo::Decimal || rhs_t == Tipo::Decimal {
                    Some(Tipo::Decimal)
                } else {
                    Some(Tipo::Entero)
                }
            }
            Expr::BoolUnaryOp(op) => {
                if op.expr.get_type(ast, index)? == Tipo::Logico {
                    Some(Tipo::Logico)
                } else {
                    None
                }
            }
            Expr::BoolBinOp(op) => {
                let lhs_t = op.lhs.get_type(&ast, index)?;
                let rhs_t = op.rhs.get_type(ast, index)?;

                if !(lhs_t == Tipo::Logico && rhs_t == Tipo::Logico) {
                    return Some(Tipo::Logico);
                } else {
                    None
                }
            }
            Expr::Comp(comp) => {
                let lhs_t = comp.lhs.get_type(ast, index)?;
                let rhs_t = comp.rhs.get_type(ast, index)?;

                if !(lhs_t.is_numeric() && rhs_t.is_numeric()) {
                    return Some(Tipo::Logico);
                } else {
                    return None;
                }
            }
            Expr::FnCall(fn_call) => ast
                .query_fn(&fn_call.fn_name)
                .map(|f| f.return_type.clone()),
            Expr::ListElement(k, _) => {
                match ast
                    .query_var(index, k)?
                    .expr
                    .as_ref()?
                    .get_type(ast, index)?
                {
                    Tipo::Nulo => None,
                    Tipo::Entero => None,
                    Tipo::Decimal => None,
                    Tipo::Palabra => None,
                    Tipo::Logico => None,
                    Tipo::Lista(t) => Some(*t),
                }
            }
        }
    }

    pub fn get_const_value(&self, ast: &Ast, index: AstNodeIndex) -> Option<Literal> {
        match self {
            Expr::Identifier(k) => ast
                .query_var(index, k)?
                .expr
                .as_ref()?
                .get_const_value(ast, index),
            Expr::Literal(l) => Some(l.clone()),

            Expr::NumUnaryOp(num_unary_op_expr) => {
                let m = match num_unary_op_expr.op {
                    NumUnaryOp::Positivo => 1,
                    NumUnaryOp::Negativo => -1,
                };

                let l = num_unary_op_expr.expr.get_const_value(ast, index)?;
                match l {
                    Literal::Entero(e) => Some(Literal::Entero(e * m)),
                    Literal::Decimal(d) => Some(Literal::Decimal(d * m as f32)),
                    _ => None,
                }
            }

            Expr::NumBinOp(num_bin_op_expr) => {
                let lhs = num_bin_op_expr.lhs.get_const_value(ast, index)?;
                let rhs = num_bin_op_expr.rhs.get_const_value(ast, index)?;
                num_bin_op_expr.op.apply(lhs, rhs)
            }

            Expr::BoolUnaryOp(bool_unary_op_expr) => {
                let l = bool_unary_op_expr.expr.get_const_value(ast, index)?;
                match l {
                    Literal::Logico(b) => Some(Literal::Logico(!b)),
                    _ => None,
                }
            }

            Expr::BoolBinOp(bool_bin_op_expr) => {
                let lhs = bool_bin_op_expr.rhs.get_const_value(ast, index)?;
                let rhs = bool_bin_op_expr.rhs.get_const_value(ast, index)?;

                if let Literal::Logico(lhs_bool) = lhs {
                    if let Literal::Logico(rhs_bool) = rhs {
                        return Some(Literal::Logico(
                            bool_bin_op_expr.op.apply(lhs_bool, rhs_bool),
                        ));
                    }
                }

                None
            }

            Expr::Comp(comp_expr) => {
                let lhs = comp_expr.rhs.get_const_value(ast, index)?;
                let rhs = comp_expr.rhs.get_const_value(ast, index)?;
                comp_expr.comp.apply(lhs, rhs)
            }

            Expr::FnCall(_) => None,

            Expr::ListElement(_, _) => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: Expr,
    pub body: CodeBlock,
    pub else_body: Option<CodeBlock>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub assignment: Assignment,
    pub condition: Expr,
    pub incr: Expr,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub condition: Expr,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DoWhile {
    pub condition: Expr,
    pub body: CodeBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CodeBlock {
    pub elements: Vec<CodeBlockElement>,
    pub lexical_scope_index: AstNodeIndex,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CodeBlockElement {
    VariableDecl(VariableDecl),
    Assignment(Assignment),
    If(If),
    While(While),
    DoWhile(DoWhile),
    For(For),
    Expr(Expr),
    FnReturn(Option<Expr>),
}
