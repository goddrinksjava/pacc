use crate::{eat, lexer::token::Token};

use super::{ParseError, Parser};
use crate::symbols::*;

#[derive(Clone, Debug)]

enum Op {
    BoolUnary(),
    BoolBin(BoolBinOp),
    NumUnary(NumUnaryOp),
    NumBin(NumBinOp),
    Comp(Comp),
}
//priorida
impl Op {
    fn get_priority(&self) -> i32 {
        match self {
            Op::NumUnary(_) | Op::BoolUnary() => 8,
            Op::NumBin(NumBinOp::Potenciacion) => 7,
            Op::NumBin(NumBinOp::Multiplicacion | NumBinOp::Division | NumBinOp::Modulo) => 6,
            Op::NumBin(NumBinOp::Suma | NumBinOp::Resta) => 5,
            Op::Comp(Comp::Menor | Comp::MenorIgual | Comp::Mayor | Comp::MayorIgual) => 4,
            Op::Comp(_) => 3,
            Op::BoolBin(BoolBinOp::Y) => 2,
            Op::BoolBin(BoolBinOp::O) => 1,
        }
    }
}

#[derive(Clone, Debug)]
enum PType {
    //Tipo de parentesis
    Enclose(),     // (4 + 9) * 2
    BeginFnArgs(), // imprimenl("Hola ", "mundo")
}

#[derive(Clone, Debug)]
enum StackE {
    Expr(Expr),
    Op(Op),
    Paren(PType),
}

// Parser de expresiones como: 5 + factorial(4 + 5) * 2
struct ExprParser<'a> {
    stack: Vec<StackE>,
    parser: &'a mut Parser,
    last_paren_type: Option<PType>,
    fn_args: Vec<Vec<Expr>>,
}

impl ExprParser<'_> {
    fn new(parser: &mut Parser) -> ExprParser<'_> {
        ExprParser {
            stack: Vec::new(),
            parser,
            last_paren_type: None,
            fn_args: Vec::new(),
        }
    }

    // Cierre de parentesis 4 * (4 + 8)
    fn end_enclose(&mut self) {
        self.parser.token_stream.bump();

        while !matches!(
            self.stack.len().checked_sub(2).map(|l| &self.stack[l]),
            Some(StackE::Paren(PType::Enclose()))
        ) {
            self.eval();
        }

        match self.stack.pop() {
            Some(e @ StackE::Expr(_)) => {
                self.stack.pop();
                self.stack.push(e);
            }
            _ => unreachable!(),
        }
    }

    // Cierre de parentesis factorial(4 + 8)
    fn end_fn_args(&mut self) {
        self.parser.token_stream.bump();

        while !matches!(
            self.stack.len().checked_sub(2).map(|l| &self.stack[l]),
            Some(StackE::Paren(PType::BeginFnArgs()))
        ) {
            self.eval();
        }

        let last_index = self.fn_args.len() - 1;

        match self.stack.pop() {
            Some(StackE::Expr(e)) => {
                self.fn_args[last_index].push(e);
                self.stack.pop(); // Sacamos el parentesis de inicio al llegar al parentesis de cierre

                match self.stack.pop() {
                    // Sacamos el nombre de la funcion
                    Some(StackE::Expr(Expr::Identifier(fn_name))) => {
                        let fn_call = FnCall {
                            fn_name,
                            args: self.fn_args.pop().unwrap(),
                        };

                        self.stack.push(StackE::Expr(Expr::FnCall(fn_call)))
                    }
                    _ => {
                        println!("{:?}", self.stack);
                        unreachable!()
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn next_fn_arg(&mut self) {
        self.parser.token_stream.bump();

        // self.stack.len()
        // .checked_sub(2) resta comprobada de enteros -> Some(numero) o None
        // 0 - 2 -> None .map(|l| &self.stack[l]) -> None
        // 1 - 2 -> None .map(|l| &self.stack[l]) -> None
        // 2 - 2 -> 0 .map(|l| &self.stack[l]) -> Some(_)
        // .map(|l| &self.stack[l]) -> Some(StackE) o None

        // 5 + 5 = Token::Literal(5) Token::Mas Token::Literal(5)

        //suma(1 + 5 + 5, 1 + 1);

        //suma(1 + 5 + 5
        // despues de evaluar
        //suma(1 + expr_derecha
        //suma(expr1

        //suma(1 + 1);
        //fn_args = [expr1]
        //suma(expr2);
        //suma
        //fn_args = [expr1, expr2]

        // matches!()  --> macro!
        // !matches!()  --> negacion macro!

        while !matches!(
            self.stack.len().checked_sub(2).map(|l| &self.stack[l]), //Obtener penultimo elemento del stack
            Some(StackE::Paren(PType::BeginFnArgs()))
        ) {
            self.eval(); // convertir tockens en general
        }

        let last_index = self.fn_args.len() - 1;

        match self.stack.pop() {
            Some(StackE::Expr(e)) => self.fn_args[last_index].push(e),
            _ => unreachable!(), //Se supone que este caso no puede pasar
        }
    }

    fn begin_paren_enclose(&mut self) {
        self.parser.token_stream.bump();
        self.stack.push(StackE::Paren(PType::Enclose()));
        self.last_paren_type = Some(PType::Enclose());
    }

    fn paren_begin_fn_args(&mut self) {
        self.parser.token_stream.bump();
        self.stack.push(StackE::Paren(PType::BeginFnArgs()));
        self.fn_args.push(Vec::new());
        self.last_paren_type = Some(PType::BeginFnArgs());
    }

    // a + 5;
    // identificador: a
    fn identifier(&mut self) -> Result<(), ParseError> {
        if matches!(self.stack.last(), Some(StackE::Expr(_))) {
            Err(self.parser.err_expected("Operador o ;"))?
        }

        let var_name = self.parser.parse_identifier()?;

        if eat!(self.parser.token_stream, Token::CorcheteA, ()).is_ok() {
            let index = self.parser.parse_expr_until_err()?;
            eat!(
                self.parser.token_stream,
                Token::CorcheteC,
                self.parser.err_expected("]")
            )?;

            self.stack
                .push(StackE::Expr(Expr::ListElement(var_name, Box::new(index))));
        } else {
            self.stack.push(StackE::Expr(Expr::Identifier(var_name)));
        };

        Ok(())
    }

    fn literal(&mut self) -> Result<(), ParseError> {
        if matches!(self.stack.last(), Some(StackE::Expr(_))) {
            Err(self.parser.err_expected("Operador o ;"))?
        }

        let l = self.parser.parse_lit()?;
        self.stack.push(StackE::Expr(Expr::Literal(l)));
        Ok(())
    }

    fn get_last_op_priority(&self) -> i32 {
        self.stack
            .len()
            .checked_sub(2)
            .map(|l| &self.stack[l])
            .map(|e| match e {
                StackE::Op(op) => op.get_priority(),
                _ => 0,
            })
            .unwrap_or_default()
    }

    fn op(&mut self, op: Op) {
        let op_priority = op.get_priority();

        while op_priority <= self.get_last_op_priority() {
            self.eval();
        }

        self.parser.token_stream.bump();
        self.stack.push(StackE::Op(op));
    }

    // StackE: stack con los tokens de la expresion a evaluar
    // Sacamos los tokens del stack para producir un arbol de sintaxis
    // StackE: [Identificador("a"), Op::Negativo, Literal(5)] -> Obtenemos una suma
    fn eval(&mut self) {
        let rhs = match self.stack.pop() {
            Some(StackE::Expr(e)) => e,
            _ => unreachable!(),
        };

        let op = match self.stack.pop() {
            Some(StackE::Op(op)) => op,
            _ => {
                unreachable!()
            }
        };

        match op {
            Op::BoolUnary() => {
                let e = BoolUnaryOpExpr {
                    expr: Box::new(rhs),
                };

                self.stack.push(StackE::Expr(Expr::BoolUnaryOp(e)));
            }

            Op::NumUnary(op) => {
                let e = NumUnaryOpExpr {
                    op,
                    expr: Box::new(rhs),
                };

                self.stack.push(StackE::Expr(Expr::NumUnaryOp(e)));
            }

            Op::BoolBin(op) => {
                let lhs = match self.stack.pop() {
                    Some(StackE::Expr(e)) => e,
                    _ => {
                        println!("{:?}", self.stack);
                        unreachable!()
                    }
                };

                let e = BoolBinOpExpr {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };

                self.stack.push(StackE::Expr(Expr::BoolBinOp(e)));
            }
            Op::NumBin(op) => {
                let lhs = match self.stack.pop() {
                    Some(StackE::Expr(e)) => e,
                    _ => {
                        println!("{:?}", self.stack);
                        unreachable!()
                    }
                };

                let e = NumBinOpExpr {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };

                self.stack.push(StackE::Expr(Expr::NumBinOp(e)));
            }
            Op::Comp(comp) => {
                let lhs = match self.stack.pop() {
                    Some(StackE::Expr(e)) => e,
                    _ => {
                        println!("{:?}", self.stack);
                        unreachable!()
                    }
                };

                let e = CompExpr {
                    lhs: Box::new(lhs),
                    comp,
                    rhs: Box::new(rhs),
                };

                self.stack.push(StackE::Expr(Expr::Comp(e)));
            }
        }
    }

    // Decide que funcion llamar dependiendo del token que encuentre
    pub fn next(&mut self) -> Result<(), ParseError> {
        match self.parser.token_stream.look_ahead(0, |t| t).clone() {
            Token::Literal(_) => {
                self.literal()?;
            }

            Token::Identificador(_) => {
                self.identifier()?;
            }

            Token::Mas => match self.stack.last() {
                Some(StackE::Expr(_)) => self.op(Op::NumBin(NumBinOp::Suma)),
                _ => self.op(Op::NumUnary(NumUnaryOp::Positivo)),
            },

            Token::Menos => match self.stack.last() {
                Some(StackE::Expr(_)) => self.op(Op::NumBin(NumBinOp::Resta)),
                _ => self.op(Op::NumUnary(NumUnaryOp::Negativo)),
            },

            Token::No => self.op(Op::BoolUnary()),

            Token::NumBinOp(nbo) => match self.stack.last() {
                Some(StackE::Expr(_)) => self.op(Op::NumBin(nbo)),
                _ => Err(self
                    .parser
                    .err_expected("Expresión a la izquierda de operador binario"))?,
            },

            Token::BoolBinOp(bbo) => match self.stack.last() {
                Some(StackE::Expr(_)) => self.op(Op::BoolBin(bbo)),
                _ => Err(self
                    .parser
                    .err_expected("Expresión a la izquierda de operador binario"))?,
            },

            Token::Comp(comp) => match self.stack.last() {
                Some(StackE::Expr(_)) => self.op(Op::Comp(comp)),
                _ => Err(self
                    .parser
                    .err_expected("Expresión a la izquierda de comparación"))?,
            },

            Token::ParenA => match self.stack.last() {
                Some(StackE::Expr(_)) => self.paren_begin_fn_args(),
                _ => self.begin_paren_enclose(),
            },

            Token::ParenC => match self.last_paren_type {
                Some(PType::BeginFnArgs()) => self.end_fn_args(),
                Some(PType::Enclose()) => self.end_enclose(),
                _ => Err(self.parser.err_unexpected(")"))?,
            },

            Token::Coma => match self.last_paren_type {
                Some(PType::BeginFnArgs()) => self.next_fn_arg(),
                _ => Err(self.parser.err_unexpected(","))?,
            },

            _ => Err(self.parser.err_expected("Expresión"))?,
        };

        Ok(())
    }

    pub fn end(mut self) -> Result<Expr, ParseError> {
        while self.stack.len() > 1 {
            self.eval();
        }

        match self.stack.pop() {
            Some(StackE::Expr(e)) => Ok(e),
            _ => unreachable!(),
        }
    }
}

impl Parser {
    //Evalua expresion hasta que el token que encuentre no pueda producir una expresion valida
    // Ejemplo 4 + 5; se detiene en ; ya que lo forma parte de una expresion
    pub fn parse_expr_until_err(&mut self) -> Result<Expr, ParseError> {
        let mut expr_paser = ExprParser::new(self);
        while expr_paser.next().is_ok() {}
        expr_paser.end()
    }
}
