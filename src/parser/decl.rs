use std::mem;

use crate::{ast::AstNodeIndex, eat, lexer::token::Token, symbols::*};

use super::{ParseError, Parser};

enum LT {
    NotAList,
    UnknownSz,
    Sz(usize),
}

impl Parser {
    // Result<Correcto, Error>
    // Correcto = Vec<(Variable, Option<Assignment>)>
    // Error = ParseError
    //assignament = puede estas presente o no
    // int a = 0;
    // int a;

    pub fn parse_list_sz(&mut self, index: AstNodeIndex) -> Result<LT, ParseError> {
        if eat!(self.token_stream, Token::CorcheteA, ()).is_err() {
            return Ok(LT::NotAList);
        }

        if eat!(self.token_stream, Token::CorcheteC, ()).is_ok() {
            return Ok(LT::UnknownSz);
        }

        let tmp = self.parse_expr_until_err()?;

        let sz = if let Some(lit) = tmp.get_const_value(&self.ast, index) {
            match lit {
                Literal::Entero(v) => LT::Sz(v.try_into().unwrap()),
                other => {
                    return Err(self.err_expected(format!(
                        "Valor constante de tipo entero, se encontró {:?}",
                        other
                    )))
                }
            }
        } else {
            return Err(self.err_expected(format!(
                "Valor constante de tipo entero, se encontró {:?}",
                tmp
            )));
        };

        eat!(self.token_stream, Token::CorcheteC, self.err_expected("]"))?;

        return Ok(sz);
    }

    pub fn parse_var_decls(
        &mut self,
        lexical_scope_index: AstNodeIndex,
    ) -> Result<Vec<VariableDecl>, ParseError> {
        let mut decls: Vec<VariableDecl> = Vec::new();

        let is_const = eat!(self.token_stream, Token::Const, ()).is_ok();
        let var_type = self.parse_type()?;

        let mut name = Some(self.parse_identifier()?);

        let mut sz = self.parse_list_sz(lexical_scope_index)?;

        let mut e = None; // i (= 0); no obligatorio

        // parte opcional de declaracion de variables
        loop {
            match self.token_stream.look_ahead(0, |t| t).clone() {
                Token::PuntoComa => {
                    self.token_stream.bump();

                    let (v, t) = match sz {
                        LT::NotAList => (mem::take(&mut e), var_type.clone()),
                        LT::UnknownSz => {
                            if e.is_none() {
                                return Err(self.err("Inicialización de array incorrecta"));
                            };

                            (mem::take(&mut e), Tipo::Lista(Box::new(var_type.clone())))
                        }
                        LT::Sz(sz) => {
                            if e.is_none() {
                                e = Some(Expr::Literal(Literal::Lista(
                                    (0..sz).map(|_| var_type.mk_default()).collect::<Vec<_>>(),
                                )));
                            };

                            (mem::take(&mut e), Tipo::Lista(Box::new(var_type.clone())))
                        }
                    };

                    let decl = VariableDecl {
                        variable: Variable {
                            name: mem::take(&mut name).unwrap(),
                            var_type: t,
                            is_const,
                        },
                        initial_value: v,
                    };

                    self.ast.add_var(lexical_scope_index, &decl).map_err(|_| {
                        self.err(format!(
                            "Variable con nombre \"{}\", ya existe",
                            decl.variable.name.clone()
                        ))
                    })?;

                    decls.push(decl);

                    break;
                }

                Token::Asignacion => {
                    self.token_stream.bump();

                    //No permitir dos asignaciones seguidas, p. ej. entero a = 2 = 2;

                    if e.is_some() {
                        return Err(self.err_expected("Uno de los siguientes ,;"));
                    }

                    e = if eat!(self.token_stream, Token::LlaveA, ()).is_ok() {
                        let mut v = Vec::new();

                        v.push(
                            self.parse_expr_until_err()?
                                .get_const_value(&self.ast, lexical_scope_index)
                                .ok_or(self.err_expected("Valor constante"))?,
                        );

                        while eat!(self.token_stream, Token::LlaveC, ()).is_err() {
                            eat!(self.token_stream, Token::Coma, self.err_expected(","))?;
                            v.push(
                                self.parse_expr_until_err()?
                                    .get_const_value(&self.ast, lexical_scope_index)
                                    .ok_or(self.err_expected("Valor constante"))?,
                            );
                        }

                        Some(Expr::Literal(Literal::Lista(v)))
                    } else {
                        Some(self.parse_expr_until_err()?)
                    }; // la variable toma el valor asignado

                    self.validate_expr(&sz, &e, lexical_scope_index, &var_type)?;
                }

                Token::Coma => {
                    self.token_stream.bump();

                    let (v, t) = match sz {
                        LT::NotAList => (mem::take(&mut e), var_type.clone()),
                        LT::UnknownSz => {
                            if e.is_none() {
                                return Err(self.err("Inicialización de array incorrecta"));
                            };

                            (mem::take(&mut e), Tipo::Lista(Box::new(var_type.clone())))
                        }
                        LT::Sz(sz) => {
                            if e.is_none() {
                                e = Some(Expr::Literal(Literal::Lista(
                                    (0..sz).map(|_| var_type.mk_default()).collect::<Vec<_>>(),
                                )));
                            };

                            (mem::take(&mut e), Tipo::Lista(Box::new(var_type.clone())))
                        }
                    };

                    let decl = VariableDecl {
                        variable: Variable {
                            name: mem::take(&mut name).unwrap(),
                            var_type: t,
                            is_const,
                        },
                        initial_value: v,
                    };

                    self.ast.add_var(lexical_scope_index, &decl).map_err(|_| {
                        self.err(format!(
                            "Variable con nombre \"{}\", ya existe",
                            decl.variable.name.clone()
                        ))
                    })?;

                    decls.push(decl);

                    name = Some(self.parse_identifier()?);

                    sz = self.parse_list_sz(lexical_scope_index)?;
                }

                t => return Err(self.err_unexpected(format!("{:?}", t))),
            }
        }

        Ok(decls)
    }

    fn validate_expr(
        &mut self,
        sz: &LT,
        e: &Option<Expr>,
        lexical_scope_index: usize,
        var_type: &Tipo,
    ) -> Result<(), ParseError> {
        match *sz {
            LT::UnknownSz | LT::NotAList => {
                match e.as_ref().unwrap().get_type(&self.ast, lexical_scope_index) {
                    Some(t) if t == *var_type => Ok(()),
                    _ => Err(self.err_expected(format!("Valor de tipo {}", var_type,))),
                }
            }
            LT::Sz(sz) => match e
                .as_ref()
                .unwrap()
                .get_const_value(&self.ast, lexical_scope_index)
            {
                Some(l) => {
                    match l {
                        Literal::Lista(v) if sz == v.len() => Ok(()),
                        _ => Err(self
                            .err_expected(format!("Lista de tipo {} con tamaño {}", var_type, sz))),
                    }
                }
                _ => {
                    Err(self.err_expected(format!("Lista de tipo {}  con tamaño {}", var_type, sz)))
                }
            },
        }
    }

    pub fn parse_assignment(&mut self) -> Result<Assignment, ParseError> {
        let var = self.parse_identifier()?;

        let index = if eat!(self.token_stream, Token::CorcheteA, ()).is_ok() {
            let index = self.parse_expr_until_err()?;
            eat!(self.token_stream, Token::CorcheteC, self.err_expected("]"))?;
            Some(index)
        } else {
            None
        };

        eat!(self.token_stream, Token::Asignacion, self.err_expected("="))?;
        let expr = self.parse_expr_until_err()?;
        eat!(self.token_stream, Token::PuntoComa, self.err_expected(";"))?;

        Ok(Assignment { var, index, expr })
    }
}
