use crate::{ast::AstNodeIndex, eat, lexer::token::Token, symbols::*};

use super::{ParseError, Parser};

impl Parser {
    pub fn parse_fn(&mut self, lexical_scope_index: AstNodeIndex) -> Result<Funcion, ParseError> {
        let return_type = self.parse_type()?;
        let name = self.parse_identifier()?;
        let params = self.parse_fn_params()?;

        for param in params.iter() {
            self.ast
                .add_var(
                    lexical_scope_index,
                    &VariableDecl {
                        variable: param.clone(),
                        initial_value: None,
                    },
                )
                .map_err(|_| {
                    self.err(format!(
                        "Parametro con nombre \"{}\", ya existe",
                        param.name.clone()
                    ))
                })?;
        }

        let body = self.parse_code_block(lexical_scope_index)?;

        let f = Funcion {
            name,
            return_type,
            params,
            body,
        };

        Ok(f)
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Variable>, ParseError> {
        eat!(self.token_stream, Token::ParenA, self.err_expected("("))?;

        let mut parameters = Vec::new();

        if self
            .token_stream
            .look_ahead(0, |t| matches!(t, Token::ParenC))
        {
            self.token_stream.bump();
            return Ok(parameters);
        }

        let var_type = self.parse_type()?;

        let is_pointer = eat!(
            self.token_stream,
            Token::NumBinOp(NumBinOp::Multiplicacion),
            ()
        )
        .is_ok();

        let name = self.parse_identifier()?;

        parameters.push(Variable {
            name,
            var_type: if is_pointer {
                Tipo::Lista(Box::new(var_type))
            } else {
                var_type
            },
            is_const: false,
        });

        while eat!(self.token_stream, Token::ParenC, ()).is_err() {
            eat!(
                self.token_stream,
                Token::Coma,
                self.err_expected("Uno de los siguientes ,)")
            )?;

            let var_type = self.parse_type()?;
            let name = self.parse_identifier()?;
            let is_pointer = eat!(
                self.token_stream,
                Token::NumBinOp(NumBinOp::Multiplicacion),
                ()
            )
            .is_ok();
            parameters.push(Variable {
                name,
                var_type: if is_pointer {
                    Tipo::Lista(Box::new(var_type))
                } else {
                    var_type
                },
                is_const: false,
            });
        }

        Ok(parameters)
    }

    pub fn parse_code_block(
        &mut self,
        lexical_scope_index: AstNodeIndex,
    ) -> Result<CodeBlock, ParseError> {
        let mut body = CodeBlock {
            elements: vec![],
            lexical_scope_index,
        };

        if eat!(self.token_stream, Token::LlaveA, ()).is_ok() {
            while eat!(self.token_stream, Token::LlaveC, ()).is_err() {
                self.parse_code_block_element(lexical_scope_index, &mut body.elements)?
            }
        } else {
            self.parse_code_block_element(lexical_scope_index, &mut body.elements)?
        }

        Ok(body)
    }

    fn parse_code_block_element(
        &mut self,
        lexical_scope_index: AstNodeIndex,
        elements: &mut Vec<CodeBlockElement>,
    ) -> Result<(), ParseError> {
        match self.token_stream.look_ahead(0, |t| t).clone() {
            Token::Tipo(_) | Token::Const => {
                let mut decls = self
                    .parse_var_decls(lexical_scope_index)?
                    .into_iter()
                    .map(CodeBlockElement::VariableDecl)
                    .collect();

                elements.append(&mut decls);
            }

            Token::Identificador(_)
                if self
                    .token_stream
                    .look_ahead(1, |t| matches!(t, Token::Asignacion | Token::CorcheteA)) =>
            {
                elements.push(CodeBlockElement::Assignment(self.parse_assignment()?));
            }

            Token::Si => {
                let new_index = self.ast.new_node(lexical_scope_index);
                elements.push(CodeBlockElement::If(self.parse_if(new_index)?));
            }

            Token::Mientras => {
                let new_index = self.ast.new_node(lexical_scope_index);
                elements.push(CodeBlockElement::While(self.parse_while(new_index)?));
            }

            Token::Repite => {
                let new_index = self.ast.new_node(lexical_scope_index);
                elements.push(CodeBlockElement::DoWhile(self.parse_do_while(new_index)?));
            }

            Token::Desde => {
                let new_index = self.ast.new_node(lexical_scope_index);
                elements.push(CodeBlockElement::For(self.parse_for(new_index)?));
            }

            Token::Regresa => {
                self.token_stream.bump();
                let expr = if eat!(self.token_stream, Token::PuntoComa, ()).is_err() {
                    let e = self.parse_expr_until_err()?;
                    eat!(self.token_stream, Token::PuntoComa, self.err_expected(";"))?;
                    Some(e)
                } else {
                    None
                };

                elements.push(CodeBlockElement::FnReturn(expr));
            }

            other if other.can_begin_expr() => {
                let e = self.parse_expr_until_err()?;
                eat!(self.token_stream, Token::PuntoComa, self.err_expected(";"))?;

                elements.push(CodeBlockElement::Expr(e));
            }

            _ => Err(self.err_expected("Declaración"))?,
        }

        Ok(())
    }
}
