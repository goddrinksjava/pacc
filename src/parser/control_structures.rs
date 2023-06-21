use crate::{ast::AstNodeIndex, eat, lexer::token::Token, symbols::*};

use super::{ParseError, Parser};
impl Parser {
    //declaracion de if
    pub fn parse_if(&mut self, lexical_scope_index: AstNodeIndex) -> Result<If, ParseError> {
        eat!(
            self.token_stream,
            Token::Si,
            self.err_expected("Palabra reservada \"si\"")
        )?;

        let condition = self.parse_expr_until_err()?;

        eat!(
            self.token_stream,
            Token::Hacer,
            self.err_expected("Palabra reservada \"hacer\"")
        )?;

        let body = self.parse_code_block(lexical_scope_index)?;

        let else_body = if eat!(self.token_stream, Token::Sino, ()).is_ok() {
            Some(self.parse_code_block(lexical_scope_index)?)
        } else {
            None
        };

        Ok(If {
            condition,
            body,
            else_body,
        })
    }

    //declaracion for
    pub fn parse_for(&mut self, lexical_scope_index: AstNodeIndex) -> Result<For, ParseError> {
        eat!(
            self.token_stream,
            Token::Desde,
            self.err_expected("Palabra reservada \"desde\"")
        )?;

        let assignment_var = self.parse_identifier()?;
        eat!(self.token_stream, Token::Asignacion, self.err_expected("="))?;
        let assignment_expr = self.parse_expr_until_err()?;

        eat!(
            self.token_stream,
            Token::Hasta,
            self.err_expected("Palabra reservada \"hasta\"")
        )?;
        let condition = self.parse_expr_until_err()?;

        eat!(
            self.token_stream,
            Token::Incr,
            self.err_expected("Palabra reservada \"incr\"")
        )?;
        let incr = self.parse_expr_until_err()?;

        let body = self.parse_code_block(lexical_scope_index)?;

        Ok(For {
            assignment: Assignment {
                var: assignment_var,
                index: None,
                expr: assignment_expr,
            },
            condition,
            incr,
            body,
        })
    }

    //while
    pub fn parse_while(&mut self, lexical_scope_index: AstNodeIndex) -> Result<While, ParseError> {
        eat!(
            self.token_stream,
            Token::Mientras,
            self.err_expected("Palabra reservada \"mientras\"")
        )?;

        eat!(
            self.token_stream,
            Token::Que,
            self.err_expected("Palabra reservada \"que\"")
        )?;

        let condition = self.parse_expr_until_err()?;

        let body = self.parse_code_block(lexical_scope_index)?;

        Ok(While { condition, body })
    }
    //do while
    pub fn parse_do_while(
        &mut self,
        lexical_scope_index: AstNodeIndex,
    ) -> Result<DoWhile, ParseError> {
        eat!(
            self.token_stream,
            Token::Repite,
            self.err_expected("Palabra reservada \"repite\"")
        )?;

        let body = self.parse_code_block(lexical_scope_index)?;

        eat!(
            self.token_stream,
            Token::Hasta,
            self.err_expected("Palabra reservada \"hasta\"")
        )?;

        eat!(
            self.token_stream,
            Token::Que,
            self.err_expected("Palabra reservada \"que\"")
        )?;

        let condition = self.parse_expr_until_err()?;

        eat!(self.token_stream, Token::PuntoComa, self.err_expected(";"))?;

        Ok(DoWhile { condition, body })
    }
}
