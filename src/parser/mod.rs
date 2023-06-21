mod control_structures;
mod decl;
mod expr;
mod fun;

use std::fmt::{Debug, Display};

use crate::{
    ast::{self, Ast, GLOBAL_SCOPE_NODE},
    eat,
    lexer::{token::Token, token_stream::TokenStream, Loc},
};

use crate::symbols::*;

pub struct ParseError {
    pub msg: String,
    pub tok: Token,
    pub loc: Loc,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at line:{}, col:{}. Found: {}",
            self.msg,
            self.loc.line + 1,
            self.loc.col + 1,
            self.tok
        )
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at line:{}, col:{}. Found: {}",
            self.msg,
            self.loc.line + 1,
            self.loc.col + 1,
            self.tok
        )
    }
}

pub struct Parser {
    ast: Ast,
    token_stream: TokenStream,
}

impl Parser {
    pub fn parse(mut self) -> Result<Ast, ParseError> {
        self.parse_program()?;
        Ok(self.ast)
    }
}

impl Parser {
    pub fn new(token_stream: TokenStream) -> Self {
        //esperamos devolver un valor del tipo Parser (Self es u valor dentro de la clase Parser)
        Self {
            ast: Ast::new(),
            token_stream,
        }
    }

    fn err<S: AsRef<str>>(&self, msg: S) -> ParseError {
        ParseError {
            msg: msg.as_ref().to_string(),
            tok: self.token_stream.last_seen_tok(),
            loc: self.token_stream.last_seen_loc(),
        }
    }

    fn err_expected<S: AsRef<str>>(&self, expected: S) -> ParseError {
        ParseError {
            msg: format!("Se esperaba: {}", expected.as_ref()),
            tok: self.token_stream.last_seen_tok(),
            loc: self.token_stream.last_seen_loc(),
        }
    }

    fn err_unexpected<S: AsRef<str>>(&self, unexpected: S) -> ParseError {
        ParseError {
            msg: format!("No se esperaba: {}", unexpected.as_ref()),
            tok: self.token_stream.last_seen_tok(),
            loc: self.token_stream.last_seen_loc(),
        }
    }

    fn parse_program(&mut self) -> Result<(), ParseError> {
        while self.token_stream.look_ahead(0, |t| t != &Token::Eof) {
            self.parse_global()?;
        }

        Ok(())
    }

    fn parse_global(&mut self) -> Result<(), ParseError> {
        match self.token_stream.look_ahead(0, |t| t).clone() {
            Token::Tipo(_) | Token::Const => self.parse_global_decl()?,
            t if t.can_begin_expr() => {
                self.parse_expr_until_err()?;
                eat!(self.token_stream, Token::PuntoComa, self.err_expected(";"))?;
            }
            t => return Err(self.err_unexpected(format!("{:?}", t))),
        }

        Ok(())
    }

    fn parse_global_decl(&mut self) -> Result<(), ParseError> {
        match self.token_stream.look_ahead(2, |t| t) {
            Token::ParenA => {
                let lexical_scope_index = self.ast.new_node(GLOBAL_SCOPE_NODE);

                let fn_ = self.parse_fn(lexical_scope_index)?;
                self.ast.add_fn(fn_);
            }
            _ => {
                let decls = self.parse_var_decls(GLOBAL_SCOPE_NODE)?;

                for decl in decls {
                    self.ast.add_var(GLOBAL_SCOPE_NODE, &decl);
                }
            }
        };

        Ok(())
    }

    fn parse_type(&mut self) -> Result<Tipo, ParseError> {
        match self.token_stream.look_ahead(0, |t| t).clone() {
            Token::Tipo(t) => {
                self.token_stream.bump();
                Ok(t)
            }
            _ => Err(self.err_expected("Tipo de dato")),
        }
    }

    fn parse_identifier(&mut self) -> Result<String, ParseError> {
        match self.token_stream.look_ahead(0, |t| t).clone() {
            Token::Identificador(nombre) => {
                self.token_stream.bump();
                Ok(nombre)
            }
            _ => Err(self.err_expected("Identificador")),
        }
    }

    fn parse_lit(&mut self) -> Result<Literal, ParseError> {
        match self.token_stream.look_ahead(0, |t| t).clone() {
            Token::Literal(l) => {
                self.token_stream.bump();
                Ok(l)
            }
            _ => Err(self.err_expected("Literal")),
        }
    }
}
