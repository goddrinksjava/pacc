use std::fmt;

use crate::symbols::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // valores que puede tomar el Token
    Asignacion,
    Tipo(Tipo),
    BoolBinOp(BoolBinOp),
    No,
    Literal(Literal),
    Const,
    Desde,
    Si,
    Sino,
    Hasta,
    Mientras,
    Regresa,
    Hacer,
    Incr,
    Repite,
    Que,
    Mas,
    Menos,
    NumBinOp(NumBinOp),
    PuntoComa,
    Coma,
    ParenA,
    ParenC,
    LlaveA,
    LlaveC,
    CorcheteA,
    CorcheteC,
    DosPuntos,
    Punto,
    Comp(Comp),
    Identificador(String),
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Token {
    pub fn can_begin_expr(&self) -> bool {
        matches!(
            self,
            Token::No
                | Token::Literal(_)
                | Token::Mas
                | Token::Menos
                | Token::ParenA
                | Token::Identificador(_)
        )
    }
}
