pub mod token;
pub mod token_stream;

use std::{collections::HashMap, iter::Peekable, mem, str::Chars};

use crate::symbols::*;

use self::{token::Token, token_stream::TokenStream};

//posicion de los token, para los mensaje de error
#[derive(Debug, PartialEq, Clone, Default)]
pub struct Loc {
    pub line: u32, // numero sin signo de 32 bits "u32"
    pub col: u32,
}

pub struct Lexer<'a> {
    loc: Loc,
    it: Peekable<Chars<'a>>,
    buffer: String,
    tokens: Vec<Token>,
    locs: Vec<Loc>,
    tok_loc: Loc,
    palabras_reservadas: HashMap<&'static str, Token>,
}
//palabras reservadas
impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            loc: Loc::default(),
            it: input.chars().peekable(),
            buffer: String::default(),
            tokens: Vec::new(),
            locs: Vec::new(),
            tok_loc: Loc::default(),
            palabras_reservadas: HashMap::from([
                ("regresa", Token::Regresa),
                ("nulo", Token::Tipo(Tipo::Nulo)),
                ("entero", Token::Tipo(Tipo::Entero)),
                ("decimal", Token::Tipo(Tipo::Decimal)),
                ("palabra", Token::Tipo(Tipo::Palabra)),
                ("logico", Token::Tipo(Tipo::Logico)),
                ("no", Token::No),
                ("constante", Token::Const),
                ("desde", Token::Desde),
                ("hasta", Token::Hasta),
                ("mientras", Token::Mientras),
                ("hacer", Token::Hacer),
                ("si", Token::Si),
                ("sino", Token::Sino),
                ("incr", Token::Incr),
                ("repite", Token::Repite),
                ("que", Token::Que),
                ("falso", Token::Literal(Literal::Logico(false))),
                ("verdadero", Token::Literal(Literal::Logico(true))),
                ("y", Token::BoolBinOp(BoolBinOp::Y)),
                ("o", Token::BoolBinOp(BoolBinOp::O)),
            ]),
        }
    }

    //verificamos la memoria
    pub fn lex(mut self) -> Result<TokenStream, String> {
        self.begin_init()?;
        Ok(TokenStream::new(self.tokens, self.locs))
    }

    fn begin_init(&mut self) -> Result<(), String> {
        while let Some(c) = self.next() {
            match c {
                ' ' | '\t' => self.whitespace(), //caracteres de escape
                '\r' => {
                    self.eat('\n');
                    self.new_line();
                }
                '\n' => self.new_line(),
                '=' => {
                    if self.eat('=') {
                        self.push(Token::Comp(Comp::Igual));
                    } else {
                        self.push(Token::Asignacion);
                    }
                }
                '>' => {
                    if self.eat('=') {
                        self.push(Token::Comp(Comp::MayorIgual));
                    } else {
                        self.push(Token::Comp(Comp::Mayor));
                    }
                }
                '<' => {
                    if self.eat('=') {
                        self.push(Token::Comp(Comp::MenorIgual));
                    } else if self.eat('>') {
                        self.push(Token::Comp(Comp::Diferente));
                    } else {
                        self.push(Token::Comp(Comp::Menor));
                    }
                }
                digit @ '0'..='9' => {
                    self.buffer.push(digit);
                    self.begin_number()?
                }
                '"' => self.begin_string()?,
                '+' => self.push(Token::Mas), // Operador unario o binario
                '-' => self.push(Token::Menos), // Operador unario o binario
                '*' => self.push(Token::NumBinOp(NumBinOp::Multiplicacion)),
                '/' => self.push(Token::NumBinOp(NumBinOp::Division)),
                '%' => self.push(Token::NumBinOp(NumBinOp::Modulo)),
                '^' => self.push(Token::NumBinOp(NumBinOp::Potenciacion)),
                ';' => self.push(Token::PuntoComa),
                ':' => self.push(Token::DosPuntos),
                '.' => self.push(Token::Punto),
                ',' => self.push(Token::Coma),
                '(' => self.push(Token::ParenA),
                ')' => self.push(Token::ParenC),
                '{' => self.push(Token::LlaveA),
                '}' => self.push(Token::LlaveC),
                '[' => self.push(Token::CorcheteA),
                ']' => self.push(Token::CorcheteC),
                chara @ ('_' | 'A'..='Z' | 'a'..='z') => {
                    self.buffer.push(chara);

                    let mut peeked = self.peek();
                    while let Some(&i) = peeked {
                        if i.is_ascii_alphanumeric() || i == '_' {
                            self.next();
                            self.buffer.push(i);
                            peeked = self.peek();
                        } else {
                            peeked = None
                        }
                    }

                    match self.palabras_reservadas.get(&self.buffer[..]) {
                        Some(t) => {
                            mem::take(&mut self.buffer);
                            self.push(t.clone());
                        }
                        None => {
                            let id = mem::take(&mut self.buffer);
                            self.push(Token::Identificador(id));
                        }
                    }
                }
                other => {
                    return Err(format!("Unexpected character {}", other));
                }
            }
        }

        Ok(())
    }
    //verificacion de decimal, entero
    fn begin_number(&mut self) -> Result<(), String> {
        let mut es_decimal = false;

        while let Some(&c) = self.it.peek() {
            match c {
                digit @ '0'..='9' => {
                    self.next();
                    self.buffer.push(digit);
                }
                '.' => {
                    self.next();

                    if es_decimal {
                        return Err("Número no puede tener más de un punto decimal".to_string());
                    }

                    es_decimal = true;
                    self.buffer.push('.');
                }
                //f32 = numro decimal de 32 bits
                _ => {
                    if es_decimal {
                        let decimal = mem::take(&mut self.buffer)
                            .parse::<f32>()
                            .map_err(|e| e.to_string())?;
                        self.push(Token::Literal(Literal::Decimal(decimal)));
                    } else {
                        let entero = mem::take(&mut self.buffer)
                            .parse::<i32>()
                            .map_err(|e| e.to_string())?;
                        self.push(Token::Literal(Literal::Entero(entero)));
                    }

                    return Ok(());
                }
            }
        }

        Ok(())
    }

    fn begin_string(&mut self) -> Result<(), String> {
        loop {
            match self.next_or_fail()? {
                '"' => {
                    let str = mem::take(&mut self.buffer); // nos devulve el valor que tiene, y despues limpia (borra)
                    self.push(Token::Literal(Literal::Palabra(str))); //retornamos el contenido del string(palabra) y lo mandamos literalmente
                    return Ok(());
                }
                '\\' => match self.next_or_fail()? {
                    'n' => self.buffer.push('\n'),
                    'r' => self.buffer.push('\r'),
                    't' => self.buffer.push('\t'),
                    '\\' => self.buffer.push('\\'),
                    '"' => self.buffer.push('"'),
                    chara => return Err(format!("Carácter de escape inválido \\{}", chara)),
                },
                '\n' => return Err("Se esperaba cierre de palabra".to_string()),
                chara => {
                    self.buffer.push(chara);
                }
            }
        }
    }

    fn next(&mut self) -> Option<char> {
        //lectura
        self.loc.col += 1;
        self.it.next()
    }

    fn next_or_fail(&mut self) -> Result<char, String> {
        //si hay un error en la lectura
        self.loc.col += 1;
        let n = self.it.next();

        match n {
            Some(n) => Ok(n),
            None => Err("Fin del archivo inesperado".to_string()),
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.it.peek()
    }

    fn new_line(&mut self) {
        self.loc.line += 1;
        self.loc.col = 0;
        self.tok_loc = self.loc.clone();
    }

    fn whitespace(&mut self) {
        self.tok_loc = self.loc.clone(); //posicion del token (inicio) // linea de lectura
    }

    fn eat(&mut self, chara: char) -> bool {
        if self.peek() == Some(&chara) {
            self.next();
            true
        } else {
            false
        }
    }

    fn push(&mut self, tok: Token) {
        self.tokens.push(tok);
        self.locs.push(self.tok_loc.clone());
        self.tok_loc = self.loc.clone();
    }
}
