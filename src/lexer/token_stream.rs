use super::{token::Token, Loc};

#[derive(Clone)]//clonamos el valor
pub struct TokenStream {
    pub tokens: Vec<Token>,
    locs: Vec<Loc>,
    tokens_i: usize,
    last_seen: usize,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>, locs: Vec<Loc>) -> TokenStream {
        TokenStream {
            tokens,
            locs,
            tokens_i: 0,
            last_seen: 0,
        }
    }

    pub fn bump(&mut self) { //indice del caracter que llevamos
        self.tokens_i += 1;
    }

    pub fn look_ahead<'a, F, R>(&'a mut self, offset: usize, fn_: F) -> R //genericos
    where
        F: Fn(&'a Token) -> R,
    {
        self.last_seen = self.tokens_i + offset; //leemos una posicion delante de la actual
        fn_(self.tokens.get(self.last_seen).unwrap_or(&Token::Eof))
    }

    pub fn last_seen_loc(&self) -> Loc {
        self.locs.get(self.last_seen).cloned().unwrap_or_default()
    }

    pub fn last_seen_tok(&self) -> Token {
        self.tokens
            .get(self.last_seen)
            .cloned()
            .unwrap_or(Token::Eof)
    }
}

#[macro_export]
macro_rules! eat {
    ($token_stream:expr, $( $pattern:pat_param )|+ $( if $guard: expr )?, $err:expr) => {
        if $token_stream.look_ahead(0, |t| matches!(t, $( $pattern )|+ $( if $guard )?)) {
            $token_stream.bump();
            Ok(())
        } else {
            Err($err)
        }
    };
}
