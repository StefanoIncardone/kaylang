#[derive( Debug )]
pub enum TokenKind {
    Unexpected,
    OpenParenthesis,
    CloseParenthesis,
    Digit,
    Plus,
    Minus,
    Times,
    Divide,
    Pow,
    Equals,
}

#[derive( Debug )]
pub struct Token {
    kind: TokenKind,
    text: String,
}


#[derive( Debug )]
struct AST {
    tokens: Vec<Token>,
}

impl From<&str> for AST {
    fn from( source_code: &str ) -> Self {
        let mut tokens = Vec::new();
        let mut src = source_code.chars().peekable();
        let mut text = String::new();

        while let Some( ch ) = src.next() {
            let token = match ch {
                ' ' => continue,
                '(' => Token{ kind: TokenKind::OpenParenthesis, text: ch.to_string() },
                ')' => Token{ kind: TokenKind::CloseParenthesis, text: ch.to_string() },
                '+' => Token{ kind: TokenKind::Plus, text: ch.to_string() },
                '-' => Token{ kind: TokenKind::Minus, text: ch.to_string() },
                '*' => Token{ kind: TokenKind::Times, text: ch.to_string() },
                '/' => Token{ kind: TokenKind::Divide, text: ch.to_string() },
                '^' => Token{ kind: TokenKind::Pow, text: ch.to_string() },
                '=' => Token{ kind: TokenKind::Equals, text: ch.to_string() },
                '0'..='9' => {
                    text.clear();

                    text.push( ch );
                    while let Some( digit ) = src.next_if( |ch| ch.is_numeric() ) {
                        text.push( digit );
                    }

                    Token{ kind: TokenKind::Digit, text: text.clone() }
                },
                _ => {
                    text.clear();

                    text.push( ch );
                    while let Some( ch ) = src.next_if( |ch| ch.is_alphabetic() ) {
                        text.push( ch );
                    }

                    Token{ kind: TokenKind::Unexpected, text: text.clone() }
                }
            };

            tokens.push( token );
        }

        return Self{ tokens };
    }
}


fn main() {
    let ast = AST::from( "2 + 2 = 4 - 1 = 3 * 2 = 6 / 3 = 2 (quick maths)" );

    println!( "{:?}", ast );
}
