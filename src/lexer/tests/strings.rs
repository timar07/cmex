#[cfg(test)]
mod tests {
    use crate::lexer::{
        token::TokenTag::{self, *}, LexError, Lexer
    };

    #[test]
    fn character_literal() {
        let lexer = Lexer::from("'a' '' '");

        assert_eq!(
            lexer.collect::<Vec<Result<TokenTag, LexError>>>(),
            vec![
                Ok(CharLiteral),
                Err(LexError::EmptyCharacterConstant),
                Err(LexError::UnterminatedCharacterLiteral)
            ]
        )
    }

    #[test]
    fn strings_literals() {
        let lexer = Lexer::from("\
            \"hello, world\"
            \"hello \\b world\"
            \"hello \\111 world\"
            \"hello \\x11 world\"
            \"this string is \\\" terminated\"
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<TokenTag, LexError>>>()
                .into_iter()
                .flatten()
                .collect::<Vec<TokenTag>>(),
            vec![
                StringLiteral,
                StringLiteral,
                StringLiteral,
                StringLiteral,
                StringLiteral,
            ]
        );
    }
}