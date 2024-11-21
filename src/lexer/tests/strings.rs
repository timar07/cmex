#[cfg(test)]
mod tests {
    use crate::lexer::{
        token::Token::{self, *},
        Lexer
    };

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
            lexer.collect::<Vec<Token>>(),
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
