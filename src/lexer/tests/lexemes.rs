#[cfg(test)]
mod tests {
    use crate::lexer::{Lexemes, Lexer};

    #[test]
    fn lexemes() {
        let lexer = Lexer::from("
            ... . > >> >>= >= < << <<= <=
            + += ++ - -= -- -> * *=
            / /= % %= & &= && ^ ^=
            | |= || = == ! != ; { } , :
            ( ) [ ] ~ ?
            static void identifier
        ");

        let lexemes = Lexemes::new(lexer.spanned());

        assert_eq!(
            lexemes.collect::<Vec<String>>(),
            vec![
                "...", ".", ">", ">>", ">>=",
                ">=", "<", "<<", "<<=", "<=",
                "+", "+=", "++", "-", "-=",
                "--", "->", "*", "*=", "/",
                "/=", "%", "%=", "&", "&=",
                "&&", "^", "^=", "|", "|=",
                "||", "=", "==", "!", "!=",
                ";", "{", "}", ",", ":", "(",
                ")", "[", "]", "~", "?",
                "static", "void", "identifier"
            ]
                .into_iter()
                .map(|s| String::from(s))
                .collect::<Vec<String>>()
        );
    }
}