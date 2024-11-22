#[cfg(test)]
mod tests {
    use crate::lexer::{
        Lexer,
        token::{
            NumberLiteralKind,
            NumberLiteralPrefix,
            NumberLiteralSuffix,
            Token::{self, *}
        },
        LexError::{self, *}
    };

    #[test]
    fn decimal_integers() {
        let lexer = Lexer::from("\
            123
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .into_iter()
                .flatten()
                .collect::<Vec<Token>>(),
            vec![
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Int
                },
            ]
        );
    }

    #[test]
    fn floats() {
        let lexer = Lexer::from("
            3.141592 1.4142l 1.4142f
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .into_iter()
                .flatten()
                .collect::<Vec<Token>>(),
            vec![
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Float
                },
                NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::Long),
                    kind: NumberLiteralKind::Float
                },
                NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::Float),
                    kind: NumberLiteralKind::Float
                },
            ]
        )
    }

    #[test]
    fn hex_and_oct_integers() {
        let lexer = Lexer::from("
            0x234 0x234ul
            0xefg
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>(),
            vec![
                Ok(NumberLiteral {
                    prefix: Some(NumberLiteralPrefix::Hex),
                    suffix: None,
                    kind: NumberLiteralKind::Int
                }),
                Ok(NumberLiteral {
                    prefix: Some(NumberLiteralPrefix::Hex),
                    suffix: Some(NumberLiteralSuffix::UnsignedLong),
                    kind: NumberLiteralKind::Int
                }),
                Err(InvalidNumberLiteralSuffix("g".into()))
            ]
        )
    }

    #[test]
    fn exponents() {
        let lexer = Lexer::from("
            123e3 123e-3
            123e3l 123e-3l
            1.6e19f 1.6e-19f
            123e- 1.6e19lf
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>(),
            vec![
                Ok(NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Exponent
                }),
                Ok(NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Exponent
                }),
                Ok(NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::Long),
                    kind: NumberLiteralKind::Exponent
                }),
                Ok(NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::Long),
                    kind: NumberLiteralKind::Exponent
                }),
                Ok(NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::Float),
                    kind: NumberLiteralKind::Exponent
                }),
                Ok(NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::Float),
                    kind: NumberLiteralKind::Exponent
                }),
                Err(ExponentHasNoDigits),
                Err(InvalidNumberLiteralSuffix("lf".into()))
            ]
        )
    }

    #[test]
    fn valid_suffixes() {
        let lexer = Lexer::from("
            123l 123L
            123ll 123LL
            123u 123U
            123ul 123uL 123lu 123Lu
            123Ul 123UL 123lU 123LU
            123ull 123uLL 123llu 123LLu
            123Ull 123ULL 123llU 123LLU
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .iter()
                .all(|t| t.is_ok()),
            true
        )
    }

    #[test]
    fn invalid_suffixes() {
        let lexer = Lexer::from("
            123lL 123lL
            123ulu 123uLu 123lul 123lUl
            123Ulu 123uLU 123Lul 123LUl
            123ulU 123ULu 123luL 123lUL
            123UlU 123ULU 123LuL 123LUL

            123ulL 123uLl 123Llu 123lLu
            123UlL 123ULl 123LlU 123lLU

            0xefg 0b1012 0o178
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .iter()
                .all(|t| t.is_err()),
            true
        )
    }

    #[test]
    fn comments() {
        let lexer = Lexer::from("\
            /* this is a comment */
            int f() {
                return 1; /* always one */
            }

            /*
            int x() {
                return 0;
            }
            */

            int a() {
                return 12321;
            }
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .into_iter()
                .flatten()
                .collect::<Vec<Token>>(),
            vec![
                Int,
                Identifier("f".into()),
                LeftParen,
                RightParen,
                LeftCurly,
                Return,
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Int
                },
                Semicolon,
                RightCurly,

                Int,
                Identifier("a".into()),
                LeftParen,
                RightParen,
                LeftCurly,
                Return,
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Int
                },
                Semicolon,
                RightCurly
            ]
        );

    }

    #[test]
    fn hello_world() {
        let lexer = Lexer::from("\
            int main() {
                printf(\"Hello, world!\");
                return 0;
            }
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .into_iter()
                .flatten()
                .collect::<Vec<Token>>(),
            vec![
                Int,
                Identifier("main".into()),
                LeftParen,
                RightParen,
                LeftCurly,
                Identifier("printf".into()),
                LeftParen,
                StringLiteral,
                RightParen,
                Semicolon,
                Return,
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Int
                },
                Semicolon,
                RightCurly
            ]
        )
    }

    #[test]
    fn one_or_two_character_tokens() {
        let lexer = Lexer::from("\
            ... . > >> >>= >= < << <<= <=
            + += ++ - -= -- -> * *=
            / /= % %= & &= && ^ ^=
            | |= || = == ! != ; { } , :
            ( ) [ ] ~ ?
        ");

        assert_eq!(
            lexer.collect::<Vec<Result<Token, LexError>>>()
                .into_iter()
                .flatten()
                .collect::<Vec<Token>>(),
            vec![
                Ellipsis, Dot, Gt, Right, RightAssign,
                Ge, Lt, Left, LeftAssign, Le, Plus,
                AddAssign, Increment, Minus, SubAssign,
                Decrement, ArrowRight, Asterisk, MulAssign,
                Slash, DivAssign, Mod, ModAssign, Ampersand,
                AndAssign, And, Circ, XorAssign, Bar, OrAssign,
                Or, Assign, Eq, Not, Neq, Semicolon, LeftCurly,
                RightCurly, Comma, Colon, LeftParen, RightParen,
                LeftBrace, RightBrace, Tilde, Quest
            ]
        )
    }
}
