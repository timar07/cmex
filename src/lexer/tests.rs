#[cfg(test)]
mod tests {
    use crate::lexer::{
        token::{NumberLiteralKind, NumberLiteralPrefix, NumberLiteralSuffix, Token::{self, *}},
        Lexer
    };

    #[test]
    fn numbers() {
        let lexer = Lexer::from("\
            123 3.141592 1.4142ll
            0x234 0x234ul
            123e3 123e-3 123e3lu
        ");

        assert_eq!(
            lexer.collect::<Vec<Token>>(),
            vec![
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Int
                },
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Float
                },
                NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::LongLong),
                    kind: NumberLiteralKind::Float
                },
                NumberLiteral {
                    prefix: Some(NumberLiteralPrefix::Hex),
                    suffix: None,
                    kind: NumberLiteralKind::Int
                },
                NumberLiteral {
                    prefix: Some(NumberLiteralPrefix::Hex),
                    suffix: Some(NumberLiteralSuffix::UnsignedLong),
                    kind: NumberLiteralKind::Int
                },
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Exponent
                },
                NumberLiteral {
                    prefix: None,
                    suffix: None,
                    kind: NumberLiteralKind::Exponent
                },
                NumberLiteral {
                    prefix: None,
                    suffix: Some(NumberLiteralSuffix::UnsignedLong),
                    kind: NumberLiteralKind::Exponent
                }
            ]
        );
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
            lexer.collect::<Vec<Token>>(),
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
            lexer.collect::<Vec<Token>>(),
            vec![
                Int,
                Identifier("main".into()),
                LeftParen,
                RightParen,
                LeftCurly,
                Identifier("printf".into()),
                LeftParen,
                StringLiteral("Hello, world!".into()),
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
            lexer.collect::<Vec<Token>>(),
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

    #[test]
    fn keywords() {
        let lexer = Lexer::from("\
            auto break case char const \
            continue default do double \
            else enum extern float for \
            goto if int long register \
            return short signed sizeof \
            static struct switch typedef \
            union unsigned void volatile \
            while
        ");

        assert_eq!(
            lexer.collect::<Vec<Token>>(),
            vec![
                Auto, Break, Case, Char, Const,
                Continue, Default, Do, Double,
                Else, Enum, Extern, Float, For,
                Goto, If, Int, Long, Register,
                Return, Short, Signed, Sizeof,
                Static, Struct, Switch, Typedef,
                Union, Unsigned, Void, Volatile,
                While
            ]
        )
    }
}
