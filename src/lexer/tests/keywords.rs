#[cfg(test)]
mod tests {
    use crate::lexer::{
        token::Token::{self, *},
        Lexer
    };

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