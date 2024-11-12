mod lexer;

use lexer::Lexer;
use lexer::Token;

fn main() {
    let mut lexer = Lexer::from("\
              break case char const \
            continue default do double \
            else enum extern float for \
            goto if int long register \
            return short signed sizeof \
            static struct switch typedef \
            union unsigned void volatile \
            while
        ");
    println!(
        "\
              break case char const \
            continue default do double \
            else enum extern float for \
            goto if int long register \
            return short signed sizeof \
            static struct switch typedef \
            union unsigned void volatile \
            while
        "
    );
    dbg!(lexer.collect::<Vec<Token>>());
}
