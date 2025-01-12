use std::collections::VecDeque;

use cmex_ast::TokenTree;
use cmex_lexer::Token;

/// A helper structure to traverse [`TokenTree`] as a flat token stream.
pub struct TtCursor<'a> {
    stack: VecDeque<Token>,
    tt: &'a Vec<TokenTree>,
    curr: usize,
}

impl<'a> TtCursor<'a> {
    pub fn new(tree: &'a Vec<TokenTree>) -> Self {
        TtCursor {
            stack: VecDeque::new(),
            tt: tree,
            curr: 0
        }
    }

    pub fn get_subtree(&mut self) -> Option<TokenTree> {
        self.tt.get(self.curr).cloned()
    }
}

impl<'a> Iterator for TtCursor<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(tok) = self.stack.pop_front() {
            Some(tok)
        } else {
            match &self.tt.get(self.curr) {
                Some(TokenTree::Token(tok)) => {
                    self.curr += 1;
                    Some(tok.clone())
                },
                Some(TokenTree::Delim(_, vec)) => {
                    self.stack.append(&mut Self::new(vec).collect());
                    self.curr += 1;
                    Some(self.stack.pop_front()?)
                },
                None => None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use cmex_ast::{DelimTag, TokenTree};
    use cmex_lexer::{Token, TokenTag::*};
    use cmex_span::Span;

    use crate::tt_cursor::TtCursor;

    #[test]
    fn nested_structures_flat_iterator() {
        let tree = vec![TokenTree::Delim(
            DelimTag::Paren,
            vec![
                TokenTree::Token((Dollar, Span(0, 1))),
                TokenTree::Token((Identifier("ident".into()), Span(1, 5))),
                TokenTree::Delim(
                    DelimTag::Curly,
                    vec![
                        TokenTree::Token((Dollar, Span(7, 8))),
                        TokenTree::Token((Dollar, Span(8, 9))),
                        TokenTree::Delim(
                            DelimTag::Paren,
                            vec![
                                TokenTree::Token((Identifier("foo".into()), Span(10, 13)))
                            ]
                        )
                    ]
                )
            ]
        )];

        assert_eq!(
            TtCursor::new(&tree).collect::<Vec<Token>>(),
            vec![
                (Dollar, Span(0, 1)),
                (Identifier("ident".into()), Span(1, 5)),
                (Dollar, Span(7, 8)),
                (Dollar, Span(8, 9)),
                (Identifier("foo".into()), Span(10, 13))
            ]
        );
    }

    #[test]
    fn deeply_nested() {
        let tree = vec![TokenTree::Delim(
            DelimTag::Paren,
            vec![
                TokenTree::Token((Dollar, Span(0, 1))),
                TokenTree::Token((Identifier("ident".into()), Span(1, 5))),
                TokenTree::Delim(
                    DelimTag::Curly,
                    vec![
                        TokenTree::Token((Dollar, Span(7, 8))),
                        TokenTree::Token((Dollar, Span(8, 9))),
                        TokenTree::Delim(
                            DelimTag::Paren,
                            vec![
                                TokenTree::Token((Identifier("foo".into()), Span(10, 13))),
                                TokenTree::Delim(
                                    DelimTag::Paren,
                                    vec![
                                        TokenTree::Token((Identifier("bar".into()), Span(13, 16))),
                                        TokenTree::Delim(
                                            DelimTag::Paren,
                                            vec![
                                                TokenTree::Token((Identifier("baz".into()), Span(16, 19))),
                                            ]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]
                )
            ]
        )];

        assert_eq!(
            TtCursor::new(&tree).collect::<Vec<Token>>(),
            vec![
                (Dollar, Span(0, 1)),
                (Identifier("ident".into()), Span(1, 5)),
                (Dollar, Span(7, 8)),
                (Dollar, Span(8, 9)),
                (Identifier("foo".into()), Span(10, 13)),
                (Identifier("bar".into()), Span(13, 16)),
                (Identifier("baz".into()), Span(16, 19)),
            ]
        );
    }

    #[test]
    fn flat_tree() {
        let tree = vec![
            TokenTree::Token((Dollar, Span(76, 77))),
            TokenTree::Token((Identifier("e".into()), Span(77, 78))),
            TokenTree::Token((Colon, Span(78, 79))),
            TokenTree::Token((Identifier("expr".into()), Span(79, 83))),
        ];

        assert_eq!(
            TtCursor::new(&tree).collect::<Vec<Token>>(),
            vec![
                (Dollar, Span(76, 77)),
                (Identifier("e".into()), Span(77, 78)),
                (Colon, Span(78, 79)),
                (Identifier("expr".into()), Span(79, 83))
            ]
        );
    }
}
