use cmex_ast::TokenTree;
use cmex_lexer::Token;

pub struct TtCursor<'a> {
    iter: TtIter<'a>,
    peeked: Option<Option<Token>>,
}

impl<'a> TtCursor<'a> {
    pub fn new(tree: &'a Vec<TokenTree>) -> Self {
        Self {
            iter: TtIter::new(tree),
            peeked: None,
        }
    }

    pub fn next_tree(&mut self) -> Option<TokenTree> {
        self.iter.next_tree()
    }

    pub fn peek_tree(&mut self) -> Option<&TokenTree> {
        self.iter.peek_tree()
    }

    pub fn peek(&mut self) -> Option<Token> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).clone()
    }
}

impl Iterator for TtCursor<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }
}

/// A helper structure to traverse [`TokenTree`] as a flat token stream.
struct TtIter<'a> {
    stack: Vec<TtIter<'a>>,
    tt: &'a Vec<TokenTree>,
    curr: usize,
}

impl<'a> TtIter<'a> {
    pub fn new(tree: &'a Vec<TokenTree>) -> Self {
        TtIter {
            stack: Vec::new(),
            tt: tree,
            curr: 0,
        }
    }

    pub fn next_tree(&mut self) -> Option<TokenTree> {
        self.stack
            .pop()
            .and_then(|iter| iter.peek_tree().cloned())
            .or_else(|| {
                self.peek_tt().cloned().inspect(|_| {
                    self.curr += 1;
                })
            })
    }

    #[inline]
    pub fn peek_tree(&self) -> Option<&TokenTree> {
        self.stack
            .last()
            .map(|iter| iter.peek_tree())
            .unwrap_or_else(|| self.peek_tt())
    }

    #[inline]
    fn peek_tt(&self) -> Option<&TokenTree> {
        self.tt.get(self.curr)
    }
}

impl Iterator for TtIter<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cursor) = self.stack.last_mut() {
            cursor.next().or_else(|| {
                self.stack.pop();
                self.next()
            })
        } else {
            match &self.tt.get(self.curr) {
                Some(TokenTree::Token(tok)) => {
                    self.curr += 1;
                    Some(tok.clone())
                }
                Some(TokenTree::Delim(_, vec)) => {
                    self.stack.push(Self::new(vec));
                    self.curr += 1;
                    self.stack.last_mut()?.next()
                }
                None => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use cmex_ast::{DelimTag, TokenTree};
    use cmex_lexer::{Token, TokenTag::*};
    use cmex_span::Span;

    use crate::tt_cursor::{TtCursor, TtIter};

    #[test]
    fn tree_iterating() {
        let tree = vec![
            TokenTree::Delim(
                DelimTag::Paren,
                vec![
                    TokenTree::Token((Dollar, Span(0, 1))),
                    TokenTree::Token((Identifier("ident".into()), Span(1, 5))),
                    TokenTree::Delim(
                        DelimTag::Curly,
                        vec![
                            TokenTree::Token((Dollar, Span(7, 8))),
                            TokenTree::Token((Dollar, Span(8, 9))),
                        ],
                    ),
                ],
            ),
            TokenTree::Delim(
                DelimTag::Paren,
                vec![TokenTree::Token((
                    Identifier("foo".into()),
                    Span(10, 13),
                ))],
            ),
        ];

        let mut cursor = TtCursor::new(&tree);

        assert_eq!(
            cursor.next_tree(),
            Some(TokenTree::Delim(
                DelimTag::Paren,
                vec![
                    TokenTree::Token((Dollar, Span(0, 1))),
                    TokenTree::Token((Identifier("ident".into()), Span(1, 5))),
                    TokenTree::Delim(
                        DelimTag::Curly,
                        vec![
                            TokenTree::Token((Dollar, Span(7, 8))),
                            TokenTree::Token((Dollar, Span(8, 9))),
                        ]
                    ),
                ],
            ))
        );

        assert_eq!(
            cursor.next_tree(),
            Some(TokenTree::Delim(
                DelimTag::Paren,
                vec![TokenTree::Token((
                    Identifier("foo".into()),
                    Span(10, 13)
                ))]
            ))
        );
    }

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
                            vec![TokenTree::Token((
                                Identifier("foo".into()),
                                Span(10, 13),
                            ))],
                        ),
                    ],
                ),
            ],
        )];

        assert_eq!(
            TtIter::new(&tree).collect::<Vec<Token>>(),
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
                                TokenTree::Token((
                                    Identifier("foo".into()),
                                    Span(10, 13),
                                )),
                                TokenTree::Delim(
                                    DelimTag::Paren,
                                    vec![
                                        TokenTree::Token((
                                            Identifier("bar".into()),
                                            Span(13, 16),
                                        )),
                                        TokenTree::Delim(
                                            DelimTag::Paren,
                                            vec![TokenTree::Token((
                                                Identifier("baz".into()),
                                                Span(16, 19),
                                            ))],
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
            ],
        )];

        assert_eq!(
            TtIter::new(&tree).collect::<Vec<Token>>(),
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
            TtIter::new(&tree).collect::<Vec<Token>>(),
            vec![
                (Dollar, Span(76, 77)),
                (Identifier("e".into()), Span(77, 78)),
                (Colon, Span(78, 79)),
                (Identifier("expr".into()), Span(79, 83))
            ]
        );
    }

    #[test]
    fn subtrees_and_tokens() {
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
                                TokenTree::Token((
                                    Identifier("foo".into()),
                                    Span(10, 13),
                                )),
                                TokenTree::Delim(
                                    DelimTag::Paren,
                                    vec![
                                        TokenTree::Token((
                                            Identifier("bar".into()),
                                            Span(13, 16),
                                        )),
                                        TokenTree::Delim(
                                            DelimTag::Paren,
                                            vec![TokenTree::Token((
                                                Identifier("baz".into()),
                                                Span(16, 19),
                                            ))],
                                        ),
                                    ],
                                ),
                            ],
                        ),
                    ],
                ),
                TokenTree::Token((Identifier("ident".into()), Span(19, 24))),
            ],
        )];

        let mut cursor = TtIter::new(&tree);

        assert_eq!(cursor.next(), Some((Dollar, Span(0, 1))));

        assert_eq!(
            cursor.next(),
            Some((Identifier("ident".into()), Span(1, 5)))
        );

        assert_eq!(
            cursor.peek_tree(),
            Some(&TokenTree::Delim(
                DelimTag::Curly,
                vec![
                    TokenTree::Token((Dollar, Span(7, 8))),
                    TokenTree::Token((Dollar, Span(8, 9))),
                    TokenTree::Delim(
                        DelimTag::Paren,
                        vec![
                            TokenTree::Token((
                                Identifier("foo".into()),
                                Span(10, 13)
                            )),
                            TokenTree::Delim(
                                DelimTag::Paren,
                                vec![
                                    TokenTree::Token((
                                        Identifier("bar".into()),
                                        Span(13, 16)
                                    )),
                                    TokenTree::Delim(
                                        DelimTag::Paren,
                                        vec![TokenTree::Token((
                                            Identifier("baz".into()),
                                            Span(16, 19)
                                        )),]
                                    )
                                ]
                            )
                        ]
                    )
                ]
            ))
        );

        assert_eq!(cursor.next(), Some((Dollar, Span(7, 8))));

        assert_eq!(cursor.next(), Some((Dollar, Span(8, 9))));

        assert_eq!(
            cursor.peek_tree(),
            Some(&TokenTree::Delim(
                DelimTag::Paren,
                vec![
                    TokenTree::Token((Identifier("foo".into()), Span(10, 13))),
                    TokenTree::Delim(
                        DelimTag::Paren,
                        vec![
                            TokenTree::Token((
                                Identifier("bar".into()),
                                Span(13, 16)
                            )),
                            TokenTree::Delim(
                                DelimTag::Paren,
                                vec![TokenTree::Token((
                                    Identifier("baz".into()),
                                    Span(16, 19)
                                )),]
                            )
                        ]
                    )
                ]
            ))
        );

        assert_eq!(
            cursor.next(),
            Some((Identifier("foo".into()), Span(10, 13)))
        );

        assert_eq!(
            cursor.peek_tree(),
            Some(&TokenTree::Delim(
                DelimTag::Paren,
                vec![
                    TokenTree::Token((Identifier("bar".into()), Span(13, 16))),
                    TokenTree::Delim(
                        DelimTag::Paren,
                        vec![TokenTree::Token((
                            Identifier("baz".into()),
                            Span(16, 19)
                        )),]
                    )
                ]
            ))
        );

        assert_eq!(
            cursor.next(),
            Some((Identifier("bar".into()), Span(13, 16)))
        );

        assert_eq!(
            cursor.peek_tree(),
            Some(&TokenTree::Delim(
                DelimTag::Paren,
                vec![TokenTree::Token((
                    Identifier("baz".into()),
                    Span(16, 19)
                )),]
            ))
        );

        assert_eq!(
            cursor.next(),
            Some((Identifier("baz".into()), Span(16, 19)))
        );

        assert_eq!(
            cursor.next(),
            Some((Identifier("ident".into()), Span(19, 24)))
        );
    }
}
