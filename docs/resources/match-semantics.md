## Weak delimiter matching

Suppose we have a pattern like `(:[1])`. We could weaken delimiter matching to
allow matching, for example, anything except parens, i.e., `(])` would be valid
and could be matched against. In strict delimiter matching, where `[]` must be
balanced, `(])` would not be valid. This could, in theory, give a bit of a
performance bump since we wouldn't neet to ensure well-balancedness with
respect to any other delimiters besides `()`.

Weak delimiter matching only works for unique delimiters. For example, the
trick does not work for a closing delimiter like `end` in Ruby where multiple
opening delimiters like `class` or `def` close with `end`. In this case, we
have no choice but to check well-balancedness of all delimiters with the same
closing delimiter.

## Matching alphanumeric delimiters

A neat thing about Comby is that holes can match at the character level (not
just word boundaries). This makes it a little bit more challenging to identify
alphanumeric delimiters like `for`, `end`, because a character sequence like
`for` in the word `before` would, under normal circumstances, under character
matching, trigger delimiter matching, and Comby will look for an `end` to the
`for` in `before`. This problem doesn't come up for `()` delimiters, for
example, because punctuation isn't mixed with alphanumberic sequences. The way
we deal with this complexity in Comby is as follows:

We detect alpanumeric delimiters by requiring surrounding content (such as
whitespace or other delimiters). Before something like 'def', we expect
whitespace, punctuation delimiters like ')', and nothing. After 'def', we
expect similar sequences. Buf after something like 'end', we expect the same
but also punctuation like `;` or `.`.

There's some complexity for detecting these cases: we don't consume the
trailing whitespace, because that would stop us from detecting `begin begin`,
separated by a single space. So the trailing part is only checked as a look
ahead, but not consumed. We do, however, need to consume the prefix in order to
advance the state (otherwise, the prefix whitespace would be handled normally,
and we would re-encounter the delimiter subsequently).

## Choice operator behavior and 'attempt'

If `a1` succees in the parse sequence `a1 >>= a2 >>= a3 <|> b1 >>= b2 >>= b3`,
the whole parser will fail, and `b1 >>= ...` will never be attempted. Putting
`attempt @@ a1 >>= a2 >>= a3 <|> attempt @@ b1 >>= ...` will backtrack if `a1`
succeeds but `a2` fails, and will then try `b1`. This pattern is important for
disambiguating holes `:[1]` and `:[[1]]`, and alphanumeric sequences (like
`begin`, `struct`) where we need to check if the sequence initiates a balanced
delimiter matching or not.
