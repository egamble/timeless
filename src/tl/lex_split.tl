# Sep is the set of separator characters on which to split.
# The separator characters are left in the output sequence, while
# the characters in between the separators are collected into strings.

| f =
{Sep << Char -> c @ Char -> [] -> [Str;[c]],
                        -> t:ts | t ∈ Sep -> Str;[c] : r,
                                | t ∉ Sep -> c : t : ts}

| lex_split =
{Sep ⊂ Char -> "" -> [],
            -> c:cs ∈ Str | r = lex_split Sep cs | c ∈ Sep -> c:r,
                                                 | c ∉ Sep -> f Sep c r}
