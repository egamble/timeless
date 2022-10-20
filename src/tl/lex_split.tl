# Sep is the set of separator characters on which to split.
# The separator characters are left in the output sequence, while
# the characters in between the separators are collected into strings.

| f :=
{Sep << Char -> c @ Char -> [] -> [c:""],
                        -> t:s | t ∈ Sep -> c:"" : r,
                               | t ∉ Sep -> c : t : s
}

| lex_split :=
{Sep ⊂ Char -> "" -> [],
            -> c:s ∈ Str | r = lex_split Sep s | c ∈ Sep -> c:r,
                                               | c ∉ Sep -> f Sep c r
}
