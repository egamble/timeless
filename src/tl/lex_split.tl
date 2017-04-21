# Sep is the set of separator characters on which to split.
# The separator characters are left in the output sequence, while
# the characters in between the separators are collected into strings.

lex_split =
  {Sep ->
    {''                          -> [],
     c:cs | r = lex_split Sep cs -> {| c ∈ Sep -> c:r,
                                               -> {[]             -> [[c]],
                                                   t:ts | t ∈ Sep -> [c]:r,
                                                                  -> c:t:ts
                                                  } r
                                    }}}
