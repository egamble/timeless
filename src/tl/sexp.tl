# A parser that recognizes S-expressions and builds an AST

WS             = {`space, `tab, `newline}
Paren          = {`(, `)}

Alpha          = Im(`a..`z ++ `A..`Z)
Alphanum       = Alpha ∪ Im(`0..`9)
Alphanum_str   = {'', (∈ Alphanum):(∈ Alphanum_str)}
Symbol         = {(∈ Alpha):(∈ Alphanum_str)}

# Sep is the set of separator characters on which to split.
# The separator characters are left in the output sequence, while
# the characters in between the separators are collected into strings.

lex_split =
  {Sep ->
    {''                          -> [],
     c:cs | r = lex_split Sep cs -> {| c ∈ Sep -> c:r
                                     |         -> {[]             -> [[c]],
                                                   t:ts | t ∈ Sep -> [c]:r
                                                        |         -> c:t:ts}
                                                  r}}}

lex_remove_ws = remove WS

# Check that all tokens are either parenthesis characters or symbol strings.

lex_check = map {t ∈ Symbol ∪ Paren -> t}

lex = lex_check ∘ lex_remove_ws ∘ lex_split (WS ∪ Paren)

parse =
  {[]                       -> [],
   (t ∉ Paren):ts           -> t : parse ts,
   '(' ++ body ++ ')' ++ ts -> parse body : parse ts}

lex_parse = parse ∘ lex

# The generated AST is ignored in this example definition of stdout.

stdout =
  {s | _ = lex_parse s -> "match"
     |                 -> "no match"}
  stdin

# This is an equivalent definition of stdout.

# stdout =
#   {∈ Dm lex_parse -> "match",
#    _              -> "no match"}
#   stdin
