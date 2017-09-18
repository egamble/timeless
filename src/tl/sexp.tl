#use lib
#use lex_split

# A parser that recognizes S-expressions and builds an AST

WS             = {`space, `tab, `newline}
Paren          = {`(, `)}

Alpha          = Im(`a..`z ++ `A..`Z)
Alphanum       = Alpha ∪ Im(`0..`9)
Alphanum_str   = {'', c:s | c ∈ Alphanum
                            s ∈ Alphanum_str}
Symbol         = {c:s | c ∈ Alpha
                        s ∈ Alphanum_str}

lex_remove_ws = remove WS

# Check that all tokens are either parenthesis characters or symbol strings.

lex_check = map {t ∈ Symbol ∪ Paren -> t}

lex = lex_check ∘ lex_remove_ws ∘ lex_split (WS ∪ Paren)

parse =
{[]                       -> [],
 t:ts | t ∉ Paren         -> t : parse ts,
 '(' ++ body ++ ')' ++ ts -> parse body : parse ts}

lex_parse = parse ∘ lex

Sexp = Dm lex_parse

# The generated AST is ignored in this example definition of stdout.

stdout =
{∈ Sexp -> "match",
 ∉ Sexp -> "no match"}
