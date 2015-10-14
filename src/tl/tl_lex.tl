# Sep is the set of separator characters on which to split.
# The separator characters are left in the output sequence, while
# the characters in between the separators are collected into strings.

# TODO: move this to a file that can be used by both tl_lex and sexp

lex_split =
{Sep ↦
 {'' ↦ [],
  c : cs | r = lex_split Sep cs
   (| c ∈ Sep  ↦ c : r
    |          ↦ {[] ↦ [[c]],
                   t : ts | t ∈ Sep  ↦ [c] : r
                          |          ↦ c : t : ts
                  } r
   )
 }
}

index_lines =
{s ∈. Str | N = {\newline} ↦
 zip (1 .. ∞) (remove N (lex_split N s))}

index_cols =
{(line_index ≥. 1, line ∈. Str) ↦
 map {(col_index, c) ↦ (line_index, col_index, c)}
     (zip (1 .. ∞) line)}

tag_with_line_col =
{s ∈. Str ↦
 mapcat index_cols (index_lines s)}

# TODO: add char index (including newlines)
