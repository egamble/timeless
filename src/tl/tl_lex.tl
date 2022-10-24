#include src/tl/lex_split


| newline = "
"

| index_lines =
{`s ∈ Str ->
 zip [1..] (remove N (lex_split N s)) | `N = {newline}
}

| index_cols =
{[`line_index ≥ 1, `line ∈ Str] ->
 map {[`col_index, `c] -> [line_index, col_index, c]}
     (zip [1..] line)
}

| tag_with_line_col =
{`s ∈ Str ->
 mapcat index_cols (index_lines s)
}

# TODO: add char index (including newlines)
