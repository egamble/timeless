# lex_split takes a set of separator characters and a string to split on those separators.
# The result is a sequence of strings and characters, which are the split pieces of the
# argument string, interleaved with the separator characters which were between the pieces.
# For example, if `S = {","0, ";"0} and `s = "foo, bar; baz", then
# lex_split S s = ["foo", ","0, " bar", ";"0, " baz"].

|`lex_split = SetOf Char => Str => SeqOf (Char ∪ Str) ∩
{
 _ -> "" -> [],
 `Separators -> `char:`rest_str
   | `rest_split = lex_split Separators rest_str
     | char ∈ Separators -> char:rest_split,
     | char ∉ Separators
       | rest_split = [] -> [char:""],
       | rest_split ≠ []
         # next_token will be either a separator char or a string
         | `next_token:`rest_rest_split = rest_split
           | next_token ∈ Separators -> (char:""):rest_split,
           | next_token ∈ Str -> (char:next_token):rest_rest_split,
}
