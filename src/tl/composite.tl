# A recognizer for strings of ones whose length is composite (i.e. not prime).

#include src/tl/lib


#name Repeat+
|`Repeat+ =
{`s ∈ Str -> {s, s ++ `t | t ∈ Repeat+ s}}

| `Factor =
{"1" ++ `s | s ∈ Repeat+ "1"}

|`Composite =
{"",
 "1",
 `s ++ `t | s ∈ Factor ∧ t ∈ Repeat+ s
}

|`out =
{∈ Composite -> "composite",
 ∉ Composite -> "prime"
}
