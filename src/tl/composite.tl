# A parser that recognizes composite-length strings

# This works on strings whose length is composite (i.e. not prime) where all the characters are the same
# and, more generally, on strings that are the concatenation of a composite number of identical strings.

Repeat* = {s  ↦ {'',  Repeat+ s}}
Repeat+ = {s  ↦ {s ++ Repeat* s}}

Composite1 = {s ++ (∈ Repeat+ s) | len s > 1}

# Equivalently:

Composite2 = {s ++ s ++ (∈ Repeat* s) | len s > 1}

stdout =
{∈ Composite1  ↦ "composite",
 _             ↦ "not composite"
}
stdin
