# A parser that recognizes composite-length strings

# This works on strings whose length is composite (i.e. not prime) where all the characters are the same
# and, more generally, on strings that are the concatenation of a composite number of identical strings.

repeat* = {s  ↦ {'',  repeat+ s}}
repeat+ = {s  ↦ {s ++ repeat* s}}

Composite = {s ++ (∈ repeat+ s) | len s > 1}

# Equivalently:

# Composite = {s ++ s ++ (∈ repeat* s) | len s > 1}

stdout =
{∈ Composite  ↦ "composite",
 _            ↦ "not composite"
}
stdin
