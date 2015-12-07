# A recognizer for strings of 1s whose length is composite (i.e. not prime).

#name Repeat+

Repeat+ = {s ↦ {s, s ++ (∈ Repeat+ s)}}

Factor = {'1' ++ (∈ Repeat+ '1')}

Composite = {'', '1', (s ∈ Factor) ++ (∈ Repeat+ s)}

stdout =
{| stdin ∈ Composite ↦ "composite"
 |                   ↦ "prime"
}
