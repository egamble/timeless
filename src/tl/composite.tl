# A recognizer for strings of 1s whose length is composite (i.e. not prime).

#name Repeat+
Repeat+ = {s -> {s,
                 s++t | t ∈ Repeat+ s}}

Factor = {'1'++s | s ∈ Repeat+ '1'}

Composite = {'', '1',
             s++t | s ∈ Factor
                    t ∈ Repeat+ s}

stdout =
{∈ Composite -> 'composite'
 ∉ Composite -> 'prime'}
