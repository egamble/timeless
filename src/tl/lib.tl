# Library operators

#infixa 10 ∘
(∘) = {f -> g -> x -> f (g x)}

#infixr 0 $
($) = {f -> x -> f x}

#infix 8 ..
(..) =
  {m ∈ Int ->
    {m -> [m],
     n ∈ Int
       | m<n
       -> m:(inc m..n)},

   c ∈ Char ->
    {c -> [c],
     d ∈ Char
       | m=charInt(c) ∧
         n=charInt(d) ∧
         m<n
       -> c:(intChar(inc m)..d)}}

#infixl 4 \
(\) =
  {A   -> B   -> {x ∈ A | x ∉ B}}
:∈ Set => Set => Set

#infix 3 ⊃
(⊃) = flip (⊂)

# Library prefix functions

id = {x -> x}

const = {x -> y -> x}

inc = {x -> x+1}
dec = {x -> x-1}

if =
  {cond -> then -> else | cond -> then,
                               -> else}
¬ =
  {true  -> false,
   false -> true}

implies = {p -> q -> ¬p ∨ q}

flip = {f -> a -> b -> f b a}

first = {x:_ -> x}

rest = {_:xs -> xs}

second = {_:x:_ -> x}

take =
  {0   -> _    -> [],
   n   -> x:xs -> x : take (dec n) xs}
:∈ Nat => Seq  => Seq

map =
  {_  -> []   -> [],
   f  -> x:xs -> f x : map f xs}
:∈ Fn => Seq  => Seq

filter =
  {_   -> []                     -> [],
   S   -> x:xs | r = filter S xs -> {| x ∈ S -> x:r,
                                             -> r}}
:∈ Set => Seq                    => Seq

remove =
  {_   -> []                     -> [],
   S   -> x:xs | r = remove S xs -> {| x ∈ S -> r,
                                             -> x:r}}
:∈ Set => Seq                    => Seq

foldl =
  {_         -> z  -> []   -> z,
   f         -> z  -> x:xs -> foldl f (f z x) xs}
:∈ (U~>U~>U) => U  => Seq  => Seq

reduce = foldl

foldr =
  {_         -> z -> []   -> z,
   f         -> z -> x:xs -> f x (foldr f z xs)}
:∈ (U~>U~>U) => U => Seq  => Seq

mapcat =
  {f        -> s   -> reduce (++) [] (map f s)}
:∈ (U~>Seq) => Seq => Seq

zip =
  {[]    -> ∈ Seq -> [],
   ∈ Seq -> []    -> [],
   x:xs  -> y:ys  -> (x,y) : zip xs ys}
