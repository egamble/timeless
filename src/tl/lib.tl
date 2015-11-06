# Library operators

#infixa 10 ∘

(∘) = {f ↦ {g ↦ {x ↦ f (g x)}}

#infix 8 ..

(..) =
{a ∈ Int ↦
 {a ↦ [a],
  b ∈ Int | a < b ⟼ a : (inc a..b)},

 c ∈ Char ↦
 {c ↦ [c],
  d ∈ Char | (a = charInt c) ∧ (b = charInt d) ∧ a < b ⟼
   c : (intChar(inc a)..d)}}

# Library prefix functions

id = {x ↦ x}

const = {x ↦ {y ↦ x}}

inc = {x ↦ x+1}
dec = {x ↦ x-1}

if =
{cond ↦
 {then ↦
  {else | cond ↦ then
        |      ↦ else}}}

map =
{f ↦
 {[] ↦ [],
  x:xs  ↦ f x : map f xs}}

filter =
{S ↦
 {[] ↦ [],
  x:xs | r = filter S xs
   (| x ∈ S ↦ x:r
    |       ↦ r)
 }
}

remove =
{S ↦
 {[] ↦ [],
  x:xs | r = remove S xs
   (| x ∈ S ↦ r
    |       ↦ x:r)
 }
}

zip =
{[] ↦ {∈ Seq ↦ []},
 x1:xs1 ↦ {[] ↦ [],
            x2:xs2 ↦ (x1,x2) : zip xs1 xs2}}
