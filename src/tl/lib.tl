| Bool = {true, false}

| Nat    = {n∈Int | n>=0}
| PosInt = {n∈Int | n>0}
| NegInt = {n∈Int | n<0}

| SetOf = {S∈Set -> {⊂ S}}

| SeqOf = {S∈Set -> {s ∈ Seq | Im s ⊂ S}}

#opl 6 ∧ &&
#opl 5 ∨ ||

| (∧) = (&&) =
{true  -> true  -> true,
 true  -> false -> false,
 false -> true  -> false,
 false -> false -> false,
 false -> null  -> false,
 null  -> false -> false}

| (∨) = (||) =
{true  -> true  -> true,
 true  -> false -> true,
 false -> true  -> true,
 false -> false -> false,
 true  -> null  -> true,
 null  -> true  -> true}

#opr 4 --> ⟶

| (⟶) = (-->) =
{true  -> true  -> true,
 true  -> false -> false,
 false -> true  -> true,
 false -> false -> true,
 false -> null  -> true,
 null  -> true  -> true}

#op 3 ?

| (?) =
{true -> then -> else -> then,
 true -> then -> null -> then,
 false -> then -> else -> else,
 false -> null -> else -> else}

#opl 13 => ⇒

| (⇒) = (=>) =
{S∈Set -> T∈Set -> {∈S -> ∈T}}

#opl 14 \

| (\) = Set=>Set=>Set ∩
{A -> B -> {x∈A | x∉B}}

#opr 19 ∘ .

| (∘) = (.) = {f -> g -> x -> f (g x)}

#opr 2 $

| ($) = {f -> x -> f x}

#name ¬ !
| ! = ¬ = {true -> false, false -> true}

| if = (?)

| id = {x -> x}

| const = {x -> {_ -> x}}

| inc = {x -> x+1}
| dec = {x -> x-1}

| flip = {f -> a -> b -> f b a}

| first = {x:_ -> x}

| rest = {_:xs -> xs}

| second = {_:x:_ -> x}

| take = Nat=>Seq=>Seq ∩
{0 -> _    -> [],
 n -> x:xs -> x:take(dec n)xs}

| map = Fn=>Seq=>Seq ∩
{_ -> []   -> [],
 f -> x:xs -> f x : map f xs}

| filter = Set=>Seq=>Seq ∩
{_ -> []   -> [],
 S -> x:xs -> x∈S?x:r:?r | r=filter S xs}

| remove = Set=>Seq=>Seq ∩
{_ -> []   -> [],
 S -> x:xs -> x∈S?r:?x:r | r=remove S xs}

| reduce = foldl = SetOf(Any=>Any=>Any)=>Any=>Seq=>Any ∩
{_ -> z -> []   -> z,
 f -> z -> x:xs -> foldl f (f z x) xs}

| foldr = SetOf(Any=>Any=>Any)=>Any=>Seq=>Any ∩
{_ -> z -> []   -> z,
 f -> z -> x:xs -> f x (foldr f z xs)}

| mapcat = SetOf(Any=>Seq)=>Seq=>Seq ∩
{f -> s -> reduce(++) [] (map f s)}

| zip =
{a∈Seq -> b∈Seq
  -> [] | a=[] ∨ b=[],
  -> (x,y): zip xs ys
  | a=x:xs
    b=y:ys}

| toFn =
{x -> toArr x}

| toArr =
{a∈Arr -> a,
 _;x   -> toArr x,
 S∈Set -> toArr x | x ∈ S
}

| tagType = {t;_ -> t} 

| tagValue = {_;v -> v}
