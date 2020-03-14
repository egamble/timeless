| Bool = {true, false}

| Nat    = {n∈Int | n>=0}
| PosInt = {n∈Int | n>0}
| NegInt = {n∈Int | n<0}

| SetOf = {S∈Set -> {⊂ S}}

| SeqOf = {S∈Set -> {s ∈ Seq | Im s ⊂ S}}

#pr> = ∧ ∨ --> ->
#pr= ∧ &&
#pr= ∨ ||

#opa ∧ ∨ && ||

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

#opr ⟶ -->
#pr= ⟶ -->

| (⟶) = (-->) =
{true  -> true  -> true,
 true  -> false -> false,
 false -> true  -> true,
 false -> false -> true,
 false -> null  -> true,
 null  -> true  -> true}

#pr> -> ? $ |

#op ?
| (?) =
{true -> then -> else -> then,
 true -> then -> null -> then,
 false -> then -> else -> else,
 false -> null -> else -> else}

#pr> ∘ \ => ∩
#pr= ⇒ =>
#opa ⇒ =>

| (⇒) = (=>) =
{S∈Set -> T∈Set -> {∈S -> ∈T}}

#opl \

| (\) = Set=>Set=>Set ∩
{A -> B -> {x∈A | x∉B}}

#opr ∘ .
#pr= ∘ .

| (∘) = (.) = {f -> g -> x -> f (g x)}

#opr $

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

| zip =
{a∈Seq -> b∈Seq
  -> [] | a=[] ∨ b=[],
  -> (x,y): zip xs ys
  | a=x:xs
    b=y:ys}
