Timeless
========

Timeless is a side effect-free, functional, strongly-typed language. Syntax and semantics are inspired by naïve set theory, and by other functional languages including Haskell and Clojure.

Timeless is designed to have very simple and very clean syntax and semantics. One of the guiding principles of Timeless is that the compiler will gradually become more sophisticated over time, while maintaining backward compatibility. This doesn't necessarily mean the language specification will become more complicated, just that the compiler will understand more complicated expressions as the compiler improves.

For example, a fixed property of Timeless is that the compiler must be able to prove that all expressions are well-formed, i.e. in each function application, the range of the argument must be a subset of the domain of the applied function. This should prevent runtime exceptions except for resource depletion. Some well-formed expressions may be rejected by the compiler initially, but eventually accepted as the compiler improves.

The detailed description of Timeless is at https://github.com/egamble/timeless/wiki

Copyright © 2013 Evan Gamble. Distributed under the Eclipse Public License. See the file COPYING.
