# Regular Languages Toolbox

* `DFA`: 
  acceptance; 
  complement and intersection;
  language emptiness and equivalence checking;
  minimisation
* `NFA`: 
  acceptance; 
  union, concatenation, and star;
  powerset construction `-> DFA`;
  Kleene's algorithm `-> Regexp`;
* `Regexp`:
  reversal;
  Thompson's construction `-> NFA`;
  monadic parser `<- String`;
  simplification using axioms of Kleene algebra from Kozen's book (highly experimental)

Docs hosted at https://pyxidatol-c.github.io/Regular-Language/ (incomplete).

## Regexp equality testing
### Usage
```sh
ghc Main.hs -o eq-regexp
./eq-regexp
```
### Gallery
![Spec and Impl are equivalent](demo-equiv.png)

![Impl accepts empty string but Spec doesn't](demo-fail.png)
