# dt-haskell

Some experiments with dependent types / type-level programming in Haskell.

This includes trying to write in Haskell some of the [examples and exercices in the Idris book](https://github.com/pascalpoizat/idris-book).

## References

### Haskell Language Features

Not to be lost with extensions (`{-# LANGUAGE xxx #-}` or `-Xxxx`) : [GHC 8.4.3 language features](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html)

### Reading

- [Basic Type Level Programming in Haskell](http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html), by M. Parsons (2017)
- [Dependent Types in Haskell, Part I](https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell) by H. IShii (2014)
- [singletons 2.4 documentation on hackage](https://hackage.haskell.org/package/singletons) by R. Eisenberg and J. Stolarek (2018)
- [Adding dependent types to Haskell] (https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell), a page of the GHC trac with information and references.

### Videos

## Questions

- [x] *I typed this in to learn:*

	```haskell
	singletons
	   [d|
	
	  data Nat = Z
	           | S Nat
	               deriving (Show, Eq, Ord)
	
	  (+) :: Nat -> Nat -> Nat
	  Z + n = n
	  S m + n = S (m + n)
	
	  (*) :: Nat -> Nat -> Nat
	  Z * n = Z
	  S m * n = n * m + m
	
	  min :: Nat -> Nat -> Nat
	  min Z Z         = Z
	  min Z (S _)     = Z
	  min (S _) Z     = Z
	  min (S m) (S n) = S (min m n)
	  |]
	-- types: Z and S n
	-- values (singletons): SZ and SS n
	```

	*but are Nat, Z, S, +, \*, min etc already defined somewhere (possibly with useful properties / proofs)?*
  
  &rightarrow; yes, for example in `Data.Type.Natural` in package [`type-natural`](https://hackage.haskell.org/package/type-natural).
  
  &rightarrow; possibly also in the other packages listed below.

- [x] *we define Vect(or) purely for learning fun (you should certainly prefer using the packages given below), but are there already packages for such kinds of structures?*

  &rightarrow; yes, for example in package [`sized-vector`](https://hackage.haskell.org/package/sized-vector) (deprecated) and in package [`sized`](https://hackage.haskell.org/package/sized)(replacement)
  
  &rightarrow; but there are several others too, possibly not relying on the `singletons` package and possibly with less dependencies or Haskell extensions required, e.g., package [`vec`](https://hackage.haskell.org/package/vec) or package [`vector-sized`](https://hackage.haskell.org/package/vector-sized).
