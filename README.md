# dt-haskell<!-- omit in toc -->


Some experiments with dependent types in Haskell.

This includes trying to write in Haskell some of the [examples and exercices in the Idris book](https://github.com/pascalpoizat/idris-book).

**Remark:** this is a very ongoing work. The current entry point is [`Sandboxes.RAE1`](src/Sandboxes/RAE1).

## Contents

- [Contents](#contents)
- [Sandboxes](#sandboxes)
- [References](#references)
- [Notes on GHC Extensions](#notes-on-ghc-extensions)
- [Questions](#questions)

## Sandboxes

To make experiments we use different sandboxes based on different approaches.

- **HI** is H. Ishii line of work

	based on [R2]

- **RAEx** is R. Eisenberg line of work

	- **RAE1** based on [R3] &leftarrow; current work, see [`Sandboxes.RAE1`](src/Sandboxes/RAE1)
	- **RAE2** based on [V1]
	
## References

### Reading<!-- omit in toc -->


- [R1] [Basic Type Level Programming in Haskell](http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html), by M. Parsons (2017)
- [R2] [Dependent Types in Haskell, Part I](https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell) by H. Ishii (2014)
- [R3] [Dependent Types in Haskell: Theory and Practice](https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1074&context=compsci_pubs) by R. A. Eisenberg, PhD thesis, University of Pennsylvania (2016) 
- [R4] [Adding dependent types to Haskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell), a page of the GHC trac with information and references.

### Videos<!-- omit in toc -->


- [V1] [R. Eisenberg on Dependent Types](https://www.reddit.com/r/haskell/comments/8lkv6l/richard_eisenberg_speaks_on_dependent_types/) by R. Eisenberg (2018)

### Libraries<!-- omit in toc -->


- [L1] [`singletons 2.4.1`](https://hackage.haskell.org/package/singletons-2.4.1)
	by R. Eisenberg and J. Stolarek (2018-01-08)
- [L2] [`type-natural 0.8.2.0`](http://hackage.haskell.org/package/type-natural-0.8.2.0)
	by H. Ishii (2018-07-28)
- [L3] [`equational-reasoning-0.5.1.0`](http://hackage.haskell.org/package/equational-reasoning-0.5.1.0)
	by H. Ishii (2018-03-09)
- [L4] [`sized-vector-1.4.3.1`](https://hackage.haskell.org/package/sized-vector)
	by H. Ishii (2016-07-27)
- [L5] [`sized-0.3.0.0`](https://hackage.haskell.org/package/sized)
	by H. Ishii (2018-03-18)

## Notes on GHC Extensions

This information is mostly taken from [GHC 8.4.3 language features](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html) plus some things from other references of from experiments.

- **ConstrainedClassMethods**

	- more constraints on class methods

- **ConstraintKinds**

	TODO:

- **DataKinds**

	TODO:

- **ExistentialQuantification**

	TODO:

- **ExplicitForAll**
 
	- explicit universal quantification    
	- may bring type variables into scope
	- `-Wunused-foralls` to warn about unused variables

- **FlexibleInstances** (&Rightarrow; *TypeSynonymInstances*)

	- type class instances with arbitrary nested types in instance head

- **FunctionalDependencies**

	TODO:

- **GADTs** (&Rightarrow; *MonoLocalBinds*, *GADTSyntax*)

	- generalise ordinary algebraic data types
     	(constructors with richer return types)
	- refinement with pattern matching

- **GADTSyntax**

	- GADT syntax in data type definitions (not only *GADTs*)
	
		What makes a type a GADT is not the syntax
       but data constructors whose result type is not just `T a b`

	- generalise existential types
	- constructor with type-class context
		&rightarrow; context available by pattern matching
	- independent constructor type signatures
	- variables in first line
		- no scope
		- `*` or `Type` (with `Data.Kinds`) can be used
		- explicit kinds can be used

	       ```haskell
	       data Set a where ...
	       data Set :: * -> * where ...
	       data Set :: Type -> Type where ...
	       data Set a (b :: * -> *) where ...
	       ```

	- strictness annotations can be used
	- deriving clauses are possible for regular type (not for *GADTs*)
	- possible type class constraints for constructors (possibly all different)
	- ... and much more things

- **MonoLocalBinds**

	- related to type inference predictability
	- infer less polymorphic types for local bindings

- **MultiParamTypeClasses** (&Rightarrow; *ConstrainedClassMethods*)

	- type classes with multiple parameters

- **NoImplicitPrelude**

	TODO:

- **PolyKinds**

	TODO:

- **RankNTypes**

	TODO:

- **ScopedTypeVariables**

	TODO:

- **StandaloneDeriving**

	- stand-alone deriving declarations
	- can be in other modules
	- with *FlexibleInstances*, instance can be more specific than datatype
	- can be used to derive instances to exotic types (e.g., *GADTs*)
	- errors can come from generated code

- **TypeFamilies**

	TODO:

- **TypeInType**

	TODO:

- **TypeSynonymInstances**

	- type class instances for type synonyms

- **TypeOperators**

	TODO:

- **UndecidableInstances**
 
	- definition of instances which may lead to type-checker non-termination

## Questions

- *I typed this (from [R2]) in to learn:*

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
	```

	*but are Nat, Z, S, +, \*, min etc already defined somewhere (possibly with useful properties / proofs)?*
  
  &rightarrow; yes, for example in `Data.Type.Natural` in package [`type-natural`](https://hackage.haskell.org/package/type-natural).
  
- *we define Vec(t(or)) purely for learning fun (you should certainly prefer using the packages given below), but are there already packages for such kinds of structures?*

  &rightarrow; yes, for example in package [`sized-vector`](https://hackage.haskell.org/package/sized-vector) (deprecated) and in package [`sized`](https://hackage.haskell.org/package/sized)(replacement)
  
  &rightarrow; but there are several others too, possibly not relying on the `singletons` package and possibly with less dependencies or Haskell extensions required, e.g., package [`vec`](https://hackage.haskell.org/package/vec) or package [`vector-sized`](https://hackage.haskell.org/package/vector-sized).

- *there seem to have a difference in doing Vect a n vs Vect n a?*

	TODO:
