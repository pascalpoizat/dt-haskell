# dt-haskell<!-- omit in toc -->


Some experiments with dependent types in Haskell.

This includes trying to write in Haskell some of the [examples and exercices in the Idris book](https://github.com/pascalpoizat/idris-book).

**Remark:** this is a very ongoing work. The current entry point is [`Sandboxes.RAE2`](src/Sandboxes/RAE2.hs).

## Contents

- [Contents](#contents)
- [Sandboxes](#sandboxes)
- [References](#references)
- [Notes on GHC Extensions](#notes-on-ghc-extensions)
- [Questions](#questions)

## Sandboxes

To make experiments we use different sandboxes based on different approaches.

- **HI** is H. Ishii line of work

	based on [R-S2]

- **RAEx** is R. Eisenberg line of work

	- **RAE1** based on [R-A1] (inconclusive)
	- **RAE2** based on [V1] &leftarrow; current work, see [`Sandboxes.RAE2`](src/Sandboxes/RAE2.hs)
	
## References

### Reading<!-- omit in toc -->

#### Starter<!-- omit in toc -->

- [R-S1] [Basic Type Level Programming in Haskell](http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html), by M. Parsons (2017)
- [R-S2] [Dependent Types in Haskell, Part I](https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell) by H. Ishii (2014)

#### Advanced<!-- omit in toc -->

- [R-A1] [Dependent Types in Haskell: Theory and Practice](https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1074&context=compsci_pubs) by R. A. Eisenberg, PhD thesis, University of Pennsylvania (2016) 

#### Other<!-- omit in toc -->

- [R-O1] [Adding dependent types to Haskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell), a page of the GHC trac with information and references.

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

	- types of kinds `Constraint` can be used in contexts
	- `Constraint` can be imported from `GHC.Exts` or `Data.Kind`
	- allows type constraint synonyms
	- in case of exotic constraints, use with `UndecidableInstances`

- **DataKinds**

	- promotion of data types at the kind level
	- useful for advanced type system features such as `TypeFamilies` and `GADTs`
	- example, the following datatype:
		```haskell
		data Nat = Zero | Succ Nat
		```
		will yield a new `Nat` kind and two new type constructors, `'Zero` and `'Succ`:
		```haskell
		data Nat = Zero | Succ Nat
		Nat :: *
		'Zero :: Nat
		'Succ :: Nat -> Nat
		```
	- there are restrictions on things being promoted (see doc) but `TypeInType` relaxes some of these restrictions
	- quote marks can be removed when non ambiguous (not recommended)
	- `-Wunticked-promoted-constructors` to signal forgotten quote marks

- **ExplicitForAll**
 
	- explicit universal quantification    
	- may bring type variables into scope
	- `-Wunused-foralls` to warn about unused variables

- **ExplicitNamespaces**

	- explicit namespaces in module import/export lists
		```haskell
		import M ((+))                -- function
		import M (type (+))           -- type constructor
		module M ((+)) where ...      -- function
		module M (type (+)) where ... -- type
		```

- **FlexibleInstances** (&Rightarrow; *TypeSynonymInstances*)

	- type class instances with arbitrary nested types in instance head

- **GADTs** (&Rightarrow; *GADTSyntax*, *MonoLocalBinds*)

	- generalise ordinary algebraic data types
     	(constructors with richer return types)
	- pattern matching cause type refinement
	- refinement done based on user-supplied type annotations
	- `DataKinds` is useful with `GADTs`

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

- **KindSignatures**

	- explicit kind signatures on type variables
	- use spacing to avoid parse errors (e.g., not `::*->*` but `:: * -> *`)

- **MonoLocalBinds**

	- related to type inference predictability
	- infer less polymorphic types for local bindings

- **MultiParamTypeClasses** (&Rightarrow; *ConstrainedClassMethods*)

	- type classes with multiple parameters

- **NoImplicitPrelude**

	- no `Prelude` import by default
	- enables to use personal preludes (that should not be named `Prelude`)

- **PolyKinds** (&Rightarrow; *KindSignatures*)

	- kind polymorphic types
	- infer most general kinds for declarations
	- precision using user-given kind signatures (**KindSignatures**), typically interesting for families (**TypeFamilies**)
		```haskell
		type family F1 a                -- F1 :: * -> *
		type family F2 (a :: k)         -- F2 :: forall k. k -> *
		type family F3 a :: k           -- F3 :: forall k. * -> k
		type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2
		-- from https://downloads.haskell.org/%7Eghc/8.4.3/docs/html/users_guide/glasgow_exts.html
		```
	- use `-fprint-explicit-kinds` (and possibly other pretty-printing options) for clearer error outputs
	- `TypeInType` can be seen as an extension of `PolyKinds`
		(indeed `TypeInType` &Rightarrow; `PolyKinds`),
  	both co-exist but in case `TypeInType` is used
		consider using `-dcore-lint` too.

- **RankNTypes** (&Rightarrow; *ExplicitForall*)

	- higher rank types
	- `forall`s can be nested arbitrarily deep in function arrows
	- type signatures (but not only) can help in making type inference possible for arbitrary-rank types

- **ScopedTypeVariables** (&Rightarrow; *ExplicitForAll*)

	- lexical scoping of type variables explicitely introduced with `forall`

- **StandaloneDeriving**

	- stand-alone deriving declarations
	- can be in other modules
	- with *FlexibleInstances*, instance can be more specific than datatype
	- can be used to derive instances to exotic types (e.g., *GADTs*)
	- errors can come from generated code

- **TypeFamilies** (&Rightarrow; *ExplicitNamespaces*, *KindSignatures*, *MonoLocalBinds*)

	- data families and indexed types
	- facilitate type-level programming
	- alternative to functional dependencies (more functional style vs more relational style)
	- type families are type constructors that represent sets of types
	- two sorts: data families and synonym families, that are the indexed variant of algebraic data types and type synonyms.
	- data families can appear at top level (more general) or in type classes (better structuring, warnings if missing instances)
	- synonym families can appear at the top level (more general) as open families or closed families (having a `where` clause), or in type classes called associated type synonyms (better structuring, warnings if missing instances)
	- type families are concerned by compatibility rules
	- data instances are instances of data families
	- type instances are instances of synonym families
	- instances can have `deriving` clauses
	- `-Wunused-type-patterns` to signal variables in left hand side patterns not used in right hand side
	- `DataKinds` and `UndecidableInstances` are useful with `TypeFamilies`
	- a lot more things ...

- **TypeInType** (&Rightarrow; *DataKinds*, *KindSignatures*, *PolyKinds*)

	- see `PolyKinds`
	- `TypeInType` can be seen as an extension of `PolyKinds`
		(indeed `TypeInType` &Rightarrow; `PolyKinds`),
  	both co-exist but in case `TypeInType` is used
		consider using `-dcore-lint` too.
	- kinds can be as intricate as types: explicit quantification over kind variables, higher-rank kinds, type synonyms and families in kinds, among other features

- **TypeSynonymInstances**

	- type class instances for type synonyms

- **TypeOperators** (&Rightarrow; *ExplicitNamespaces*)

	- definition and use of types with operator names
	- possibly ambiguity in module import/export lists resolved using *ExplicitNamespaces* (see the corresponding entry)

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
