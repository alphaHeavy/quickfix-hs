## 1.1.1

- Drop support for GHC prior GHC-8.6.5

## 1.1

- Depend on `bifunctor-classes-compat` only.
  Instances for types defined in `bifunctors` package are moved there.
  With this change `assoc` only depends on `base` and `tagged`.
- Mark modules as explicitly Safe

## 1.0.2

- Add 'Swap' instances for more n-tuples

## 1.0.1

- Add `Assoc Const` and `Tagged` instances
