# om-plugin-imports

This plugin behaves similarly to the native GHC options
`-ddump-minimal-imports`.  It will dump a file of the form
`<src-file>.full-imports`. E.g. if the Haskell file was `src/Foo/Bar.hs`,
then the dump file will be `src/Foo/Bar.hs.full-imports`.  The file will
contain a set of imports which can be copy/pasted over the imports in
your module in a way that satisfies `-Wmissing-import-lists`.

## Motivation

This is _almost_ what `-ddump-minimal-imports` does, but
`-ddump-minimal-imports` has the following deficiencies:

* It will not always produce output that satisfies `-Wmissing-import-lists`.
  E.g. it will sometimes produce something like:

  ```
  import Foo (Bar(..))
  ```
* It will explicitly import all the names from modules which are already
  qualified, E.g:
  ```
  import qualified Foo as F (foo, bar, baz)
  ```

## Options

The plugin supports the following option:

* `excessive`: Output the import list even on unambiguous qualified imports.


