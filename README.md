# om-plugin-imports

This plugin behaves similarly to the native GHC options
`-ddump-minimal-imports`. If (and only if) you specify a `-dumpdir` directory
to GHC, this plugin will emit files to that directory that contain a
normalized, complete set of explicit imports for your module. This is sort of
what `-ddump-minimal-imports` does, but `-ddump-minimal-imports` has the
following deficiencies:

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

This plugin solves these problems. It will dump
a file in the user-supplied `-dumpdir` of the form
`<dumpdir>/<module-name>.full-imports`. The file will contain a set of
imports which can be copy/pasted over the imports in your module in a
way that satisfies `-Wmissing-import-lists`.

