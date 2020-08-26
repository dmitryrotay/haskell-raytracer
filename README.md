# The Ray Tracer Challenge With Haskell

Working through The Ray Tracer Challenge book to get a feel of Haskell.

## Chapters 1-4

### Changes:

* Basic 3D space primitives (Point, Vector) and operations on them.
* 2D canvas with functions to set its pixels and output it as a valid PPM string.
* Matrices with main operations `multiply (|*|)` and `inverse`, and a bunch of others.
* 3D transformations using 4x4 matrices.
* A couple of usage examples: drawing projectile trajectory and drawing clock face.

### Thoughts

`Space` module feels clunky. I implemented `Point` and `Vector` as data types instead of proposed in the book tuples or lists. As a result, there are too many specialized functions which so far are not used anywhere outside the module's test suite. Cannot tell how heavily this module will be used further down the line. It might make sense to implement the primitives as matrices constructed from corresponding functions.

`Canvas` `setPixel` performance is a bit of a concern. Setting a large number of pixels even on a moderately sized canvas one-by-one will be inefficient. In the projectile example I used a `HashMap` of pixels and `map` to set pixels in one pass. In the future it will probably be better to just construct the canvas from a pre-built set of pixels.

`Matrix` module looks okay-ish. Glanced into the monads world when decided that `multiply` and `inverse` operations must return `Either`. As a result, testing got more interesting as I had to use `do` notation or `>>=` when applying transformations. I do not think what I am doing with them currently is idiomatic, will try to improve it along the way.

Nothing special in the `Transform` module although chaining transformations which can fail was interesting. Same as the above I will most likely revisit it later.
