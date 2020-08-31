# The Ray Tracer Challenge With Haskell

Working through The Ray Tracer Challenge book to get a feel of Haskell.

## Chapters 1-4

### Changes

* Basic 3D space primitives (Point, Vector) and operations on them.
* 2D canvas with functions to set its pixels and output it as a valid PPM string.
* Matrices with main operations `multiply (|*|)` and `inverse`, and a bunch of others.
* 3D transformations using 4x4 matrices.
* A couple of usage examples: drawing projectile trajectory and drawing clock face.

### Thoughts

`Space` module feels clunky. I implemented `Point` and `Vector` as data types instead of proposed in the book tuples or lists. As a result, there are too many specialized functions which so far are not used anywhere outside the module's test suite and projectile example. Cannot tell how heavily this module will be used further down the line. It might make sense to implement the primitives as matrices constructed from corresponding functions. *(Edit: The module is already used heavily in the next chapter)*

`Canvas` `setPixel` performance is a bit of a concern. Setting a large number of pixels even on a moderately sized canvas one-by-one will be inefficient. In the projectile example I used a `HashMap` of pixels and `map` to set pixels in one pass. In the future it will probably be better to just construct the canvas from a pre-built set of pixels.

`Matrix` module looks okay-ish. Glanced into the monads world when decided that `multiply` and `inverse` operations must return `Either`. As a result, testing got more interesting as I had to use `do` notation or `>>=` when applying transformations. I do not think what I am doing with them currently is idiomatic, will try to improve it along the way.

Nothing special in the `Transform` module although chaining transformations which can fail was interesting. Same as the above I will most likely revisit it later.

### Image Samples

[Using canvas PPM output (converted to PNG) with points and vectors.](/samples/projectile.png)

[Using transformations to place points at clock face hours positions.](/samples/clock.png)

## Chapters 5

### Changes

* Added types for Sphere, Ray and Intersection.
* Operations to find an intersection and a hit (first intersection) of a ray with a sphere.

### Thoughts

Among the new modules `Geometry`, `Geometry.Sphere` and `Ray` the first one turned out to be the trickiest. I wanted to make `Intersection a` and instance of `Ord` typeclass and for that I had to add `Eq` constraint to the `Intersection`'s type parameter. Apparently, you cannot do this in a simple data type declaration. The solution that I found [here](https://wiki.haskell.org/Data_declaration_with_constraint) suggests using GADTs for this and so I did.

When I got to implementing this chapter's practical example, I decided that I want to refactor matrix transformations to avoid working with `Either`. An experienced Haskell developer showed me an example of using `TypeLits` (`TypeNats` for my case) which I could use to check matrices' sizes at compile time instead of runtime. That was the most difficult part.

I am pretty far from understanding what exactly is happening there but hey, it works! Initially, I have spent quite some time trying to magically pass data constructor parameters to type parameters and wondered why I couldn't make it work. Now, my understanding is that a big chunk of the type-safety with `TypeLits` and the related features is achieved with good reasoning and encapsulation. That is, you "inform" compiler what type you are returning from which function (`TypeOperators` are really cool though). You also have to hide data constructors and get you type's instances from functions with fully described types. If you get any of it wrong, the compiler cannot help you if some arbitrary type instances start flying around.

For the example, instead of doing it the way suggested by the hints I implemented a kind of a brute-force approach. First, applied scale and translation transformations to make the sphere a proper size and put it in the centre of the canvas. Then, cast a ray from each pixel's `x` and `y` coordinates and an appropriate `z` coordinate. The resulting hits draw a silhouette of the given sphere. 

### Image Samples

[Using ray intersections to draw a silhouette of a sphere on the canvas.](/samples/silhouette.png)