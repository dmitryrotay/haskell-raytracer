# The Ray Tracer Challenge With Haskell

Writing the first Haskell application by walking through The Ray Tracer Challenge book.

## Chapters 1-4

### Changes

* Basic 3D space primitives (Point, Vector) and operations on them.
* 2D canvas with functions to set its pixels and output it as a valid PPM string.
* Matrices with main operations `multiply (|*|)` and `inverse`, and a bunch of others.
* 3D transformations using 4x4 matrices.
* A couple of usage examples: drawing projectile trajectory and drawing clock face.

### Thoughts

`Space` module feels clunky. I implemented `Point` and `Vector` as data types instead of proposed in the book tuples or lists. As a result, there are too many specialized functions which so far are not used anywhere outside the module's test suite and projectile example. Cannot tell how heavily this module will be used further down the line. It might make sense to implement the primitives as matrices constructed from corresponding functions.

*([Edit]: The `Space` module is already used heavily in the next chapter)*

`Canvas` `setPixel` performance is a bit of a concern. Setting a large number of pixels even on a moderately sized canvas one-by-one will be inefficient. In the projectile example I used a `HashMap` of pixels and `map` to set pixels in one pass. In the future it will probably be better to just construct the canvas from a pre-built set of pixels.

`Matrix` module looks okay-ish. Glanced into the monads world when decided that `multiply` and `inverse` operations must return `Either`. As a result, testing got more interesting as I had to use `do` notation or `>>=` when applying transformations. I do not think what I am doing with them currently is idiomatic, will try to improve it along the way.

Nothing special in the `Transform` module although chaining transformations which can fail was interesting. Same as the above, I will most likely revisit it later.

### Image Samples

[Using canvas PPM output (converted to PNG) with points and vectors.](/samples/projectile.png)

[Using transformations to place points at clock face hours positions.](/samples/clock.png)

## Chapters 5

### Changes

* Data types for Sphere, Ray and Intersection.
* Operations to find an intersection and a hit (first intersection) of a ray with a sphere.

### Thoughts

Among the new modules `Geometry`, `Geometry.Sphere` and `Ray` the first one turned out to be the trickiest. I wanted to make `Intersection a` and instance of `Ord` typeclass and for that I had to add `Eq` constraint to the `Intersection`'s type parameter. Apparently, you cannot do this in a simple data type declaration. The solution that I found [here](https://wiki.haskell.org/Data_declaration_with_constraint) suggests using GADTs for this and so I did.

*[Edit] As it was pointed out to me, I don't need the type parameter constraint here. Moreover, it is generally a bad idea to add such constraints, according to [this explanation](https://www.google.com/url?q=https://stackoverflow.com/a/12770425/394253&sa=D&source=hangouts&ust=1598983884505000&usg=AFQjCNHjctWqcZl4E-UgSEJlA9lu-ETcLQ). In addition to that, I only needed these constraints so that I could use `minimum` instead `minimumBy`. Currently, it makes more sense to use `minimumBy` instead of implementing `Ord`. I don't really have a requirement to always sort intersections by distance while completely ignoring the object. It might come later though.*

When I got to implementing this chapter's practical example, I decided that I want to refactor matrix transformations to avoid working with `Either`. An experienced Haskell developer showed me an example of using `TypeLits` (`TypeNats` for my case) which I could use to check matrices' sizes at compile time instead of runtime. That was the most difficult part.

I am pretty far from understanding what exactly is happening there but hey, it works! Initially, I have spent quite some time trying to magically pass data constructor parameters to type parameters and wondered why I couldn't make it work. Now, my understanding is that a big chunk of the type-safety with `TypeLits` and the related features is achieved with good reasoning and encapsulation. That is, you "inform" compiler what type you are returning from which function (`TypeOperators` are really cool though). You also have to hide data constructors and get you type's instances from functions with fully described types. If you get any of it wrong, the compiler cannot help you if some arbitrary type instances start flying around.

For the sample, instead of doing it the way suggested by the hints I used a kind of a brute-force approach. First, applied scale and translation transformations to make the sphere a proper size and put it in the centre of the canvas. Then, cast a ray perpendicular to the canvas from each pixel's `x` and `y` coordinates and a suitable `z` coordinate. The resulting hits draw a silhouette of the given sphere. 

### Image Samples

[Using ray intersections to draw a silhouette of a sphere on the canvas.](/samples/silhouette.png)

## Chapters 5

### Changes

* Sphere normals calculation.
* Data types for material and point light.
* Perceived sphere points color computation using material and lighting.

### Thoughts

Overall, nothing too complex in terms of programming this chapter, although the result feels really satisfying. The biggest change was removing more of the `Either`'s from the code. My reasoning is that currently I don't have situations when I really need my code to fail gracefully and follow some other path. If something goes wrong, e.g. trying to invert an uninvertible matrix or create a point not from 4x1 matrix, I just want an exception. I might revisit this later when I have a full picture of the code flow and learn to write proper monadic functions.

Following an advice, added `-Wall` option for the compiler. This lead to improving code by cleaning up quite a few places which compiler marked with warnings.

A `let`-ladder in the `Materials.lighting` function is a little concerning. I don't have a good idea on how to improve it yet.

As expected, I had to rewrite the previous chapter sample's code to properly cast rays from a single view point. While doing it the old way (by casting rays perpendicular to the canvas) produced a fair-looking 3D image, it had some lighting artifacts on the sphere.

### Image Samples

[Using material and lighting to create a first real 3D object.](/samples/sphere-with-lighting.png)