# NumVal — Implicitly abstract numerics #

The `NumVal` Scala trait enables developers to write a math expression once, then implicitly apply it to any of the 10 built-in number types.  Like `AnyVal`, NumVal acts as a common super-type of the primitive types: `Boolean`, `Byte`, `Char`, `Short`, `Int`, `Long`, `Float` and `Double`, but also includes Scala's `BigInt` and `BigDecimal` types.  Unlike `AnyVal`, `NumVal` supplies common arithmetic operations and most of the operations found in `scala.math`.  Besides applying to any number type, `NumVal` operations also fix a number of anomalous behaviors in the standard math library, described in the documentation.

NumVal was developed to satisfy these goals:

- Transparent (implicit) abstraction across all Scala number types
- Arbitrary precision, using BigInt and BigDecimal types
- Accuracy to the last (rounded) digit
- Mathematical correctness for all input values
- Performance

Trait NumVal was initially developed to support a Java-based graphics animation library, Formulo, that is not yet released.  I decided to publish NumVal as a separate project, since generalized numerics has broader utility than merely for graphical applications.

## Installation ##
NumVal is hosted on [Maven Central](http://central.maven.org/maven2/org/oxland/).
You can add it as a dependency to your `build.sbt` file. It is built for Scala 2.9.0, 2.9.1, 2.9.2, 2.9.3-RC, and dash-numbered variants.

```scala
libraryDependencies += "org.oxland" %% "numval" % "0.1.0"
```


## Usage ##

Include one or both of these imports in your source code:

- `import org.oxland.math._      // to explicitly define NumVal variables`
- `import org.oxland.math.NumVal._  // to implicitly convert among number types`

The first import, `~.math._`, lets you explicitly reference the `NumVal` trait, for example:

- `import org.oxland.math._`
- `def add(a:NumVal, b:NumVal):NumVal = a + b`

The second import, `~.NumVal._`, enables implicit conversions from any of the 10 Scala number types to `NumVal`, as needed to invoke a `NumVal` operation.

For example:

- `import org.oxland.math.NumVal._`
- `true * math.Pi    // since the Boolean type "true" has no '*' operator, it is implicitly wrapped as a NumVal`

In Scala you can restrict the scope of either import by placing it in the curly brackets of a block.
Either import can be used independently of the other.

## Documentation ##
- See the [http://kabob.github.com/NumVal](API documentation) (Scaladocs).
- The [project wiki](https://github.com/kabob/NumVal/wiki/_pages) contains a full user guide, development notes, performance guide, and other resources.


## Build yourself ##

```sh
$ cd NumVal
$ ./sbt
> compile
```

## Contact ##
Robert Kohlenberger (kohlenrw@bdumail.com)


