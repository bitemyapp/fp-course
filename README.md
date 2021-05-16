# Functional Programming Course

![Haskell logo](https://haskell-lang.org/static/img/logo.png?etag=rJR84DMh)

### Written by Tony Morris & Mark Hibberd for Data61

### With contributions from individuals (thanks!)

This version is maintained by Chris Allen at [https://github.com/bitemyapp/fp-course](https://github.com/bitemyapp/fp-course)

#### Introduction

The course is structured according to a linear progression and uses the Haskell
programming language to learn programming concepts pertaining to functional
programming.

Exercises are annotated with a comment containing the word "Exercise." The existing code compiles, however answers have been replaced with a call to the Haskell `error` function and so the code will throw an exception if it is run. Some exercises contain tips, which are annotated with a preceding "Tip:". It is not necessary to adhere to tips. Tips are provided for potential guidance, which may be discarded if you prefer a different path to a solution.

The exercises are designed in a way that requires personal guidance, so if you attempt it on your own and feel a little lost, this is normal. All the instructions are not contained herein.

#### Differences from the Original

- We officially support the `stack` build tool.
- The test suite is converted from `tasty` to `Hspec`.

The original course is located [here](https://github.com/data61/fp-course/).

### Getting Help

There are two mailing lists for asking questions. All questions are welcome,
however, your first post might be moderated. This is simply to prevent spam.

1. \#haskell-beginners [on Freenode](irc://irc.freenode.net/#haskell-beginners) is an IRC channel that is operated by Chris Allen. The participants are volunteers and fellow learners who help others on their own time.

2. \#haskell or \#haskell-beginners [on the FPChat Slack](https://fpchat-invite.herokuapp.com/). FPChat is a Slack community of fellow travelers. The \#haskell and \#haskell-beginners channels are good places to seek assistance with this course.

### Getting Started

1. [Install Stack](https://haskellstack.org/) which will manage your compiler versions automatically and enable you to build this project.

2. Change to the directory containing this document.

3. Execute the command `make ghci`, which will compile and load all the source code.
   You may need to set permissions on the root directory and the ghci configuration
   file, `chmod go-w .ghci ./`.

4. Inspect the introductory modules to get a feel for Haskell's syntax, then move
   on to the exercises starting with `Course.Optional`. The
   [Progression](#progression) section of this document lists the recommended
   order in which to attempt the exercises.

5. Edit a source file to a proposed solution to an exercise. At the `ghci`
   prompt, issue the command `:reload`. This will compile your solution and
   reload it in the GHC interpreter. You may use `:r` for short.

### Tips after having started

1. Some questions take a particular form. These are called *WTF questions*. WTF
   questions are those of this form or similar:
  * What does ____ mean?
  * What does the ____ function mean?
  * What is a ____ ?
  * Where did ____ come from ?
  * What is the structure of ____ ?

  They are all answerable with the `:info` command. For example, suppose you
  have the question, "What does the `swiggletwoop` function mean?" You may
  answer this at GHCi with:

  `> :info swiggletwoop`

  You may also use `:i` for short.

2. Functional Programming techniques rely heavily on types. This reliance may
   feel foreign at first, however, it is an important part of this course. If
   you wish to know the type of an expression or value, use `:type`. For
   example,

   `> :type reverse`

   `List t -> List t`

   This tells you that the `reverse` function takes a list of elements of some
   arbitrary type (`t`) and returns a list of elements of that same type. Try
   it.

   You may also use `:t` for short.

3. GHCi has TAB-completion. For example you might type the following:

   `> :type rev`

   Now hit the TAB key. If there is only one function in scope that begins with
   the characters `rev`, then that name will auto-complete. Try it. This
   completion is context-sensitive. For example, it doesn't make sense to ask
   for the type of a data type itself, so data type names will not auto-complete
   in that context, however, if you ask for `:info`, then they are included in
   that context. Be aware of this when you use auto-complete.

   This also works for file names:

   `> readFile "/etc/pas"`

   Now hit the TAB key. If there is only one existing filename on a path that
   begins with `/etc/pas`, then that name will auto-complete. Try it.

   If there is more than one identifier that can complete, hit TAB twice
   quickly. This will present you with your options to complete.

4. Follow the types.

   You may find yourself in a position of being unsure how to proceed for a
   given exercise. You are encouraged to adopt a different perspective. Instead
   of asking how to proceed, ask how you might proceed while adhering to the
   guideline provided by the types for the exercise at hand.

   It is possible to follow the types without achieving the desired goal,
   however, this is reasonably unlikely at the start. As you become more reliant
   on following the types, you will develop more trust in the potential paths
   that they can take you, including identification of false paths.

   Your instructor must guide you where types fall short, but you should also
   take the first step. Do it.

5. Do not use tab characters

   Set up your text editor to use space characters rather than tabs.
   Using tab characters in Haskell can lead to confusing error messages.
   GHC will give you a warning if your program contains a tab character.

### Running the tests

Tests are available as a [tasty](https://hackage.haskell.org/package/tasty)
test suite.

#### Hspec

Hspec tests are stored under the `test/` directory. Each module from the course
that has tests has a corresponding `<MODULE>Spec.hs` file. Within each test
module, tests for each function are grouped using the `describe` function.
Within each test group there are test cases (`it` function), and
properties (`prop` function).

To run the full test suite, build the project as follows:

    > stack test

Tasty will also allow you to run only those tests whose description match a
pattern. Tests are organised in nested groups named after the relevant module
and function, so pattern matching should be intuitive. For example, to run the
tests for the `List` module you could run:

    > stack test --ta "--match=Course.List/"

Likewise, to run only the tests for the `headOr` function in the `List` module, you could use:

    > stack test --ta "--match=Course.List/headOr"

In addition, GHCi may be used to run tasty tests. Assuming you have run `ghci`
from the root of the project, you may do the following:

```
# Ensure you've run `make test` at least once!
make test-ghci
```

From there you can run `:main` at your GHCi prompt to run the entire test suite. If you want to do the equivalent of `stack test --ta "--match=ListSpec"` in GHCi do this:

```
:main --match=Course.List/
```

Similarly you can run:

```
:main --match=Course.List/headOr
```

To target just the `Course.List/headOr` tests. If you've changed your code, run `:r` to reload and then run your `:main` invocation again.


### Progression

It is recommended to perform some exercises before others. The first step is to
inspect the introduction modules.

* `Course.ExactlyOne`
* `Course.Validation`

They contain examples of data structures and Haskell syntax. They do not contain
exercises and exist to provide a cursory examination of Haskell syntax. The next
step is to complete the exercises in `Course.Optional`.

After this, the following progression of modules is recommended:

* `Course.List`
* `Course.Functor`
* `Course.Applicative`
* `Course.Monad`
* `Course.FileIO`
* `Course.State`
* `Course.StateT`
* `Course.Extend`
* `Course.Comonad`
* `Course.Compose`
* `Course.Traversable`
* `Course.ListZipper`
* `Course.Parser` *(see also `Course.Person` for the parsing rules)*
* `Course.MoreParser`
* `Course.JsonParser`
* `Course.Interactive`
* `Course.Anagrams`
* `Course.FastAnagrams`
* `Course.Cheque`

During this progression, it is often the case that some exercises are abandoned
due to time constraints and the benefit of completing some exercises over
others. For example, in the progression, `Course.Functor` to `Course.Monad`, the
exercises repeat a similar theme. Instead, a participant may wish to do
different exercises, such as `Course.Parser`. In this case, the remaining
answers are filled out, so that progress on to `Course.Parser` can begin
(which depends on correct answers up to `Course.Monad`). It is recommended to
take this deviation if it is felt that there is more reward in doing so.

After these are completed, complete the exercises in the `projects` directory.

### IDEs

The use of IDEs (integrated development environments) is not recommended as many Haskell beginners will spin their wheels trying to get the tools installed and working.

Instead, if you'd like maximum interactivity and fast feedback the following is recommended:

- A regular text editor which can syntax highlight and indent Haskell source code. Sublime Text, Emacs, vim, Atom, and Visual Studio Code are all suitable for this purpose.

- A GHCi REPL created with `make ghci` or `make test-ghci`.

- For automatic type errors, use `ghcid`. To install ghcid run `make dev-deps` and then to run `ghcid` use: `make ghcid`. When `ghcid` is ready to go, it should look something like:

```
All good (27 modules)
```

From there you should be able to edit a source file, save, and see `ghcid` reload and type-check your code automatically.


### Introducing Haskell

This section is a guide for the instructor to introduce Haskell syntax. Each of
these points should be covered before attempting the exercises.

* values, assignment
* type signatures `::` reads as *has the type*
  * The `->` in a type signature is *right-associative*
* functions are values
* functions take arguments
  * functions take *only one argument* but we approximate without spoken
    language
  * functions can be declared inline using *lambda expressions*
  * the `\` symbol in a lambda expression denotes a Greek lambda
* operators, beginning with non-alpha character, are in infix position by
  default
  * use in prefix position by surrounding with *(parentheses)*
* regular identifiers, beginning with alpha character, are in prefix position by
  default
  * use in infix position by surrounding with ``backticks``
* polymorphism
  * type variables *always* start with a lower-case character
* data types, declared using the `data` keyword
  * following the `data` keyword is the *data type name*
  * following the data type name are zero of more type variables
  * then `=` sign
  * data types have zero or more constructors
    * data type constructors start with an upper-case character, or colon `(:)`
  * following each constructor is a list of zero or more *constructor arguments*
  * between each constructor is a pipe symbol `(|)`
  * the `deriving` keyword gives us default implementations for some functions
    on that data type
  * when constructors appear on the left side of `=` we are *pattern-matching*
  * when constructors appear on the right side of `=` we are *constructing*
* type-classes

### Learning the tools

When this course is run in-person, some tools, particularly within Haskell, are
covered first.

* GHCi
  * `:type`
  * `:info`
  * `:load:`
  * `:reload`
  * `:main`
* values
* type signatures
  * `x :: T` is read as *x is of the type T*
* functions are values
* functions take arguments
* functions take one argument
* lambda expressions
* operators (infix/prefix)
  * identifiers starting with `isAlpha` are prefix by default, infix surrounded in backticks (\`)
  * other identifiers are infix by default, prefix surrounded in parentheses
* data types
  * `data` keyword
  * recursive data types
* pattern matching
* `deriving` keyword
* type-classes
* type parameters
  * always lower-case 'a'..'z'
  * aka generics, templates C++, parametric polymorphism
* running the tests
  * `cabal test`

### Parser grammar assistance

The exercises in `Parser.hs` can be assisted by stating problems in a specific way, with a conversion to code.

| English   | Parser library                    |
|-----------|-----------------------------------|
| and then  | `bindParser` `>>=`                |
| always    | `valueParser` `pure`              |
| or        | `\|\|\|`                             |
| 0 or many | `list`                            |
| 1 or many | `list1`                           |
| is        | `is`                              |
| exactly n | `thisMany n`                      |
| fail      | `failed`                          |
| call it x | `\x ->`                           |

### Monad comprehension

##### do-notation

* insert the word `do`
* turn `>>=` into `<-`
* delete `->`
* delete `\`
* swap each side of `<-`

##### LINQ

* write `from` on each line
* turn `>>=` into in
* delete `->`
* delete `\`
* swap each side of `in`
* turn value into `select`

### Demonstrate IO maintains referential transparency

Are these two programs, the same program?

    p1 ::
      IO ()
    p1 =
      let file = "/tmp/file"
      in  do  _ <- writeFile file "abcdef"
              x <- readFile file
              _ <- putStrLn x
              _ <- writeFile file "ghijkl"
              y <- readFile file
              putStrLn (show (x, y))

    p2 ::
      IO ()
    p2 =
      let file = "/tmp/file"
          expr = readFile file
      in  do  _ <- writeFile file "abcdef"
              x <- expr
              _ <- putStrLn x
              _ <- writeFile file "ghijkl"
              y <- expr
              putStrLn (show (x, y))

What about these two programs?

    def writeFile(filename, contents):
        with open(filename, "w") as f:
        f.write(contents)

    def readFile(filename):
        contents = ""
        with open(filename, "r") as f:
        contents = f.read()
        return contents

    def p1():
        file = "/tmp/file"

        writeFile(file, "abcdef")
        x = readFile(file)
        print(x)
        writeFile(file, "ghijkl")
        y = readFile(file)
        print (x + y)

    def p2():
        file = "/tmp/file"
        expr = readFile(file)

        writeFile(file, "abcdef")
        x = expr
        print(x)
        writeFile(file, "ghijkl")
        y = expr
        print (x + y)

### One-day

Sometimes this course material is condensed into one-day. In these cases, the
following exercises are recommended:

* `Optional`
  * `mapOptional`
  * `bindOptional`
  * `(??)`
  * `(<+>)`
* `List`
  * `headOr`
  * `product`
  * `length`
  * `map`
  * `filter`
  * `(++)`
  * `flatMap`
  * `reverse`
* `Functor`
  * `instance Functor List`
  * `instance Functor Optional`
  * `instance Functor ((->) t)`
  * `instance Functor void`
* `Applicative`
  * `instance Applicative List`
  * `instance Applicative Optional`
  * `instance Applicative ((->) t)`
  * `lift2`
  * `sequence`
* `FileIO`

### References

* [The Haskell `error` function](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#v:error)

* [Glasgow Haskell Compiler](http://haskell.org/ghc)
