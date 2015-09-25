montague
========

`montague` is a little CCG semantic parsing library for Scala.

You can build on this code to translate English-into-SQL,
English-into-API commands, etc.

The code currently implements boolean (non-probabilistic) CCG
parsing, using a [CKY](https://en.wikipedia.org/wiki/CYK_algorithm)-based
parse search strategy.

![An example parse tree](https://ghe.megaleo.com/upshot/montague/blob/master/example.png?raw=true "Example parse tree")

Authors
-------

* [Thomas Kim](https://twitter.com/tksfz)
* Joseph Turian
* [Aleksandr Nisnevich](http://alex.nisnevich.com)

Background
----------

> "Oh, get ahold of yourself. Nobody's proposing that we parse English."
> — Larry Wall in `<199709032332.QAA21669@wall.org>`

`montague` takes its name from [Montague
semantics](https://en.wikipedia.org/wiki/Montague_grammar), the
idea that human language can be expressed through formal logic and
lambda-calculus. The process of inferring this formal representation
from natural language is called "semantic parsing". Specifically,
`montague` implements [Combinatory Categorial Grammar
(CCG)](https://en.wikipedia.org/wiki/Combinatory_categorial_grammar), a
particular grammar formalism that has become popular recently for
semantic parsing.

### Lambda-calculus? Combinatory grammar? Huh??

Let's break it down.
Here's an example of a definition in `montague`:
```scala
  ("plus" -> ((N\N)/N, λ {y: Int => λ {x: Int => x + y}}))
```

Here's what it means:
- There's a term called _"plus"_.
- It has the syntactic category `(N\N)/N`. This means that it's something that
  attaches to a noun (`N`) after it to form a `N\N`, which is a thing that attaches
  to a noun before it to form another noun. In other words, "plus" must be between
  two nouns, and the end result of `(Noun) plus (Noun)` syntactically is just another noun.
  So far, so good!
- It has the semantic definition `λ {y: Int => λ {x: Int => x + y}}`. In other words,
  it's a function that takes an integer and returns a function that another integer, adding
  the first integer to it. Uncurrying it (because in Montague semantics, all functions must
  be curried) simply yields `λ {x: Int, y: Int => x + y}`. Well, that's pretty straightforward.

Looking through the code of the examples below, you'll notice that not all
definitions look quite like this. Some
of them have multiple synonyms for one definition, or multiple definitions
for a single term (in that case, we say that the term is _ambiguous_).
Some of them don't operate on single terms at all, but on _matchers_ (functions that try
to find certain kinds of strings). Some of them don't have semantic definitions
at all and only describe syntactic categories. But the general idea for all of these
is the same.

The way you use `montague` is by defining your own _lexicon_ of terms with syntactic
and semantic definitions. And the semantic parser does the rest.

History
-------

At [UPSHOT](http://blogs.workday.com/workday-acquires-upshot/)
(acquired by Workday), we built a semantic parser that translated
English into SQL, and—later—English into SOQL (the Salesforce query
language).

This package improves upon and open-sources the CCG-based semantic
parser component of UPSHOT. We hope that that other people find it
useful and educational. We plan to clean up the SQL-generation code
and release that too.

Getting Started
---------------

`montague` comes with a few simple examples demonstrating some applications of its semantic parsing features.

### English-to-calculator arithmetic

(See [`ArithmeticParser`](https://ghe.megaleo.com/upshot/montague/blob/master/src/main/scala/example/ArithmeticParser.scala).)

In this example, English is parsed into a semantic form, which is
then realized as arithmetic operations.

```sh
> sbt "runMain example.ArithmeticParser (3 + 5) * 2"
Input: (3 + 5) * 2
Output: 16
```

Our current implementation treats all grammatical rule applications
as either possible (true) or impossible (false).  For this reason,
the parser cannot currently discriminate between rules of different
precedence:

```sh
> sbt "runMain example.ArithmeticParser 3 + 5 * 2"
Input: 3 + 5 * 2
Output: Ambiguous(13, 16)
```

Besides ambiguity arising from the inability to discriminate between
different rule applications, we might want intentionally to encode
ambiguity into our language. For example, the `+/-` operation is
intentionally ambiguous, and has multiple valid semantic
interpretations:

```sh
> sbt "runMain example.ArithmeticParser (3 +/- 5) * 2"
Input: (3 +/- 5) * 2
Output: Ambiguous(16, -4)
```

We ignore all unrecognized tokens by adding an `Else` clause in the
lexicon, which matches all tokens that wouldn't match otherwise, and in
this case produces semantically null parses:

```
> sbt "runMain example.ArithmeticParser Could you please tell me, what is 100 + 100 ?"
Input: Could you please tell me, what is 100 + 100 ?
Output: 200
```

### English-to-syntactic structure

(See [`CcgBankParser`](https://ghe.megaleo.com/upshot/montague/blob/master/src/main/scala/example/CcgBankParser.scala).)

Using the CCGBank lexicon, we parse English sentences into syntax dependency trees.

If you don't have the CCGBank lexicon, you can download an older version
of it from [Julia Hockenmaier's site](http://juliahmr.cs.illinois.edu/CCGlexicon/):

```sh
pushd data/ && \
wget http://juliahmr.cs.illinois.edu/CCGlexicon/lexicon.wsj02-21.gz && \
gunzip lexicon.wsj02-21.gz && \
popd
```

You can then parse sentences using the old CCGBank lexicon as follows:

```sh
> sbt "runMain example.OldCcgBankParser Thom and Alex and Joseph are writing a parser"
Input: Thom and Alex and Joseph are writing a parser
Output:
  are
    writing
      a
        parser
    and
      joseph
      and
        alex
        thom
```

If you do have the CCGBank lexicon and would like to use it, put
`CCGbank.00-24.lexicon` into subdirectory `data/`, and invoke the
parser as follows:

```sh
> sbt "runMain example.CcgBankParser Thom and Alex and Joseph are writing a parser"
```

### English-to-information storage and retrieval

(See [`InformationStore`](https://ghe.megaleo.com/upshot/montague/blob/master/src/main/scala/example/InformationStore.scala).)

`InformationStore` uses [`SemanticRepl`](https://ghe.megaleo.com/upshot/montague/blob/master/src/main/scala/parser/SemanticRepl.scala) to implement a very basic information storage and retrieval system, by parsing statements into `Define` constructs and queries into `Query` constructs, then executing them accordingly.

**Important:** This example uses the older-style CCGBank lexicon - see [the above example](#english-to-semantic-structure) for download instructions.

An example interactive session with `InformationStore`:
```sh
> sbt "runMain example.InformationStore"
>> Joseph is a programmer
Ok
>> Joseph is pretty weird
Ok
>> Who is Joseph?
{a(programmer), pretty(weird)}
>> Who are Joseph and Ted?
I don't know
>> Joseph and Ted are Alex's bandmates
Ok
>> Who are Joseph and Ted?
alex's(bandmates)
```

Library overview
----------------

### `SemanticParser`

`SemanticParser` is the main entry point into _montague_. To instantiate a `SemanticParser`, you need a syntactic scheme (`CcgCat` for our purposes) and a lexicon, stored in a `ParserDict`.

Once you've instantiated a `SemanticParser`, `.parse(text, tokenizer)` yields a `SemanticParseResult`, which you can unpack to find the parse tree and resulting semantic representation (if the parse succeeded). See `SemanticParser.main` for an example of how to extract results.

##### Lexicons

To build up a lexicon, you can create a new `ParserDict()` (or load syntactic entries from a CCGbank lexicon, if you have one, with `ParserDict.fromCcgBankLexicon`) and add entries to it with the `+` operator.

A lexicon entry looks like `(matcher -> meaning)`, where
- `matcher` can be
    1. a term (String),
    2. a Seq of terms,
    3. an instance of `TokenMatcher[T]` (a `String => Seq[T]` function), or
    4. `Else`, which matches any otherwise un-matched token; and
- `meaning` can be
    1. a syntactic category,
    2. a `(`syntactic category`,` semantic representation`)` pair,
    3. a Seq of either of the above _(in which case the meaning of the term is ambiguous)_, or
    4. _(only if the `matcher` is a `TokenMatcher` or `Else`)_, a function of the matched object that produces of Seq of syntactic categories or a `(`syntactic category`,` semantic representation`)` pairs

##### `SemanticRepl`

`SemanticRepl` is a wrapper on top of `SemanticParser` that's useful for making REPLs that repeatedly read input, parse it into an "action", and pattern-match that action to perform some operation against an internal state. For an example of `SemanticRepl` at work, see the [`InformationStore`](https://ghe.megaleo.com/upshot/montague#english-to-information-storage-and-retrieval) example above.

### Syntactic Categories

_montague_ supports arbitrary syntactic schemes, but the only one built-in is `CcgCat`, representing CCG categories. Here are the categories available in the `ccg` package:

- _Terminal_ syntactic categories are ones that can appear at the top of the parse tree and cannot consume adjacent terms. Built-in terminals are `S` ("sentence"), `N` ("noun"), `NP` ("noun phrase"), and `PP` ("prepositional phrase"), but others are easy to add, depending on your application.
- _Non-terminal_ syntactic categories are ones that can (and must) consume adjacent terms. A parse cannot succeed if there is a non-terminal at the top of the parse tree. Types of non-terminal categories are:
   - _Forward application_: `A/B` consumes a `B` in front of it to become an `A`.
   - _Backward application_: `A\B` consumes a `B` behind it to become an `A`.
   - _Bidirectional application_: `A|B` consumes an adjacent `Y` to become an `A`.
   - _Identity categories_: `X|X`, `X/X`, and `X\X` are special cases of the above -- they can consume a term of _any category_ to become that category.
   - `Conj`, the _conjunction_ category, is a short-hand for `(X\X)/X`.
     _(Exercise: Why is this called the "conjunction" category?)_
- Additionally, a category assigned to a term may have a probability attached to it: `Cat % prob`. For example, if the term _apple_ has categories `N % 0.9` and `(NP/N) % 0.1`, that means that it's 9 times more likely to be a noun than an adjective, and the parser will score potential parses accordingly. Probabilities default to `1.0` if unspecified.
- A category may also have a label: `Cat("label")`. `A/B("somelabel")` can consume a `B("somelabel")` but not a regular `B`.

### Semantic Representations

A `SemanticState` is generally one of two things (in each case, `LF` corresponds to the type of the objects we're dealing with -- in the examples below, `LF = Int`):

- A _form_ `Form[LF]` represents a semantic state that is "complete" (i.e. doesn't consume any arguments) -- for example, `Form(4)` represents the number four.
- A _lambda_ `λ {x: LF => <semantic state>}` represents a semantic state that must consume arguments -- for example, `λ {x: Int => Form(x + 5)}` represents the function that adds 5 to any integer. Implicit conversions allow us to write this more concisely to `λ {x: Int => x + 5}`. Multi-argument functions are represented by currying (e.g. `λ {y: Int => λ {x: Int => x + y}}`).

There are a few other possible `SemanticStates` that generally shouldn't be specified directly in the lexicon, but can appear in parse results:
- `Nonsense` represents a parse with no valid semantic outputs.
- `Ambiguous(Set[SemanticState])` represents a parse with more than one valid semantic output.
- `Ignored` represents a parse that ignored semantics entirely (i.e. you didn't specify semantic representations in the lexicon).

Exercises for the reader
------------------------
_(In rough order of difficulty.)_

### Parsing

1. While all of our examples involve CCG parsing, _montague_ supports alternative syntactic schemes. Create your own semantic scheme (that is, a type hierarchy that inherits from `SyntacticLabel`), and parse something with it.
1. **Composition.** _montague_'s CCG implementation currently supports only one of the three CCG combinators: the _application_ combinator (`X/Y Y -> X`, `X X\Y -> Y`). Extend it to also support the [_composition_ combinator](https://en.wikipedia.org/wiki/Combinatory_categorial_grammar#Composition_Combinators) (`X/Y Y/Z -> X/Z`).
1. **Type-raising.** As above, but for the [_type-raising_ combinator](https://en.wikipedia.org/wiki/Combinatory_categorial_grammar#Type-raising_Combinators) (`X -> T/(T\X)`).
1. **Probabilistic parsing.** The parser currently supports boolean parsing: a parse of the input
string is either possible (true) or impossible (false).
_(This corresponds to implementing the boolean semiring parser of
[Goodman, 1999](http://www.aclweb.org/anthology/J99-4004) -- see
Figure 5.)_
Extend the code so that it supports
probabilistic or weighted parsing. _(The existing probabilistic
implementation of English parsing, based upon multiplying out lexicon
weights, is a hack that including the token weights within the CCG
category.)_
1. **Speed improvements.** The parser implements a [CKY](https://en.wikipedia.org/wiki/CYK_algorithm) search strategy, which is bottom-up.
If the parser had weight implemented, we could parse faster using
agenda-based parsing: You use an agenda to order nodes by some
priority. For example, the priority can be the cumulative probability
of applying the rules (best-first parsing). Alternately, instead
of agenda-based parsing, beam pruning could be used to reduce the
size of the search space. In this case, only the top *k* weighted
nodes are kept in any parse cell.
1. **† Fuzzy matching.** What if a user enters a phrase that doesn't parse successfully, but adding or removing one word (or perhaps correcting a misspelling) would fix it? Create a _"Did You Mean?"_ feature that implements this efficiently.

### Applications

1. Add more features to the `ArithmeticParser` example. For example, improve the tokenizer to correctly handle infix expressions without spaces (e.g. `(1+2)*3`), or add more operations.
1. Add more features to the `InformationStore` example. For example, add other types of relations, or support more kinds of expressions.
1. **IFTTT.** Parse English phrases like _"When (this happens), (do this)"_ into semantic forms corresponding to [IFTTT](https://ifttt.com/) API calls. Try building a REPL on top of `SemanticRepl` for communicating with IFTTT via natural language.
1. **Slack bot.** Similar to the above, but do something cool with a [Slack bot](https://api.slack.com/bot-users) instead.
1. **† Game semantics.** Come up with a semantic scheme for representing rule descriptions for a simple card game (think _Magic_, _Hearthstone_, etc., but simplify!) For example, a card may say something like _"Whenever your opponent loses life, draw a card"_. Then write a parser for it.
1. **† English to Freebase.** Parse English phrases into [Freebase](https://www.freebase.com/) queries. For example (borrowing an example from [SEMPRE](http://www-nlp.stanford.edu/software/sempre/)), _"Which college did Obama go to?"_ → `(and (Type University) (Education BarackObama))` → _"Occidental College, Columbia University"_. (_Hint_: You'll have to generate most of the lexicon programmatically using Freebase as well.)
1. **† English to SQL.** Parse English questions (such as _"How many customers in Europe made a purchase last month?"_) into SQL statements. Assume that you have all relevant table structure information available. (_Hints_: Generate an abstract structure for the full parse first, and then worry about generating SQL out of it. Much of the lexicon will have to be generated from table and column names, as well as entries for categorical columns.  There will be a *lot* of ambiguous definitions.)

Related work
------------

* [SEMPRE](http://www-nlp.stanford.edu/software/sempre/) is a toolkit
for training semantic parsers.
