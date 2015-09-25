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
  it's a function that takes an integer and returns a function that an integer, adding
  the first integer to it. Uncurrying it (because in Montague semantics, all functions must
  be curried) simply yields `λ {x: Int, y: Int => x + y}`. Well, that's pretty straightforward.

Looking through the code of the examples below, you'll notice that not all
definitions look quite like this. Some
of them have multiple synonyms for one definition, or multiple definitions
for a single term (in that case, we say that the term has _semantic ambiguity_).
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

(See [`CcgBankParser`](https://ghe.megaleo.com/upshot/montague/blob/master/src/main/scala/example/CcgParser.scala).)

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

This example uses the older-style CCGBank lexicon - see [the above example](#english-to-semantic-structure) for download instructions.

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

[here's how you build something new]

[here's some cool Scala stuff that's excellent for CCG]

Exercises for the reader
------------------------
(In rough order of difficulty.)

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
1. **IFTTT.** Parse English phrases like _"When <this happens>, <do this>"_ into semantic forms corresponding to [IFTTT](https://ifttt.com/) API calls. Try building a REPL on top of `SemanticRepl` for communicating with IFTTT via natural language.
1. **Slack bot.** Similar to the above, but do something cool with a [Slack bot](https://api.slack.com/bot-users) instead.
1. **† Game semantics.** Come up with a semantic scheme for representing rule descriptions for a simple card game (think _Magic_, _Hearthstone_, etc., but simplify!) For example, a card may say something like _"Whenever your opponent loses life, draw a card"_. Then write a parser for it.
1. **† English to Freebase.** Parse English phrases into [Freebase](https://www.freebase.com/) queries. For example (borrowing an example from [SEMPRE](http://www-nlp.stanford.edu/software/sempre/)), _"Which college did Obama go to?"_ → `(and (Type University) (Education BarackObama))` → _"Occidental College, Columbia University"_. (_Hint_: You'll have to generate most of the lexicon programmatically using Freebase as well.)
1. **† English to SQL.** Parse English questions (such as _"How many customers in Europe made a purchase last month?"_) into SQL statements. Assume that you have all relevant table structure information available. (_Hints_: Generate an abstract structure for the full parse first, and then worry about generating SQL out of it. Much of the lexicon will have to be generated from table and column names, as well as entries for categorical columns.  There will be a *lot* of ambiguous definitions.)

Related work
------------

* [SEMPRE](http://www-nlp.stanford.edu/software/sempre/) is a toolkit
for training semantic parsers.
