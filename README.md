montague
========

`montague` is a little CCG semantic parsing library for Scala.

You can build on this code to translate English-into-SQL,
English-into-API commands, etc.

[need some nice graphic here]

Authors
-------

* Thomas Am Kim
* Joseph Turian
* Aleksandr Nisnevich

Background
----------

"Oh, get ahold of yourself. Nobody's proposing that we parse English."
— Larry Wall in `<199709032332.QAA21669@wall.org>`

`montague` takes its name from [Montague
Semantics](https://en.wikipedia.org/wiki/Montague_grammar), the
idea that human language can be expressed through formal logic and
lambda-calculus.  Specifically, `montague` implements [Combinatory
Categorial Grammar
(CCG)](https://en.wikipedia.org/wiki/Combinatory_categorial_grammar), a
particular grammar formalism that has become popular recently for
semantic parsing.

[Semantic parsing]

History
-------

At UPSHOT (acquired by Workday), we built a semantic parser that
translated English into SQL, and—later—English into SOQL (the
Salesforce query language).

This package improves upon and open-sources the CCG-based semantic
parser component of UPSHOT. We hope that that other people find it
useful and educational. We plan to clean up the SQL-generation code
and release that too.

Getting Started
---------------

### English-to-calculator arithmetic

See `example.ArithmeticParser`.

In this example, English is parsed into a semantic form, which is
then realized as arithmetic operations.

```sh
> sbt "runMain example.ArithmeticParser (3 + 5) * 2"
Input: (3 + 5) * 2
Output: Form(16)
```

Because CCG doesn't have a built-in notion of precedence, all binary operations have equal precedence, and thus
syntactic ambiguity can result:

```sh
> sbt "runMain example.ArithmeticParser 3 + 5 * 2"
Input: 3 + 5 * 2
Output: Ambiguous(Set(Form(13), Form(16)))
```

You can also add ambiguity through the use of terms with multiple semantic definitions, such as `+/-`:

```sh
> sbt "runMain example.ArithmeticParser (3 +/- 5) * 2"
Input: (3 +/- 5) * 2
Output: Ambiguous(Set(Form(16), Form(-4)))
```

The `Else` clause in the lexicon definition allows this parser to ignore all unrecognized tokens:
```
> sbt "runMain example.ArithmeticParser Could you please tell me, what is 100 + 100 ?"
Input: Could you please tell me, what is 100 + 100 ?
Output: Form(200)
```

### English-to-semantic structure

Using the CCGBank lexicon, we parse English sentences into

```sh
sbt runMain [blah blah blah]
```

If you have the CCGBank lexicon and would like to use it, [blah blah blah]

If you don't have CCGBank lexicon, you can download an older version
of it from (Julia
Hockenmaier)[http://juliahmr.cs.illinois.edu/CCGlexicon/]:

```sh
pushd data/ && wget http://juliahmr.cs.illinois.edu/CCGlexicon/lexicon.wsj02-21.gz && gunzip lexicon.wsj02-21.gz && popd
```

Functionality
-------------

[Non-probabilistic]

Possible future projects
------------------------

### Technical improvements

* Probabilistic parser with rules. [...]
* Faster parsing:
	* Agenda-based parsing: Instead of building the parse using
	CKY (bottom-up), use an agenda to order nodes by some
	priority. For example, the priority can be the cumulative
	probability of applying the rules (best-first parsing).
	* Beam search

Library overview
----------------

[here's how you build something new]

[here's some cool Scala stuff that's excellent for CCG]

Related academic work
---------------------

TODO
----

- Low-level unit tests
- High-level tests for parsing and semantic comprehension (i.e. the
things now in `SemanticParser#main`)
- Add pretty examples to the readme
- Cleanup of UPSHOT-specific code
- Bring in a simplified version of the SQL-specific stuff from UPSHOT
- Some more in-depth fun examples - e.g. game logic? SQL-like?
- Fix up toSvg output
