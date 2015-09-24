montague
========

`montague` is a little CCG semantic parsing library for Scala.

You can build on this code to translate English-into-SQL,
English-into-API commands, etc.

The code currently implements boolean (non-probabilistic) CCG
parsing, using a [CKY](https://en.wikipedia.org/wiki/CYK_algorithm)-based
parse search strategy.

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
lambda-calculus. The process of inferring this formal representation
from natural language is called "semantic parsing". Specifically,
`montague` implements [Combinatory Categorial Grammar
(CCG)](https://en.wikipedia.org/wiki/Combinatory_categorial_grammar), a
particular grammar formalism that has become popular recently for
semantic parsing.

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

### English-to-calculator arithmetic

See [`example.ArithmeticParser`](https://ghe.megaleo.com/upshot/montague/blob/master/src/main/scala/example/ArithmeticParser.scala).

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
lexicon, which [...]:

```
> sbt "runMain example.ArithmeticParser Could you please tell me, what is 100 + 100 ?"
Input: Could you please tell me, what is 100 + 100 ?
Output: 200
```

### English-to-semantic structure

Using the CCGBank lexicon, we parse English sentences into

```sh
> sbt "runMain example.CcgBankParser Alex and Joseph are writing a parser"
Input: Alex and Joseph are writing a parser
Highest-scoring parse: are(writing(a(parser)))(and(joseph)(alex))
Some(([0,7], 'S [prob = 0.116236165]', ([3,7], '(S[dcl]\NP) [prob = 0.1440928]', ('((S[dcl]\NP)/(S[ng]\NP)) [prob = 0.276222]', 'are'),
([4,7], '(S[ng]\NP) [prob = 0.52165574]', ('((S[ng]\NP)/NP) [prob = 0.636364]', 'writing'),
([5,7], 'NP [prob = 0.8197443]', ('(NP[nb]/N) [prob = 0.910827]', 'a'),
('N [prob = 0.9]', 'parser')))),
([0,3], 'N [prob = 0.80667573]', ([1,3], '(N\N) [prob = 0.8963064]', ('((X\X)/X) [prob = 0.995896]', 'and'),
('N [prob = 0.9]', 'joseph')),
('N [prob = 0.9]', 'alex'))))
```

If you have the CCGBank lexicon and would like to use it, [blah
blah blah]

If you don't have CCGBank lexicon, you can download an older version
of it from [Julia
Hockenmaier](http://juliahmr.cs.illinois.edu/CCGlexicon/):

```sh
pushd data/ && \
wget http://juliahmr.cs.illinois.edu/CCGlexicon/lexicon.wsj02-21.gz && \
gunzip lexicon.wsj02-21.gz && \
popd
```

Functionality and limitations
-----------------------------

The code currently supports boolean parsing: A parse of the input
string is either possible (true) or impossible (false).

This corresponds to implementing the boolean semiring parser of
[Goodman, 1999](http://www.aclweb.org/anthology/J99-4004) (see
Figure 5).

A possible future project is to extend the code so that it supports
probabilistic or weighted parsing. (The existing probabilistic
implementation of English parsing, based upon multiplying out lexicon
weights, is hacked by including the token weights within the CCG
category.)

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

Related work
------------

* [SEMPRE](http://www-nlp.stanford.edu/software/sempre/) is a toolkit
for training semantic parsers.

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
