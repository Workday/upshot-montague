montague
========

`montague` is a little CCG semantic parsing library for Scala.

You can build on this code to translate English-into-SQL,
English-into-API commands, etc.

Authors
-------

* Thomas Am Kim
* Joseph Turian
* Aleksander Nisnevich

Background
----------

At UPSHOT (acquired by Workday), we built a semantic parser that
translated English into SQL, and---later---English into SOQL (the
Salesforce query language).

We are open-sourcing the semantic parser component, in the hopes
that other people find it useful and educational. We plan to clean
up the SQL-generation code and release that too.

Getting Started
---------------

English-to-calculator arithmetic
================================

In this example, English is parsed into a semantic form, which is
then realized as arithmetic operations.

```sh
sbt runMain [blah blah blah]
```

English-to-semantic structure
================================

Using the CCGBank lexicon, we parse English sentences into 

```sh
sbt runMain [blah blah blah]
```

Functionality
-------------

[Non-probabilistic]

Possible future projects
------------------------

Technical improvements
======================

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
