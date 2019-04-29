# Clean ABAP

[**Public**](#public)
&nbsp;·&nbsp;
[**Optional**](#optional)
&nbsp;·&nbsp;
[**Continuous Release**](#continuous-release)
&nbsp;·&nbsp;
[**Open Source**](#open-source)
&nbsp;·&nbsp;
[**Grassroots Project**](#grassroots-project)

This guide is an adoption of
[Robert C. Martin's _Clean Code_](https://www.oreilly.com/library/view/clean-code/9780136083238/)
for [ABAP](https://en.wikipedia.org/wiki/ABAP).

The [Cheat Sheet](../cheat-sheet/CheatSheet.md) is a print-optimized version.

## Content

- [About this guide](#about-this-guide)
  - [Public](#public)
  - [Optional](#optional)
  - [Continuous Release](#continuous-release)
  - [Open Source](#open-source)
  - [Grassroots Project](#grassroots-project)
- [How to](#how-to)
  - [How to Get Started with Clean Code](#how-to-get-started-with-clean-code)
  - [How to Refactor Legacy Code](#how-to-refactor-legacy-code)
  - [How to Relate to Other Guides](#how-to-relate-to-other-guides)
  - [How to Disagree](#how-to-disagree)  

## About this guide

> [Clean ABAP](#clean-abap) > [Content](#content)

### Public

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide)

This document is **public**, as described in our
SAP's _Global Information Classification & Handling Standard_
[(internal link)](https://wiki.wdf.sap.corp/wiki/pages/viewpage.action?pageId=1891700683),
meaning you can freely share it with anybody.

We believe that everybody should be enabled to code cleanly.

### Optional

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide)

Following this guide is **optional**, meaning you -
or more precisely: your team -
can choose whether you want to adhere to it.
This applies equally to in-house developers, partners, and customers.

We believe that clean code comes from the heart, not from pressure.

### Continuous Release

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide)

This guide is updated **continuously**,
meaning any change is reviewed and immediately put "live",
without special publication versions.

As ABAP and our understanding of Clean Code evolve,
we believe that this document is "work in progress"
and will probably never see a status "finished";
as agile developers, we welcome this.

### Open Source

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide)

This repository is **open source**,
meaning it is written by a loose community of interested persons,
and anybody from within and without SAP is invited to contribute.

[LICENSE.md](../LICENSE.md) describes how you may use this material,
while [CONTRIBUTING.md](../CONTRIBUTING.md) describes how you can contribute.

We believe that clean code should be discussed freely and openly.

### Grassroots Project

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide)

This guide is a **grassroots project**, meaning it was started, and is still driven,
by programmers who spend their day coding, and want to get better at it.

It was first conceived as a team-specific Wiki, then turned into an SAP-wide private repository,
before being published here. It was thus distilled from many many years of ABAP experience and thorough code reviews.

We are developers, architects, quality engineers, and consultants,
from associates to chief experts, from language creators to tool developers,
from S/4HANA to the ABAP language group.
We respect all roles, ranks, and units, and welcome any suggestions and improvements.

## How to

> [Clean ABAP](#clean-abap) > [Content](#content)

### How to Get Started with Clean Code

> [Clean ABAP](#clean-abap) > [Content](#content) > [How To](#how-to)

If you are new to Clean Code, the [Clean Code Developer initiative](https://clean-code-developer.com/)
may help you getting started with a didactically smooth stepwise introduction to the topic in general.

We recommend you to start with things that are easily understood and broadly accepted,
such as [Booleans](#booleans), [Conditions](#conditions), and [Ifs](#ifs).

You will probably benefit most from the section [Methods](#methods),
especially [Do one thing, do it well, do it only](#do-one-thing-do-it-well-do-it-only) and [Small](#small),
because these tremendously improve the overall structure of your code.

Some topics in here can spark difficult discussions in teams
that are experienced in what they do but new to Clean Code;
these topics are perfectly "healthy", but people may have problems
making themselves comfortable with them in the beginning.

Continue to these more controversial topics later;
especially [Comments](#comments), [Naming](#naming), and [Formatting](#formatting)
can lead to near-religious disputes
and should only be addressed by teams that already saw proof of Clean Code's positive effects.

### How to Refactor Legacy Code

> [Clean ABAP](#clean-abap) > [Content](#content) > [How-to](#how-to)

The topics [Booleans](#booleans), [Conditions](#conditions), [Ifs](#ifs), and [Methods](#methods)
are most rewarding if you are working on a legacy project with tons of code that you cannot or do not want to change:
they can be applied to new code without conflicts, and may be applied to old code following the boy scout rule
_("always leave the code you're editing a little better than you found it")_.

The topic [Naming](#naming) is very demanding for legacy projects,
as it may introduce a breach between old and new code,
up to a degree where sections like
[Avoid encodings, esp. Hungarian notation and prefixes](#avoid-encodings-esp-hungarian-notation-and-prefixes)
are better ignored.

### How to Relate to Other Guides

> [Clean ABAP](#clean-abap) > [Content](#content) > [How-to](#how-to)

Our guide follows the _spirit_ of Clean Code,
meaning we adjusted some things to the ABAP programming language
e.g. [Throw CX_STATIC_CHECK for manageable exceptions](#throw-cx_static_check-for-manageable-exceptions).

Some facts are from the
[ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenabap_pgl.htm),
which this guide is mostly compatible to; deviations are indicated and always in the spirit of cleaner code.

This guide also respects the
[DSAG's Recommendations for ABAP Development](https://www.dsag.de/sites/default/files/dsag_recommendation_abap_development.pdf),
although we are more precise in most details.

### How to Disagree

> [Clean ABAP](#clean-abap) > [Content](#content) > [How-to](#how-to)

We wrote this style guide for readers who are already acquainted with Clean Code or who are right now working on that,
with a strong focus on how to apply Clean Code _specifically to ABAP_.

Please mind that we therefore did not introduce all concepts in the same length and depth
as the original book and related resources: these are still worth a read,
especially if you disagree with things in here just because we didn't explain them very well.
Use the links in the sections to read up on the background of our guidance.

You are free to discuss and disagree with anything we say here.
One of the pillars of Clean Code is that _the team rules_.
Just be sure to give things a fair chance before you discard them.

[CONTRIBUTING.md](../CONTRIBUTING.md) suggests ways how you can change this guide or deviate from it in minor details.