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
[Robert C. Martin's _Clean Code_]
for [ABAP](https://en.wikipedia.org/wiki/ABAP).

The [Cheat Sheet](../cheat-sheet/CheatSheet.md) is a print-optimized version.

[Robert C. Martin's _Clean Code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

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
- [Names](#names)
  - [Use descriptive names](#use-descriptive-names)
  - [Prefer solution domain and problem domain terms](#prefer-solution-domain-and-problem-domain-terms)
  - [Use plural](#use-plural)
  - [Use pronounceable names](#use-pronounceable-names)
  - [Avoid abbreviations](#avoid-abbreviations)
  - [Use same abbreviations everywhere](#use-same-abbreviations-everywhere)
  - [Use nouns for classes and verbs for methods](#use-nouns-for-classes-and-verbs-for-methods)
  - [Avoid noise words such as "data", "info", "object"](#avoid-noise-words-such-as-data-info-object)
  - [Pick one word per concept](#pick-one-word-per-concept)
  - [Use pattern names only if you mean them](#use-pattern-names-only-if-you-mean-them)
  - [Avoid encodings, esp. Hungarian notation and prefixes](#avoid-encodings-esp-hungarian-notation-and-prefixes)
    - [Reasoning](#reasoning)
    - [Arguments](#arguments)
    - [Compromises](#compromises)
- [Language](#language)
  - [Mind the legacy](#mind-the-legacy)
  - [Mind the performance](#mind-the-performance)
  - [Prefer object orientation over imperative programming](#prefer-object-orientation-over-imperative-programming)
    - [Function Groups vs. Classes](#function-groups-vs-classes)
  - [Prefer functional over procedural language constructs](#prefer-functional-over-procedural-language-constructs)
  - [Use design patterns wisely](#use-design-patterns-wisely)
- [Constants](#constants)
  - [Use constants instead of magic numbers](#use-constants-instead-of-magic-numbers)
  - [Prefer enumeration classes over constants interfaces](#prefer-enumeration-classes-over-constants-interfaces)
    - [Enumerations: Constant Pattern](#enumerations-constant-pattern)
    - [Enumerations: Object Pattern](#enumerations-object-pattern)
    - [Enumerations: Anti-Pattern](#enumerations-anti-pattern)
    - [Enumerations: Benefits](#enumerations-benefits)
  - [If you don't use enumeration classes, group your constants](#if-you-dont-use-enumeration-classes-group-your-constants)
- [Variables](#variables)
  - [Prefer inline over up-front declarations](#prefer-inline-over-up-front-declarations)
  - [Don't declare inline in optional branches](#dont-declare-inline-in-optional-branches)
  - [Do not chain up-front declarations](#do-not-chain-up-front-declarations)
  - [Prefer REF TO over FIELD-SYMBOL](#prefer-ref-to-over-field-symbol)
- [Tables](#tables)
  - [Use the right table type](#use-the-right-table-type)
  - [Avoid DEFAULT KEY](#avoid-default-key)
  - [Prefer INSERT INTO TABLE over APPEND TO](#prefer-insert-into-table-over-append-to)
  - [Prefer LINE_EXISTS over READ TABLE or LOOP AT](#prefer-line_exists-over-read-table-or-loop-at)
  - [Prefer READ TABLE over LOOP AT](#prefer-read-table-over-loop-at)
  - [Prefer LOOP AT WHERE over nested IF](#prefer-loop-at-where-over-nested-if)
  
## About this guide

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#about-this-guide)

### Public

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide) > [This section](#public)

This document is **public**, as described in our
SAP's _Global Information Classification & Handling Standard_
[(internal link)](https://wiki.wdf.sap.corp/wiki/pages/viewpage.action?pageId=1891700683),
meaning you can freely share it with anybody.

We believe that everybody should be enabled to code cleanly.

### Optional

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide) > [This section](#optional)

Following this guide is **optional**, meaning you -
or more precisely: your team -
can choose whether you want to adhere to it.
This applies equally to in-house developers, partners, and customers.

We believe that clean code comes from the heart, not from pressure.

### Continuous Release

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide) > [This section](#continuous-release)

This guide is updated **continuously**,
meaning any change is reviewed and immediately put "live",
without special publication versions.

As ABAP and our understanding of Clean Code evolve,
we believe that this document is "work in progress"
and will probably never see a status "finished";
as agile developers, we welcome this.

### Open Source

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide) > [This section](#open-source)

This repository is **open source**,
meaning it is written by a loose community of interested persons,
and anybody from within and without SAP is invited to contribute.

[LICENSE.md](../LICENSE.md) describes how you may use this material,
while [CONTRIBUTING.md](../CONTRIBUTING.md) describes how you can contribute.

We believe that clean code should be discussed freely and openly.

### Grassroots Project

> [Clean ABAP](#clean-abap) > [Content](#content) > [About this guide](#about-this-guide) > [This section](#grassroots-project)

This guide is a **grassroots project**, meaning it was started, and is still driven,
by programmers who spend their day coding, and want to get better at it.

It was first conceived as a team-specific Wiki, then turned into an SAP-wide private repository,
before being published here. It was thus distilled from many many years of ABAP experience and thorough code reviews.

We are developers, architects, quality engineers, and consultants,
from associates to chief experts, from language creators to tool developers,
from S/4HANA to the ABAP language group.
We respect all roles, ranks, and units, and welcome any suggestions and improvements.

## How to

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#how-to)

### How to Get Started with Clean Code

> [Clean ABAP](#clean-abap) > [Content](#content) > [How to](#how-to) > [This section](#how-to-get-started-with-clean-code)

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

> [Clean ABAP](#clean-abap) > [Content](#content) > [How to](#how-to) > [This section](#how-to-refactor-legacy-code)

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

> [Clean ABAP](#clean-abap) > [Content](#content) > [How to](#how-to) > [This section](#how-to-relate-to-other-guides)

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

> [Clean ABAP](#clean-abap) > [Content](#content) > [How to](#how-to) > [This section](#how-to-disagree)

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

## Names

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#names)

### Use descriptive names

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#use-descriptive-names)

Use names that convey the content and meaning of things.

```ABAP
CONSTANTS max_wait_time_in_seconds TYPE i ...
DATA customizing_entries TYPE STANDARD TABLE ...
METHODS read_user_preferences ...
CLASS /clean/user_preference_reader ...
```

Do not focus on the data type or technical encoding.
They hardly contribute to understanding the code.

```ABAP
" anti-pattern
CONSTANTS sysubrc_04 TYPE sysubrc ...
DATA iso3166tab TYPE STANDARD TABLE ...
METHODS read_t005 ...
CLASS /dirty/t005_reader ...
```

[Do not attempt to fix bad names by comments.](#comments-are-no-excuse-for-bad-names)

> Read more in _Chapter 2: Meaningful Names: Use Intention-Revealing Names_ of [Robert C. Martin's _Clean Code_].

### Prefer solution domain and problem domain terms

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#prefer-solution-domain-and-problem-domain-terms)

Search for good names in the solution domain, i.e. computer science terms such as "queue" or "tree",
and in the problem domain, i.e. business field terms such as "account" or "ledger".

Layers that are business-like will sound best when named according to the problem domain.
This is especially true for components that are design with Domain-Driven Design, such as APIs and business objects.

Layers that provide mostly technical functionality, such as factory classes and abstract algorithm,
will sound best when named according to the solution domain.

In any case, do not attempt to make up your own language.
We need to be able to exchange information between developers, product owners, partners and customers,
so choose names that all of these can relate to without a customized dictionary.

> Read more in _Chapter 2: Meaningful Names: Use Solution Domain Names_ and _[...]:
> Use Problem Domain Names_ of [Robert C. Martin's _Clean Code_].

### Use plural

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#use-plural)

There is a legacy practice at SAP to name tables of things in singular,
for example `country` for a "table of countries".
Common tendency in the outside world is to use the plural for lists of things.
We therefore recommend to prefer `countries` instead.

> Read more in _Chapter 2: Meaningful Names: Use Intention-Revealing Names_ of [Robert C. Martin's _Clean Code_].

### Use pronounceable names

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#use-pronounceable-names)

We think and talk a lot about objects, so use names that you can pronounce,
for example prefer `detection_object_types` to something cryptic like `dobjt`.

> Read more in _Chapter 2: Meaningful Names: Use Pronounceable Names_ of [Robert C. Martin's _Clean Code_]

### Avoid abbreviations

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#avoid-abbreviations)

If you have enough space, write out names in full.
Start abbreviating only if you exceed length limitations.

If you do have to abbreviate, start with the _unimportant_ words.

Abbreviating things may appear efficient at first glance, but becomes ambiguous very fast.
For example, does the "cust" in `cust` mean "customizing", "customer", or "custom"?
All three are common in SAP applications.

> Read more in _Chapter 2: Meaningful Names: Make Meaningful Distinctions_ of [Robert C. Martin's _Clean Code_].

### Use same abbreviations everywhere

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#use-same-abbreviations-everywhere)

People will search for keywords to find relevant code.
Support this by using the same abbreviation for the same thing.
For example, always abbreviate "detection object type" to "dobjt"
instead of mixing "dot", "dotype", "detobjtype" and so on.

> Read more in _Chapter 2: Meaningful Names: Use Searchable Names_ of [Robert C. Martin's _Clean Code_].

### Use nouns for classes and verbs for methods

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#use-nouns-for-classes-and-verbs-for-methods)

Use nouns or noun phrases to name classes, interfaces, and objects:

```ABAP
CLASS /clean/account
CLASS /clean/user_preferences
INTERFACE /clean/customizing_reader
```

Use verbs or verb phrases to name methods:

```ABAP
METHODS withdraw
METHODS add_message
METHODS read_entries
```

Starting Boolean methods with verbs like `is_` and `has_` yields nice reading flow:

```ABAP
IF is_empty( table ).
```

We recommend naming functions like methods:

```ABAP
FUNCTION /clean/read_alerts
```

### Avoid noise words such as "data", "info", "object"

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#avoid-noise-words-such-as-data-info-object)

Omit noise words

    account  " instead of account_data
    alert    " instead of alert_object

or replace them with something specific that really adds value

    user_preferences          " instead of user_info
    response_time_in_seconds  " instead of response_time_variable

> Read more in _Chapter 2: Meaningful Names: Make Meaningful Distinctions_ of [Robert C. Martin's _Clean Code_]

### Pick one word per concept

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#pick-one-word-per-concept)

```ABAP
METHODS read_this.
METHODS read_that.
METHODS read_those.
```

Choose a term for a concept and stick to it; don't mix in other synonyms.
Synonyms will make the reader waste time on finding a difference that's not there.

```ABAP
" anti-pattern
METHODS read_this.
METHODS retrieve_that.
METHODS query_those.
```

> Read more in _Chapter 2: Meaningful Names: Pick One Word per Concept_ of [Robert C. Martin's _Clean Code_]

### Use pattern names only if you mean them

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#use-pattern-names-only-if-you-mean-them)

Don't use the names of software design patterns for classes and interfaces unless you really mean them.
For example, don't call your class `file_factory` unless it really implements the factory design pattern.
The most common patterns include:
[singleton](https://en.wikipedia.org/wiki/Singleton_pattern),
[factory](https://en.wikipedia.org/wiki/Factory_method_pattern),
[facade](https://en.wikipedia.org/wiki/Facade_pattern),
[composite](https://en.wikipedia.org/wiki/Composite_pattern),
[decorator](https://en.wikipedia.org/wiki/Decorator_pattern),
[iterator](https://en.wikipedia.org/wiki/Iterator_pattern),
[observer](https://en.wikipedia.org/wiki/Observer_pattern), and
[strategy](https://en.wikipedia.org/wiki/Strategy_pattern).

> Read more in _Chapter 2: Meaningful Names: Avoid Disinformation_ of [Robert C. Martin's _Clean Code_]

### Avoid encodings, esp. Hungarian notation and prefixes

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [This section](#avoid-encodings-esp-hungarian-notation-and-prefixes)

We encourage you to get rid of _all_ encoding prefixes.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

instead of the needlessly longer

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> Read more in _Chapter 2: Meaningful Names: Avoid Encodings_ of [Robert C. Martin's _Clean Code_].
> The examples in this document are written without prefixes to demonstrate the value.

#### Reasoning

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [Avoid encodings, esp. Hungarian notation and prefixes](#avoid-encodings-esp-hungarian-notation-and-prefixes) > [This section](#reasoning)

SAP has a bad, pervasive legacy practice of adding prefixes to each and everything, to encode things like

- kind, such as "cl_" for classes,
- direction, such as "is_" for an input parameter,
- scope, such as "mo_" for a class member,
- type, such as "lt_" for a table-like variable, and
- mutability, such as "sc_" for a constant.

This kind of prefixing is a relic from the early days of programming, when code was printed out and read on paper,
and you didn't want to flip around just to find some variable's type.
Modern development environments give easy access to data types, signatures, and object navigation,
such that it is no longer needed to get readable code.

#### Arguments

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [Avoid encodings, esp. Hungarian notation and prefixes](#avoid-encodings-esp-hungarian-notation-and-prefixes) > [This section](#arguments)

Before you disagree, consider these:

- ABAP's 30 character limitation makes it hard enough to squeeze meaningful names
  into the available space without wasting another 3-4 characters for needless encodings.
- The disputes that arise over prefixes are not worth the effort:
  whether your constant starts with `sc_` or `cv_` does not really influence readability.
- Different team styles create confusion: is "lr_" an object reference or a range table?
  You'll stumble over this in code that connects different things, for example your determinations within BOPF.
- Changes create needless work: turning a table from `STANDARD` to `SORTED` shouldn't require you
  to rename all variables from "lt_" to "lts_".
- Prefixing doesn't make it easier to tell global from local things.
  If you fill a `gt_sum` from an `lt_sum`, both are still only sums and it's not clear what distinguishes the two.
  The better idea is to fill a `total_sum` from a `partial_sum`, or an `overall_result` from an `intermediate_result`.
  The name confusion described in
  [section _Program-Internal Names_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenprog_intern_names_guidl.htm)
  should thus be solved otherwise.
- If you follow Clean Code, your methods will become so short (3-5 statements)
  that prefixing is no longer necessary to tell importing from exporting parameters and local from global variables.
- The ABAP foundation doesn't prefix anymore, for example you won't find importing/exporting prefixes
  on the method parameters in `cl_abap_math`.
- Other languages like Java use absolutely no prefixes, and still Java code is perfectly readable.

#### Compromises

> [Clean ABAP](#clean-abap) > [Content](#content) > [Names](#names) > [Avoid encodings, esp. Hungarian notation and prefixes](#avoid-encodings-esp-hungarian-notation-and-prefixes) > [This section](#compromises)

There is only one prefix that ABAP forces you to use: your application's namespace,
to avoid conflicts with objects from other teams in the global dictionary, where every thing needs a unique name.

If this rule is too hard for you, consider a compromise:
avoid encodings in local contexts (within a method body, method parameters, local classes, etc.),
and apply them only to global objects that are stored in the same global Dictionary namespace.

We agree that following this suggestion will work out only if the code is already _clean_ in some other aspects,
especially short methods and good method and variable names.
While prefixes needlessly complicate a clean method with two statements,
they may be your only remaining lifeline in a thousand-line legacy function with cryptic variable names.

> Read more in _Chapter 2: Meaningful Names: Avoid Encodings_ of [Robert C. Martin's _Clean Code_].
> This section contradicts the sections [_Names of Repository Objects_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)
> and [_Program-Internal Names_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenexit_procedure_guidl.htm)
> of the ABAP Programming Guidelines which recommend to use prefixes.
> We think that avoiding prefixes is the more modern and readable variant and that the guideline should be adjusted.

## Language

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#language)

### Mind the legacy

> [Clean ABAP](#clean-abap) > [Content](#content) > [Language](#language) > [This section](#mind-the-legacy)

If you code for older ABAP releases, take the advice in this guide with care:
Many recommendations below make use of relatively new syntax and constructs
that may not be supported in older ABAP releases.
Validate the guidelines you want to follow on the oldest release you must support.
Do not simply discard Clean Code as a whole -
the vast majority of rules (e.g. naming, commenting) will work in _any_ ABAP version.

### Mind the performance

> [Clean ABAP](#clean-abap) > [Content](#content) > [Language](#language) > [This section](#mind-the-performance)

If you code high performance components, take the advice in this guide with care:
Some aspects of Clean Code may make things slower (more method calls) or consume more memory (more objects).
ABAP has some specialties that may intensify this, for example it compares data types when calling a method,
such that splitting a single large method into many sub-methods may make the code slower.

However, we strongly recommend to not optimize prematurely, based on obscure fears.
The vast majority of rules (e.g. naming, commenting) has no negative impact at all.
Try to build things in a clean, object-oriented way.
If something is too slow, make a performance measurement.
Only then should you take a fact-based decision to discard selected rules.

### Prefer object orientation over imperative programming

> [Clean ABAP](#clean-abap) > [Content](#content) > [Language](#language) > [This section](#prefer-object-orientation-over-imperative-programming)

Object-oriented programs (classes, interfaces) are segmented better
and can be refactored and tested more easily than imperative code (functions, programs).
Although there are situations where you must provide imperative objects
(a function for an RFC, a program for a transaction),
these objects should do little more than call a corresponding class that provides the actual feature:

```ABAP
FUNCTION check_business_partner [...].
  DATA(validator) = NEW /clean/biz_partner_validator( ).
  result = validator->validate( business_partners ).
ENDFUNCTION.
```

#### Function Groups vs. Classes

> [Clean ABAP](#clean-abap) > [Content](#content) > [Language](#language) > [Prefer object orientation over imperative programming](#prefer-object-orientation-over-imperative-programming) > [This section](#function-groups-vs-classes)

New clean coders routinely ask for clarification,
especially what the advantage of classes over function groups should be.

Think of a function group as a `global abstract final class` with functions as `static public` members
and form routines and global variables as `static private` members -
with all positive and negative aspects this entails:

- No instantiation.
You cannot create multiple instances of the same function group.
Makes it hard to prevent undesired sideways access to value-carrying fields.

- No inheritance.
You cannot inherit from or let inherit function groups.
Prevents implementing some design patterns,
such as [Composite](https://en.wikipedia.org/wiki/Composite_pattern).

- No interfaces.
You cannot provide two implementations for the same function group.
Prevents mocking function calls in unit tests without dedicated techniques such as test seams.

- No substitution.
You cannot exchange a call to one function with a call to another one with different name but identical signature.
Makes it hard to implement design patterns that overwrite methods,
such as [Decorator](https://en.wikipedia.org/wiki/Decorator_pattern).

- No overloading.
You cannot provide two functions with identical names but different parameters.
(This is not possible in ABAP OO too, by the way.)

- Variable encapsulation.
Function groups can hide internal state in private variables.
Classes can do this a little better because they hide value on instance level with objects. 

- Method encapsulation.
Function groups can hide internal methods ("form routines");
they are equal to classes in this respect.

> Originally [answered on StackOverflow](https://stackoverflow.com/questions/55243044/function-groups-vs-classes/55244019#55244019).

### Prefer functional over procedural language constructs

> [Clean ABAP](#clean-abap) > [Content](#content) > [Language](#language) > [This section](#prefer-functional-over-procedural-language-constructs)

They are usually shorter and come more natural to modern programmers.

```ABAP
DATA(variable) = 'A'.
" MOVE 'A' TO variable.

DATA(uppercase) = to_upper( lowercase ).
" TRANSLATE lowercase TO UPPER CASE.

index += 1.         " >= NW 7.54
index = index + 1.  " < NW 7.54
" ADD 1 TO index.

DATA(object) = NEW /clean/my_class( ).
" CREATE OBJECT object TYPE /dirty/my_class.

result = VALUE #( FOR row IN input ( row-text ) ).
" LOOP AT input INTO DATA(row).
"  INSERT row-text INTO TABLE result.
" ENDLOOP.

DATA(line) = value_pairs[ name = 'A' ].
" READ TABLE value_pairs INTO DATA(line) WITH KEY name = 'A'.

DATA(exists) = xsdbool( line_exists( value_pairs[ name = 'A' ] ) ).
IF line_exists( value_pairs[ name = 'A' ] ).
" READ TABLE value_pairs TRANSPORTING NO FIELDS WITH KEY name = 'A'.
" DATA(exists) = xsdbool( sy-subrc = 0 ).
```

Many of the detailed rules below are just specific reiterations of this general advice.

### Use design patterns wisely

> [Clean ABAP](#clean-abap) > [Content](#content) > [Language](#language) > [This section](#use-design-patterns-wisely)

Where they are appropriate and provide noticeable benefit.
Don't apply design patterns everywhere just for the sake of it.

## Constants

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#constants)

### Use constants instead of magic numbers

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [This section](#use-constants-instead-of-magic-numbers)

```ABAP
IF abap_type = cl_abap_typedescr=>typekind_date.
```

is clearer than

```ABAP
" anti-pattern
IF abap_type = 'D'.
```

> Read more in _Chapter 17: Smells and Heuristics: G25:
> Replace Magic Numbers with Named Constants_ of [Robert C. Martin's _Clean Code_].

### Prefer enumeration classes over constants interfaces

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [This section](#prefer-enumeration-classes-over-constants-interfaces)

There are two common patterns that reflect enumerations.

#### Enumerations: Constant Pattern

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [Prefer enumeration classes over constants interfaces](#prefer-enumeration-classes-over-constants-interfaces) > [This section](#enumerations-constant-pattern)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.

CLASS /clean/message_severity IMPLEMENTATION.
ENDCLASS.
```

```ABAP
IF log_contains( /clean/message_severity=>warning ).
```

#### Enumerations: Object Pattern

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [Prefer enumeration classes over constants interfaces](#prefer-enumeration-classes-over-constants-interfaces) > [This section](#enumerations-object-pattern)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.
  PUBLIC SECTION.
    CLASS-DATA:
      warning TYPE REF TO /clean/message_severity READ-ONLY,
      error   TYPE REF TO /clean/message_severity READ-ONLY.
    CLASS-METHODS class_constructor.
    METHODS constructor IMPORTING value TYPE /clean/severity.
    METHODS equals
      IMPORTING
        value         TYPE symsgty
      RETURNING
        VALUE(result) TYPE abap_bool.
    DATA value TYPE symsgty READ-ONLY.
ENDCLASS.

CLASS /clean/message_severity IMPLEMENTATION.

  METHOD class_constructor.
    warning = NEW /clean/message_severity( 'W' ).
    error = NEW /clean/message_severity( 'E' ).
  ENDMETHOD.

  METHOD constructor.
    me->value = value.
  ENDMETHOD.

  METHOD equals.
    result = xsdbool( value = me->value ).
  ENDMETHOD.

ENDCLASS.
```

```ABAP
METHODS to_string IMPORTING severity TYPE REF TO /clean/message_severity.
IF /clean/message_severity=>warning->equals( worst_severity ).
object-severity = /clean/message_severity=>warning->value.
```

#### Enumerations: Anti-Pattern

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [Prefer enumeration classes over constants interfaces](#prefer-enumeration-classes-over-constants-interfaces) > [This section](#enumerations-anti-pattern)

```ABAP
" anti-pattern
INTERFACE /dirty/common_constants.
  CONSTANTS:
    warning      TYPE symsgty VALUE 'W',
    transitional TYPE i       VALUE 1,
    error        TYPE symsgty VALUE 'E',
    persisted    TYPE i       VALUE 2.
ENDINTERFACE.
```

#### Enumerations: Benefits

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [Prefer enumeration classes over constants interfaces](#prefer-enumeration-classes-over-constants-interfaces) > [This section](#enumerations-benefits)

- An enumeration class groups all possible values for a data type at one glance.
- In contrast to the ABAP statement `ENUM`, these constants can be used on all levels,
including data exchange with the database.
- Enumeration classes improve cohesion because you depend only on those constants that you need,
not everything in a large interface.
- Enumeration classes can be found by their name via the data dictionary,
using _Display other object..._ in SE24 and SE80 or _Ctrl+Shift+A_ in ADT.
- The `FINAL` and `ABSTRACT` prevents people from "inheriting" or "implementing" the constants list,
which would sacrifice cohesion for a slightly shorter syntax.
- You can add type-related methods such as conversions, validation, etc. to the enumeration class.
- You can add unit tests to the enumeration,
for example to assert that it's still in sync with the fixed values of its underlying DDIC Domain.
- The object-oriented pattern comes with value-safety,
meaning it is not possible to provide a value that's not contained in the enumeration.

> Read more in _Chapter 17: Smells and Heuristics: J3: Constants versus Enums_ of [Robert C. Martin's _Clean Code_].

### If you don't use enumeration classes, group your constants

> [Clean ABAP](#clean-abap) > [Content](#content) > [Constants](#constants) > [This section](#if-you-dont-use-enumeration-classes-group-your-constants)

If you collect constants in a loose way, for example in an interface, group them:

```ABAP
CONSTANTS:
  BEGIN OF message_severity,
    warning TYPE symsgty VALUE 'W',
    error   TYPE symsgty VALUE 'E',
  END OF message_severity,
  BEGIN OF message_lifespan,
    transitional TYPE i VALUE 1,
    persisted    TYPE i VALUE 2,
  END OF message_lifespan.
```

Makes the relation clearer than:

```ABAP
CONSTANTS:
  warning      TYPE symsgty VALUE 'W',
  transitional TYPE i       VALUE 1,
  error        TYPE symsgty VALUE 'E',
  persisted    TYPE i       VALUE 2,
```

The group also allows you group-wise access, for example for input validation:

```ABAP
DO number_of_constants TIMES.
  ASSIGN COMPONENT sy-index OF STRUCTURE message_severity TO FIELD-SYMBOL(<constant>).
  IF <constant> = input.
    is_valid = abap_true.
    RETURN.
  ENDIF.
ENDWHILE.
```

> Read more in _Chapter 17: Smells and Heuristics: G27: Structure over Convention_ of [Robert C. Martin's _Clean Code_].

## Variables

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#variables)

### Prefer inline over up-front declarations

> [Clean ABAP](#clean-abap) > [Content](#content) > [Variables](#variables) > [This section](#prefer-inline-over-up-front-declarations)

If you follow these guidelines, your methods will become so short (3-5 statements)
that declaring variables inline at first occurrence will look more natural

```ABAP
METHOD do_something.
  DATA(name) = 'something'.
  DATA(reader) = /clean/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

than declaring variables with a separate `DATA` section at the beginning of the method

```ABAP
" anti-pattern
METHOD do_something.
  DATA:
    name   TYPE seoclsname,
    reader TYPE REF TO /dirty/reader.
  name = 'something'.
  reader = /dirty/reader=>get_instance_for( name ).
  result = reader->read_it( ).
ENDMETHOD.
```

> Read more in _Chapter 5: Formatting: Vertical Distance: Variable Declarations_ of [Robert C. Martin's _Clean Code_].

### Don't declare inline in optional branches

> [Clean ABAP](#clean-abap) > [Content](#content) > [Variables](#variables) > [This section](#dont-declare-inline-in-optional-branches)

```ABAP
" anti-pattern
IF has_entries = abap_true.
  DATA(value) = 1.
ELSE.
  value = 2.
ENDIF.
```

This works fine because ABAP handles inline declarations as if they were at the beginning of the method.
However, it is extremely confusing for readers,
especially if the method is longer and you don't spot the declaration right away.
In this case, break with inlining and put the declaration up-front:

```ABAP
DATA value TYPE i.
IF has_entries = abap_true.
  value = 1.
ELSE.
  value = 2.
ENDIF.
```

> Read more in _Chapter 5: Formatting: Vertical Distance: Variable Declarations_ of [Robert C. Martin's _Clean Code_].

### Do not chain up-front declarations

> [Clean ABAP](#clean-abap) > [Content](#content) > [Variables](#variables) > [This section](#do-not-chain-up-front-declarations)

```ABAP
DATA name TYPE seoclsname
DATA reader TYPE REF TO /dirty/reader.
```

Chaining suggests the defined variables are related on a logical level.
To consistently use it, you would have to ensure that all chained variables belong together,
and introduce additional chain groups to add variables.
While this is possible, it is usually not worth the effort.

Chaining also needlessly complicates reformatting and refactoring
because each line looks different and changing them requires meddling with
colons, dots, and commas, that are not worth the effort.

```ABAP
" anti-pattern
DATA:
  name   TYPE seoclsname,
  reader TYPE REF TO /dirty/reader.
```

> Also refer to [Don't align type clauses](#dont-align-type-clauses)

### Prefer REF TO over FIELD-SYMBOL

> [Clean ABAP](#clean-abap) > [Content](#content) > [Variables](#variables) > [This section](#prefer-ref-to-over-field-symbol)

```ABAP
LOOP AT components REFERENCE INTO DATA(component).
```

instead of the equivalent

```ABAP
" anti-pattern
LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).
```

except where you need field symbols

```ABAP
ASSIGN generic->* TO FIELD-SYMBOL(<generic>).
ASSIGN COMPONENT name OF STRUCTURE structure TO FIELD-SYMBOL(<component>).
ASSIGN (class_name)=>(static_member) TO FIELD-SYMBOL(<member>).
```

Code reviews demonstrate that people tend to choose between the two arbitrarily,
"just because", "because we are always LOOPing that way", or "for no special reason".
Arbitrary choices make the reader waste time on the pointless question why one is used over the other
and thus should be replaced with well-founded, precise decisions.
Our recommendation is based on this reasoning:

- Field symbols can do some things that references cannot, such as dynamically accessing the components of a structure.
Likewise, references can do things that field symbols can't, such as constructing a dynamically typed data structure.
In summary, settling for one alone is not possible.

- In object-oriented ABAP, references are all over the place and cannot be avoided,
as any object is a `REF TO <class-name>`.
In contrast, field symbols are only strictly required in few, special cases concerned with dynamic typing.
References thus form a natural preference in any object-oriented program.

- Field symbols are shorter than references, but the resulting memory saving is so tiny that it can be safely neglected.
Similarly, speed is not an issue. As a consequence, there is no performance-related reason to prefer one over the other.

> Read more in the article
> [_Accessing Data Objects Dynamically_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abendyn_access_data_obj_guidl.htm).

## Tables

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#tables)

### Use the right table type

> [Clean ABAP](#clean-abap) > [Content](#content) > [Tables](#tables) > [This section](#use-the-right-table-type)

- You typically use `HASHED` tables for **large tables**
that are **filled in a single step**, **never modified**, and **read often by their key**.
Their inherent memory and processing overhead makes hash tables only valuable
for large amounts of data and lots of of read accesses.
Each change to the table's content requires expensive recalculation of the hash,
so don't use this for tables that are modified too often.

- You typically use `SORTED` tables for **large tables**
that need to be **sorted at all times**, that are **filled bit by bit** or **need to be modified**,
and **read often by one or more full or partial keys** or processed **in a certain order**.
Adding, changing, or removing content requires finding the right insertion spot,
but doesn't require adjusting the rest of the table's index.
Sorted tables demonstrate their value only for large numbers of read accesses.

- Use `STANDARD` tables for **small tables**, where indexing produces more overhead than benefit,
and **"arrays"**, where you either don't care at all for the order of the rows,
or you want to process them in exactly the order they were appended.

> These are only rough guidelines.
> Find more details in the article [_Selection of Table Category_ in the ABAP Language Help](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenitab_kind.htm).

### Avoid DEFAULT KEY

> [Clean ABAP](#clean-abap) > [Content](#content) > [Tables](#tables) > [This section](#avoid-default-key)

```ABAP
" anti-pattern
DATA itab TYPE STANDARD TABLE OF row_type WITH DEFAULT KEY.
```

Default keys are often only added to get the newer functional statements working.
The keys themselves in fact are usually superfluous and waste resources for nothing.
They can even lead to obscure mistakes because they ignore numeric data types.

Either specify the key components explicitly

```ABAP
DATA itab2 TYPE STANDARD TABLE OF row_type WITH NON-UNIQUE KEY comp1 comp2.
```

or resort to `EMPTY KEY` if you don't need a key at all.

```ABAP
DATA itab1 TYPE STANDARD TABLE OF row_type WITH EMPTY KEY.
```

> Following [Horst Keller's blog on _Internal Tables with Empty Key_](https://blogs.sap.com/2013/06/27/abap-news-for-release-740-internal-tables-with-empty-key/)

### Prefer INSERT INTO TABLE over APPEND TO

> [Clean ABAP](#clean-abap) > [Content](#content) > [Tables](#tables) > [This section](#prefer-insert-into-table-over-append-to)

```ABAP
INSERT VALUE #( ... ) INTO TABLE itab.
```

`INSERT INTO TABLE` works with all table and key types,
thus making it easier for you to refactor the table's type and key definitions if your performance requirements change.

Use `APPEND TO` only if you use a `STANDARD` table in an array-like fashion,
if you want to stress that the added entry shall be the last row.

### Prefer LINE_EXISTS over READ TABLE or LOOP AT

> [Clean ABAP](#clean-abap) > [Content](#content) > [Tables](#tables) > [This section](#prefer-line_exists-over-read-table-or-loop-at)

```ABAP
IF line_exists( my_table[ key = 'A' ] ).
```

expresses the intent clearer and shorter than

```ABAP
" anti-pattern
READ TABLE my_table TRANSPORTING NO FIELDS WITH KEY key = 'A'.
IF sy-subrc = 0.
```

or even

```ABAP
" anti-pattern
LOOP AT my_table ASSIGNING FIELD-SYMBOL(<line>) WHERE key = 'A'.
  line_exists = abap_true.
  EXIT.
ENDLOOP.
```

### Prefer READ TABLE over LOOP AT

> [Clean ABAP](#clean-abap) > [Content](#content) > [Tables](#tables) > [This section](#prefer-read-table-over-loop-at)

```ABAP
READ TABLE my_table ASSIGNING FIELD-SYMBOL(<line>) WITH KEY key = 'A'.
```

expresses the intent clearer and shorter than

```ABAP
" anti-pattern
LOOP AT my_table ASSIGNING FIELD-SYMBOL(<line>) WHERE key = 'A'.
  EXIT.
ENDLOOP.
```

or even

```ABAP
" anti-pattern
LOOP AT my_table ASSIGNING FIELD-SYMBOL(<line>).
  IF <line>-key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```

### Prefer LOOP AT WHERE over nested IF

> [Clean ABAP](#clean-abap) > [Content](#content) > [Tables](#tables) > [This section](#prefer-loop-at-where-over-nested-if)

```ABAP
LOOP AT my_table ASSIGNING FIELD-SYMBOL(<line>) WHERE key = 'A'.
``` 

expresses the intent clearer and shorter than

```ABAP
LOOP AT my_table ASSIGNING FIELD-SYMBOL(<line>).
  IF <line>-key = 'A'.
    EXIT.
  ENDIF.
ENDLOOP.
```