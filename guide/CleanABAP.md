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
- [Strings](#strings)
  - [Use ` to define literals](#use--to-define-literals)
  - [Use | to assemble text](#use--to-assemble-text)
- [Booleans](#booleans)
  - [Use ABAP_BOOL for Booleans](#use-abap_bool-for-booleans)
  - [Use ABAP_TRUE and ABAP_FALSE for comparisons](#use-abap_true-and-abap_false-for-comparisons)
  - [Use XSDBOOL to set Boolean variables](#use-xsdbool-to-set-boolean-variables)
- [Conditions](#conditions)
  - [Try to make conditions positive](#try-to-make-conditions-positive)
  - [Consider decomposing complex conditions](#consider-decomposing-complex-conditions)
  - [Consider extracting complex conditions](#consider-extracting-complex-conditions)
- [Ifs](#ifs)
  - [No empty IF branches](#no-empty-if-branches)
  - [Prefer CASE to ELSE IF for multiple alternative conditions](#prefer-case-to-else-if-for-multiple-alternative-conditions)
  - [Keep the nesting depth low](#keep-the-nesting-depth-low)
- [Regular expressions](#regular-expressions)
  - [Prefer simpler methods to regular expressions](#prefer-simpler-methods-to-regular-expressions)
  - [Prefer basis checks to regular expressions](#prefer-basis-checks-to-regular-expressions)
  - [Consider assembling complex regular expressions](#consider-assembling-complex-regular-expressions)
- [Classes](#classes)
  - [Classes: Object orientation](#classes-object-orientation)
    - [Prefer objects to static classes](#prefer-objects-to-static-classes)
    - [Prefer composition over inheritance](#prefer-composition-over-inheritance)
    - [Don't mix stateful and stateless in the same class](#dont-mix-stateful-and-stateless-in-the-same-class)
  - [Scope](#scope)
    - [Global by default, local only in exceptional cases](#global-by-default-local-only-in-exceptional-cases)
    - [FINAL if not designed for inheritance](#final-if-not-designed-for-inheritance)
    - [Members PRIVATE by default, PROTECTED only if needed](#members-private-by-default-protected-only-if-needed)
    - [Consider using immutable instead of getter](#consider-using-immutable-instead-of-getter)
    - [Use READ-ONLY sparingly](#use-read-only-sparingly)
  - [Constructors](#constructors)
    - [Prefer NEW over CREATE OBJECT](#prefer-new-over-create-object)
    - [If your global class is CREATE PRIVATE, leave the CONSTRUCTOR public](#if-your-global-class-is-create-private-leave-the-constructor-public)
    - [Prefer multiple static factory methods over optional parameters](#prefer-multiple-static-factory-methods-over-optional-parameters)
    - [Use descriptive names for multiple constructor methods](#use-descriptive-names-for-multiple-constructor-methods)
    - [Make singletons only where multiple instances don't make sense](#make-singletons-only-where-multiple-instances-dont-make-sense)
- [Methods](#methods)
  - [Calls](#calls)
    - [Prefer functional over procedural calls](#prefer-functional-over-procedural-calls)
    - [Omit RECEIVING](#omit-receiving)
    - [Omit the optional keyword EXPORTING](#omit-the-optional-keyword-exporting)
    - [Omit the parameter name in single parameter calls](#omit-the-parameter-name-in-single-parameter-calls)
  - [Methods: Object orientation](#methods-object-orientation)
    - [Prefer instance to static methods](#prefer-instance-to-static-methods)
    - [Public instance methods should be part of an interface](#public-instance-methods-should-be-part-of-an-interface)
  - [Parameter Number](#parameter-number)
    - [Aim for few IMPORTING parameters, at best less than three](#aim-for-few-importing-parameters-at-best-less-than-three)
    - [Split methods instead of adding OPTIONAL parameters](#split-methods-instead-of-adding-optional-parameters)
    - [Use PREFERRED PARAMETER sparingly](#use-preferred-parameter-sparingly)
    - [RETURN, EXPORT, or CHANGE exactly one parameter](#return-export-or-change-exactly-one-parameter)
    
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

## Strings

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#strings)

### Use ` to define literals

> [Clean ABAP](#clean-abap) > [Content](#content) > [Strings](#strings) > [This section](#use--to-define-literals)

```ABAP
CONSTANTS some_constant TYPE string VALUE `ABC`.
DATA(some_string) = `ABC`.  " --> TYPE string
```

Refrain from using `'`, as it adds a superfluous type conversion and confuses the reader
whether he's dealing with a `CHAR` or `STRING`:

```ABAP
" anti-pattern
DATA some_string TYPE string.
some_string = 'ABC'.
```

`|` is generally okay, but cannot be used for `CONSTANTS` and adds needless overhead when specifying a fixed value:

```ABAP
" anti-pattern
DATA(some_string) = |ABC|.
```

### Use | to assemble text

> [Clean ABAP](#clean-abap) > [Content](#content) > [Strings](#strings) > [This section](#use--to-assemble-text)

```ABAP
DATA(message) = |Received HTTP code { status_code } with message { text }|.
```

String templates highlight better what's literal and what's variable,
especially if you embed multiple variables in a text.

```ABAP
" anti-pattern
DATA(message) = `Received an unexpected HTTP ` && status_code && ` with message ` && text.
```

## Booleans

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#booleans)

### Use ABAP_BOOL for Booleans

> [Clean ABAP](#clean-abap) > [Content](#content) > [Booleans](#booleans) > [This section](#use-abap_bool-for-booleans)

```ABAP
DATA has_entries TYPE abap_bool.
```

Don't use the generic type `char1`.
Although it is technically compatible it obscures the fact that we're dealing with a Boolean variable.

Also avoid other Boolean types as they often have strange side effects,
for example `boolean` supports a third value "undefined" that results in subtle programming errors.

In some cases you may need a data dictionary element, for example for DynPro fields.
`abap_bool` cannot be used here because it is defined in the type pool `abap`, not in the data dictionary.
In this case, resort to `boole_d` or `xfeld`.
Create your own data element if you need a custom description.

> ABAP may be the one single programming language that does not come with a universal Boolean data type.
> However, having one is imperative.
> This recommendation is based on the ABAP Programming Guidelines.

### Use ABAP_TRUE and ABAP_FALSE for comparisons

> [Clean ABAP](#clean-abap) > [Content](#content) > [Booleans](#booleans) > [This section](#use-abap_true-and-abap_false-for-comparisons)

```ABAP
has_entries = abap_true.
IF has_entries = abap_false.
```

Don't use the character equivalents `'X'` and `' '` or `space`;
they make it hard to see that this is a Boolean expression:

```ABAP
" anti-pattern
has_entries = 'X'.
IF has_entries = space.
```

Avoid comparisons with `INITIAL` - it forces readers to recollect that `abap_bool`'s default is `abap_false`:

```ABAP
" anti-pattern
IF has_entries IS NOT INITIAL.
```

> ABAP may be the one single programming language that does not come with built-in "constants" for true and false.
> However, having them is imperative.
> This recommendation is based on the ABAP Programming Guidelines.

### Use XSDBOOL to set Boolean variables

> [Clean ABAP](#clean-abap) > [Content](#content) > [Booleans](#booleans) > [This section](#use-xsdbool-to-set-boolean-variables)

```ABAP
DATA(has_entries) = xsdbool( line IS NOT INITIAL ).
```

The equivalent `IF`-`THEN`-`ELSE` is much longer for nothing:

```ABAP
" anti-pattern
IF line IS INITIAL.
  has_entries = abap_false.
ELSE.
  has_entries = abap_true.
ENDIF.
```

`xsdbool` is the best method for our purpose, as it directly produces a `char1`,
which fits our boolean type `abap_bool` best.
The equivalent functions `boolc` and `boolx` produce different types
and add an unnecessary implicit type conversion.

We agree that the name `xsdbool` is unlucky and misleading;
after all, we're not at all interested in the "XML Schema Definition" parts that the "xsd" prefix suggests.

A possible alternative to `xsdbool` is the `COND` ternary form.
Its syntax is intuitive, but a little longer because it needlessly repeats the `THEN abap_true` segment,
and requires knowledge of the implicit default value `abap_false` -
which is why we suggest it only as secondary solution.

```ABAP
DATA(has_entries) = COND abap_bool( WHEN line IS NOT INITIAL THEN abap_true ).
```

## Conditions

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#conditions)

### Try to make conditions positive

> [Clean ABAP](#clean-abap) > [Content](#content) > [Conditions](#conditions) > [This section](#try-to-make-conditions-positive)

```ABAP
IF has_entries = abap_true.
```

For comparison, see how hard to understand the same statement gets by reversing it:

```ABAP
" anti-pattern
IF has_no_entries = abap_false.
```

The "try" in the section title means you shouldn't force this
up to the point where you end up with something like [empty IF branches](#no-empty-if-branches):

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
  " only do something in the ELSE block, IF remains empty
ENDIF.
```

> Read more in _Chapter 17: Smells and Heuristics: G29: Avoid Negative Conditionals_ of [Robert C. Martin's _Clean Code_].

### Consider decomposing complex conditions

> [Clean ABAP](#clean-abap) > [Content](#content) > [Conditions](#conditions) > [This section](#consider-decomposing-complex-conditions)

Conditions can become easier when decomposing them into the elementary parts that make them up:

```ABAP
DATA(example_provided) = xsdbool( example_a IS NOT INITIAL OR
                                  example_b IS NOT INITIAL ).

DATA(one_example_fits) = xsdbool( applies( example_a ) = abap_true OR
                                  applies( example_b ) = abap_true OR
                                  fits( example_b ) = abap_true ).

IF example_provided = abap_true AND
   one_example_fits = abap_true.
```

instead of leaving everything in-place:

```ABAP
" anti-pattern
IF ( example_a IS NOT INITIAL OR
     example_b IS NOT INITIAL ) AND
   ( applies( example_a ) = abap_true OR
     applies( example_b ) = abap_true OR
     fits( example_b ) = abap_true ).
```

> Use the ABAP Development Tools quick fixes to quickly extract conditions and create variables as shown above.

### Consider extracting complex conditions

> [Clean ABAP](#clean-abap) > [Content](#content) > [Conditions](#conditions) > [This section](#consider-extracting-complex-conditions)

It's nearly always a good idea to extract complex conditions to methods of their own:

```ABAP
IF is_provided( example ).

METHOD is_provided.
DATA(is_filled) = xsdbool( example IS NOT INITIAL ).
DATA(is_working) = xsdbool( applies( example ) = abap_true OR
                           fits( example ) = abap_true ).
result = xsdbool( is_filled = abap_true AND
                 is_working = abap_true ).
ENDMETHOD.
```

## Ifs

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#ifs)

### No empty IF branches

> [Clean ABAP](#clean-abap) > [Content](#content) > [Ifs](#ifs) > [This section](#no-empty-if-branches)

```ABAP
IF has_entries = abap_false.
" do some magic
ENDIF.
```

is shorter and clearer than

```ABAP
" anti-pattern
IF has_entries = abap_true.
ELSE.
" do some magic
ENDIF.
```

### Prefer CASE to ELSE IF for multiple alternative conditions

> [Clean ABAP](#clean-abap) > [Content](#content) > [Ifs](#ifs) > [This section](#prefer-case-to-else-if-for-multiple-alternative-conditions)

```ABAP
CASE type.
WHEN type-some_type.
 " ...
WHEN type-some_other_type.
 " ...
WHEN OTHERS.
 RAISE EXCEPTION NEW /clean/unknown_type_failure( ).
ENDCASE.
```

`CASE` makes it easy to see a set of alternatives that exclude each other.
It can be faster than a series of `IF`s because it can translate to a different microprocessor command
instead of a series of subsequently evaluated conditions.
You can introduce new cases quickly, without having to repeat the discerning variable over and over again.
The statement even prevents some errors that can occur when accidentally nesting the `IF`-`ELSEIF`s.

```ABAP
" anti-pattern
IF type = type-some_type.
" ...
ELSEIF type = type-some_other_type.
" ...
ELSE.
RAISE EXCEPTION NEW /dirty/unknown_type_failure( ).
ENDIF.
```

### Keep the nesting depth low

> [Clean ABAP](#clean-abap) > [Content](#content) > [Ifs](#ifs) > [This section](#keep-the-nesting-depth-low)

```ABAP
" ani-pattern
IF <this>.
IF <that>.
ENDIF.
ELSE.
IF <other>.
ELSE.
 IF <something>.
 ENDIF.
ENDIF.
ENDIF.
```

Nested `IF`s get hard to understand very quickly and require an exponential number of test cases for complete coverage.

Decision trees can usually be taken apart by forming sub-methods and introducing boolean helper variables.

Other cases can be simplified by merging IFs, such as

```ABAP
IF <this> AND <that>.
```

instead of the needlessly nested

```ABAP
" anti-pattern
IF <this>.
IF <that>.
```

## Regular expressions

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#regular-expressions)

### Prefer simpler methods to regular expressions

> [Clean ABAP](#clean-abap) > [Content](#content) > [Regular expressions](#regular-expressions) > [This section](#prefer-simpler-methods-to-regular-expressions)

```ABAP
IF input IS NOT INITIAL.
" IF matches( val = input  regex = '.+' ).

WHILE contains( val = input  sub = 'abc' ).
" WHILE contains( val = input  regex = 'abc' ).
```

Regular expressions become hard to understand very quickly.
Simple cases are usually easier without them.

Regular expressions also usually consume more memory and processing time
because they need to be parsed into an expression tree and compiled at runtime into an executable matcher.
Simple solutions may do with a straight-forward loop and a temporary variable.

### Prefer basis checks to regular expressions

> [Clean ABAP](#clean-abap) > [Content](#content) > [Regular expressions](#regular-expressions) > [This section](#prefer-basis-checks-to-regular-expressions)

```ABAP
CALL FUNCTION 'SEO_CLIF_CHECK_NAME'
EXPORTING
 cls_name = class_name
EXCEPTIONS
 ...
```

instead of reinventing things

```ABAP
" anti-pattern
DATA(is_valid) = matches( val     = class_name
                       pattern = '[A-Z][A-Z0-9_]{0,29}' ).
```

> There seems to be a natural tendency to turn blind to the Don't-Repeat-Yourself (DRY) principle
> when there are regular expressions around,
> compare section _Chapter 17: Smells and Heuristics: General: G5: Duplication_ in [Robert C. Martin's _Clean Code_].

### Consider assembling complex regular expressions

> [Clean ABAP](#clean-abap) > [Content](#content) > [Regular expressions](#regular-expressions) > [This section](#consider-assembling-complex-regular-expressions)

```ABAP
CONSTANTS class_names TYPE string VALUE `CL\_.*`.
CONSTANTS interface_names TYPE string VALUE `IF\_.*`.
DATA(object_names) = |{ class_names }|{ interface_names }|.
```

Some complex regular expressions become easier
when you demonstrate the reader how they are built up from more elementary pieces.

## Classes

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#classes)

### Classes: Object orientation

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [This section](#classes-object-orientation)

#### Prefer objects to static classes

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Classes: Object orientation](#classes-object-orientation) > [This section](#prefer-objects-to-static-classes)

Static classes give up all advantages gained by object orientation in the first place.
They especially make it nearly impossible to replace productive dependencies with test doubles in unit tests.

If you think about whether to make a class or method static, the answer will nearly always be: no.

One accepted exception to this rule are plain type utils classes.
Their methods make it easier to interact with certain ABAP types.
They are not only completely stateless, but so basic that they look like ABAP statements or built-in functions.
The discriminating factor is that their consumers tie them into their code so tightly
that they actually don't want to mock them in unit tests.

```ABAP
CLASS /clean/string_utils DEFINITION [...].
CLASS-METHODS trim
 IMPORTING
   string        TYPE string
 RETURNING
   VALUE(result) TYPE string.
ENDCLASS.

METHOD retrieve.
DATA(trimmed_name) = /clean/string_utils=>trim( name ).
result = read( trimmed_name ).
ENDMETHOD.
```

#### Prefer composition over inheritance

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Classes: Object orientation](#classes-object-orientation) > [This section](#prefer-composition-over-inheritance)
 
Avoid building hierarchies of classes with inheritance. Instead, favor composition.

Clean inheritance is hard to design because you need to respect rules
like the [Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle).
It is also hard to understand because people need to realize and digest the guiding principles behind the hierarchy.
Inheritance reduces reuse because methods tend to be made available only to sub-classes.
It also complicates refactoring because moving or changing members tend to require changes to the whole hierarchy tree.

Composition means that you design small, independent objects, each of which serves one specific purpose.
These objects can be recombined into more complex objects by simple delegation and facade patterns.
Composition may produce more classes, but has otherwise no further disadvantages.

Don't let this rule discourage you from using inheritance in the right places.
There are good applications for inheritance,
for example the [Composite design pattern](https://en.wikipedia.org/wiki/Composite_pattern).
Just ask yourself critically whether inheritance in your case will really provide more benefits than disadvantages.
If in doubt, composition generally is the safer choice.

#### Don't mix stateful and stateless in the same class

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Classes: Object orientation](#classes-object-orientation)

Don't mix the stateless and the stateful programming paradigms in the same class.

In stateless programming, methods get input and produce output, _without any side effects_,
resulting in methods that produce the same result no matter when and in what order they are called.

```ABAP
CLASS /clean/xml_converter DEFINITION PUBLIC FINAL CREATE PUBLIC.
PUBLIC SECTION.
 METHODS convert
   IMPORTING
     file_content  TYPE xstring
   RETURNING
     VALUE(result) TYPE /clean/some_inbound_message.
ENDCLASS.

CLASS /clean/xml_converter IMPLEMENTATION.
METHOD convert.
 cl_proxy_xml_transform=>xml_xstring_to_abap(
   EXPORTING
     xml       = file_content
     ext_xml   = abap_true
     svar_name = 'ROOT_NODE'
   IMPORTING
     abap_data = result ).
ENDMETHOD.
ENDCLASS.
```
 
In stateful programming, we manipulate the internal state of objects through their methods,
meaning this is _full of side effects_.

```ABAP
CLASS /clean/log DEFINITION PUBLIC CREATE PUBLIC.
PUBLIC SECTION.
 METHODS add_message IMPORTING message TYPE /clean/message.
PRIVATE SECTION.
 DATA messages TYPE /clean/message_table.
ENDCLASS.

CLASS /clean/log IMPLEMENTATION.
METHOD add_message.
 INSERT message INTO TABLE messages.
ENDMETHOD.
ENDCLASS.
```
 
Both paradigms are okay and have their applications.
However, _mixing_ them in the same object produces code that is hard to understand and sure to fail
with obscure carry-over errors and synchronicity problems.
Don't do that.

### Scope

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [This section](#scope)

#### Global by default, local only in exceptional cases

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Scope](#scope) > [This section](#global-by-default-local-only-in-exceptional-cases)

Work with global classes as default (meaning the ones that are visible in the dictionary).

Use local classes only in exceptional cases,
for example for very specific data structures or to facilitate writing unit tests.

Local classes hinder reuse because they cannot be used elsewhere.
Although they are easy to extract, people will usually fail to even find them.

A clear indication that a local class should be made global is if you have the urge to write tests for it.
A local class is a natural private cogwheel in its greater global class that you will usually not test.
The need for tests indicates that the class is independent from its surrounding and so complex
that it should become an object of its own.

#### FINAL if not designed for inheritance

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Scope](#scope) > [This section](#final-if-not-designed-for-inheritance)

Make classes that are not explicitly designed for inheritance `FINAL`.

When designing class cooperation,
your first choice should be [composition, not inheritance](#prefer-composition-over-inheritance).
Enabling inheritance is not something that should be done lightly,
as it requires you to think about things like `PROTECTED` vs. `PRIVATE`
and the [Liskov substitution principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle),
and freezes a lot of design internals.
If you didn't consider these things in your class design,
you should thus prevent accidental inheritance by making your class `FINAL`.

There _are_ some good applications for inheritance, of course,
for example the design pattern [composite](https://en.wikipedia.org/wiki/Composite_pattern).
Business Add-Ins can also become more useful by allowing sub-classes,
enabling the customer to reuse most of the original code.
However, note that all of these cases have inheritance built in by design from the start.

Unclean classes that don't [implement interfaces](#public-instance-methods-should-be-part-of-an-interface)
should be left non-`FINAL` to allow consumers mocking them in their unit tests.

#### Members PRIVATE by default, PROTECTED only if needed

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Scope](#scope) > [This section](#members-private-by-default-protected-only-if-needed)

Make attributes, methods, and other class members `PRIVATE` by default.

Make them only `PROTECTED` if you want to enable sub-classes that override them.

Internals of classes should be made available to others only on a need-to-know basis.
This includes not only outside callers but also sub-classes.
Making information over-available can cause subtle errors by unexpected redefinitions and hinder refactoring
because outsiders freeze members in place that should still be liquid.

#### Consider using immutable instead of getter

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Scope](#scope) > [This section](#consider-using-immutable-instead-of-getter)

An immutable is an object that never changes after its construction.
For this kind of object consider using public read-only attributes instead of getter methods.

```ABAP
CLASS /clean/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        a TYPE i
        b TYPE c
        c TYPE d.
    DATA a TYPE i READ-ONLY.
    DATA b TYPE c READ-ONLY.
    DATA c TYPE d READ-ONLY.
ENDCLASS.
```

instead of

```ABAP
CLASS /dirty/some_data_container DEFINITION.
  PUBLIC SECTION.
    METHODS get_a ...
    METHODS get_b ...
    METHODS get_c ...
  PRIVATE SECTION.
    DATA a TYPE i.
    DATA b TYPE c.
    DATA c TYPE d.
ENDCLASS.
```

> **Caution**: For objects which **do** have changing values, do not use public read-only attributes.
> Otherwise this attributes always have to be kept up to date,
> regardless if their value is needed by any other code or not.

#### Use READ-ONLY sparingly

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Scope](#scope) > [This section](#use-read-only-sparingly)

Many modern programming languages, especially Java, recommend making class members read-only
wherever appropriate to prevent accidental side effects.

While ABAP _does_ offer the `READ-ONLY` addition for data declarations, we recommend to use it sparingly.

First, the addition is only available in the `PUBLIC SECTION`, reducing its applicability drastically.
You can neither add it to protected or private members nor to local variables in a method.

Second, the addition works subtly different from what people might expect from other programming languages:
READ-ONLY data can still be modified freely from any method within the class itself, its friends, and its sub-classes.
This contradicts the more widespread write-once-modify-never behavior found in other languages.
The difference may lead to bad surprises.

> To avoid misunderstandings: Protecting variables against accidental modification is a good practice.
> We would recommend applying it to ABAP as well if there was an appropriate statement.

### Constructors

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [This section](#constructors)

#### Prefer NEW over CREATE OBJECT

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Constructors](#constructors) > [This section](#prefer-new-over-create-object)

```ABAP
DATA object TYPE REF TO /clean/some_number_range.
object = NEW #( '/CLEAN/CXTGEN' )
...
DATA(object) = NEW /clean/some_number_range( '/CLEAN/CXTGEN' ).
...
DATA(object) = CAST /clean/number_range( NEW /clean/some_number_range( '/CLEAN/CXTGEN' ) ).
```

instead of the needlessly longer

```ABAP
" anti-pattern
DATA object TYPE REF TO /dirty/some_number_range.
CREATE OBJECT object
  EXPORTING
    number_range = '/DIRTY/CXTGEN'.
```

except where you need dynamic types, of course

```ABAP
CREATE OBJECT number_range TYPE (dynamic_type)
  EXPORTING
    number_range = '/CLEAN/CXTGEN'.
```

#### If your global class is CREATE PRIVATE, leave the CONSTRUCTOR public

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Constructors](#constructors) > [This section](#if-your-global-class-is-create-private-leave-the-constructor-public)

```ABAP
CLASS /clean/some_api DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    METHODS constructor.
```

We agree that this contradicts itself.
However, according to the article
[_Instance Constructor_ of the ABAP Help](https://ldcifri.wdf.sap.corp:44300/sap/public/bc/abap/docu?input=guideline&format=standard&object=abeninstance_constructor_guidl&sap-language=EN&sap-client=100&full_text=X&tree=x),
specifying the `CONSTRUCTOR` in the `PUBLIC SECTION` is required to guarantee correct compilation and syntax validation.

This applies only to global classes.
In local classes, make the constructor private, as it should be.

#### Prefer multiple static factory methods over optional parameters

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Constructors](#constructors) > [This section](#prefer-multiple-static-factory-methods-over-optional-parameters)

```ABAP
CLASS-METHODS describe_by_data IMPORTING data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING data_ref TYPE REF TO data [...]
```

Don't try to "remedy" ABAP's missing support for
[overloading](https://en.wikipedia.org/wiki/Function_overloading)
by adding optional parameters.

```ABAP
" anti-pattern
METHODS constructor
  IMPORTING
    data       TYPE any OPTIONAL
    name       TYPE any OPTIONAL
    object_ref TYPE REF TO object OPTIONAL
    data_ref   TYPE REF TO data OPTIONAL
  [...]
```

The general guideline
[_Split methods instead of adding OPTIONAL parameters_](#split-methods-instead-of-adding-optional-parameters)
explains the reasoning behind this.

Consider resolving complex constructions to a multi-step construction with the
[Builder design pattern](https://en.wikipedia.org/wiki/Builder_pattern).

#### Use descriptive names for multiple constructor methods

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Constructors](#constructors) > [This section](#use-descriptive-names-for-multiple-constructor-methods)

```ABAP
CLASS-METHODS describe_by_data IMPORTING p_data TYPE any [...]
CLASS-METHODS describe_by_name IMPORTING p_name TYPE any [...]
CLASS-METHODS describe_by_object_ref IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS describe_by_data_ref IMPORTING p_data_ref TYPE REF TO data [...]
```

instead of something meaningless like

```ABAP
" anti-pattern
CLASS-METHODS create_1 IMPORTING p_data TYPE any [...]
CLASS-METHODS create_2 IMPORTING p_name TYPE any [...]
CLASS-METHODS create_3 IMPORTING p_object_ref TYPE REF TO object [...]
CLASS-METHODS create_4 IMPORTING p_data_ref TYPE REF TO data [...]
```

Good words to start constructors are `new_`, `create_`, and `construct_`.
People intuitively connect them to the construction of objects.
They also add up nicely to verb phrases like `new_from_template`, `create_as_copy`, or `create_by_name`.

#### Make singletons only where multiple instances don't make sense

> [Clean ABAP](#clean-abap) > [Content](#content) > [Classes](#classes) > [Constructors](#constructors) > [This section](#make-singletons-only-where-multiple-instances-dont-make-sense)

```ABAP
METHOD new.
  IF singleton IS NOT BOUND.
    singleton = NEW /clean/my_class( ).
  ENDIF.
  result = singleton.
ENDMETHOD.
```

Apply the singleton pattern where your object-oriented design says
that having a second instance of something doesn't make sense.
Use it to ensure that every consumer is working with the same thing in the same state and with the same data.

Do not use the singleton pattern out of habit or because some performance rule tells you so.
It is the most overused and wrongly applied pattern and
produces unexpected cross-effects and needlessly complicates testing.
If there are no design-driven reasons for a unitary object,
leave that decision to the consumer - he can still reach the same by means outside the constructor,
for example with a factory.

## Methods

> [Clean ABAP](#clean-abap) > [Content](#content) > [This section](#methods)

These rules apply to methods in classes and function modules.

### Calls

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [This section](#calls)

#### Prefer functional over procedural calls

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Calls](#calls) > [This section](#prefer-functional-over-procedural-calls)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

instead of the needlessly longer

```ABAP
" anti-pattern
CALL METHOD modify->update
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

If dynamic typing forbids functional calls, resort to the procedural style

```ABAP
CALL METHOD modify->(method_name)
  EXPORTING
    node           = /clean/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields.
```

Many of the detailed rules below are just more specific variations of this advice.

#### Omit RECEIVING

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Calls](#calls) > [This section](#omit-receiving)

```ABAP
DATA(sum) = aggregate_values( values ).
```

instead of the needlessly longer

```ABAP
" anti-pattern
aggregate_values(
  EXPORTING
    values = values
  RECEIVING
    result = DATA(sum) ).
```

#### Omit the optional keyword EXPORTING

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Calls](#calls) > [This section](#omit-the-optional-keyword-exporting)

```ABAP
modify->update( node           = /clean/my_bo_c=>node-item
                key            = item->key
                data           = item
                changed_fields = changed_fields ).
```

instead of the needlessly longer

```ABAP
" anti-pattern
modify->update(
  EXPORTING
    node           = /dirty/my_bo_c=>node-item
    key            = item->key
    data           = item
    changed_fields = changed_fields ).
```

#### Omit the parameter name in single parameter calls

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Calls](#calls) > [This section](#omit-the-parameter-name-in-single-parameter-calls)

```ABAP
DATA(unique_list) = remove_duplicates( list ).
```

instead of the needlessly longer

```ABAP
" anti-pattern
DATA(unique_list) = remove_duplicates( list = list ).
```

There are cases, however, where the method name alone is not clear enough
and repeating the parameter name may further understandability:

```ABAP
car->drive( speed = 50 ).
```

### Methods: Object orientation

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [This section](#methods-object-orientation)

#### Prefer instance to static methods

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Methods: Object orientation](#methods-object-orientation) > [This section](#prefer-instance-to-static-methods)

Methods should be instance members by default.
Instance methods better reflect the "object-hood" of the class.
They can be mocked easier in unit tests.

```ABAP
METHODS publish.
```

Methods should be static only in exceptional cases, such as static constructor methods.

```ABAP
CLASS-METHODS create_instance
  RETURNING
    VALUE(result) TYPE REF TO /clean/blog_post.
```

#### Public instance methods should be part of an interface

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Methods: Object orientation](#methods-object-orientation) > [This section](#public-instance-methods-should-be-part-of-an-interface)

Public instance methods should always be part of an interface.
This decouples dependencies and simplifies mocking them in unit tests.

```ABAP
METHOD /clean/blog_post~publish.
```

In clean object orientation, having a method public without an interface does not make much sense -
with few exceptions such as enumeration classes
which will never have an alternative implementation and will never be mocked in test cases.

### Parameter Number

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [This section](#parameter-number)

#### Aim for few IMPORTING parameters, at best less than three

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Parameter Number](#parameter-number) > [This section](#aim-for-few-importing-parameters-at-best-less-than-three)

```ABAP
FUNCTION seo_class_copy
  IMPORTING
    clskey      TYPE seoclskey
    new_clskey  TYPE seoclskey
    config      TYPE class_copy_config
  EXPORTING
    ...
```

would be much clearer than

```ABAP
" anti-pattern
FUNCTION seo_class_copy
  IMPORTING
    clskey                 TYPE seoclskey
    new_clskey             TYPE seoclskey
    access_permission      TYPE seox_boolean DEFAULT seox_true
    VALUE(save)            TYPE seox_boolean DEFAULT seox_true
    VALUE(suppress_corr)   TYPE seox_boolean DEFAULT seox_false
    VALUE(suppress_dialog) TYPE seox_boolean DEFAULT seox_false
    VALUE(authority_check) TYPE seox_boolean DEFAULT seox_true
    lifecycle_manager      TYPE REF TO if_adt_lifecycle_manager OPTIONAL
    lock_handle            TYPE REF TO if_adt_lock_handle OPTIONAL
    VALUE(suppress_commit) TYPE seox_boolean DEFAULT seox_false
  EXPORTING
    ...
```

Too many input parameters let the complexity of a method explode
because it needs to handle an exponential number of combinations.
Many parameters are an indicator that the method may do more than one thing.

You can reduce the number of parameters by combining them into meaningful sets with structures and objects.

#### Split methods instead of adding OPTIONAL parameters

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Parameter Number](#parameter-number) > [This section](#split-methods-instead-of-adding-optional-parameters)

```ABAP
METHODS do_one_thing IMPORTING what_i_need TYPE string.
METHODS do_another_thing IMPORTING something_else TYPE i.
```

instead of trying to compensate ABAP's missing support for
[overloading](https://en.wikipedia.org/wiki/Function_overloading)
by adding optional parameters

```ABAP
" anti-pattern
METHODS do_one_or_the_other
  IMPORTING
    what_i_need    TYPE string
    something_else TYPE i.
```

Optional parameters confuse callers:
Which ones are really required?
Which combinations are valid?
Which ones exclude each other?

Multiple methods avoid this confusion by giving clear guidance which parameter combinations are valid and expected.

#### Use PREFERRED PARAMETER sparingly

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Parameter Number](#parameter-number) > [This section](#use-preferred-parameter-sparingly)

The addition `PREFERRED PARAMETER` makes it hard to see which parameter is actually supplied,
making it harder to understand the code.
Minimizing the number of parameters, especially optional ones,
automatically reduces the need for `PREFERRED PARAMETER`.

#### RETURN, EXPORT, or CHANGE exactly one parameter

> [Clean ABAP](#clean-abap) > [Content](#content) > [Methods](#methods) > [Parameter Number](#parameter-number) > [This section](#return-export-or-change-exactly-one-parameter)

A good method does _one thing_, and that should be reflected by the method also returning exactly one thing.
If the output parameters of your method do _not_ form a logical entity,
your method does more than one thing and you should split it.

There are cases where the output is a logical entity that consists of multiple things.
These are easiest represented by returning a structure or object:

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```

instead of

```ABAP
" anti-pattern
METHODS check_business_partners
  IMPORTING
    business_partners TYPE business_partners
  EXPORTING
    result            TYPE result_type
    failed_keys       TYPE /bobf/t_frw_key
    messages          TYPE /bobf/t_frw_message.
```

Especially in comparison to multiple EXPORTING parameters, this allows people to use the functional call style,
spares you having to think about `IS SUPPLIED` and saves people from accidentally forgetting
to retrieve a vital `ERROR_OCCURRED` information.

Instead of multiple optional output parameters, consider splitting the method according to meaningful call patterns:

```ABAP
TYPES:
  BEGIN OF check_result,
    result      TYPE result_type,
    failed_keys TYPE /bobf/t_frw_key,
    messages    TYPE /bobf/t_frw_message,
  END OF check_result.

METHODS check
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE result_type.

METHODS check_and_report
  IMPORTING
    business_partners TYPE business_partners
  RETURNING
    VALUE(result)     TYPE check_result.
```