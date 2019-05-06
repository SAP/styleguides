# Enumerations

> [Back to the guide](../CleanABAP.md)

ABAP does not support enumerations as natively and completely as
other programming languages.

ABAPers therefore were forced to think up their own solutions
and came up with a set of [patterns](#patterns) that can be
found in the majority of today's object-oriented ABAP code.

When deciding for an enumeration pattern,
or wanting to design one of your own,
consider the [guidelines](#guidelines).

- [Patterns](#patterns)
  - [Constant pattern](#constant-pattern)
  - [Object pattern](#object-pattern)
  - [Interface pattern](#interface-pattern)
  - [Collection pattern](#collection-pattern)
- [Guidelines](#guidelines)
  - [Use one development object per enumeration](#use-one-development-object-per-enumeration)
  - [Prefer classes to interfaces](#prefer-classes-to-interfaces)
  - [Try to enforce type safety](#try-to-enforce-type-safety)
- [What about ENUM?](#what-about-enum)

## Patterns

> [Enumerations](#enumerations) > [This section](#patterns)

We recommend using either
the **[constant pattern](#constant-pattern)**
or the **[object pattern](#object-pattern)**
because they combine most advantages
and can be generally considered clean.

The widely used [interface pattern](#interface-pattern)
is also acceptable, but has some slight drawbacks.

Think twice before resorting to the
[collection pattern](#collection-pattern).
Although it has become widely spread through BOPF,
and can be quite convenient in some scenarios,
it harbors the danger of degrading into a mess.

### Constant Pattern

> [Enumerations](#enumerations) > [Patterns](#patterns) > [This section](#constant-pattern)

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

used as

```ABAP
IF log_contains( /clean/message_severity=>warning ).
```

### Object Pattern

> [Enumerations](#enumerations) > [Patterns](#patterns) > [This section](#object-pattern)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC CREATE PRIVATE FINAL.

  PUBLIC SECTION.

    CLASS-DATA warning TYPE REF TO /clean/message_severity READ-ONLY,
    CLASS-DATA error   TYPE REF TO /clean/message_severity READ-ONLY.

    DATA value TYPE symsgty READ-ONLY.

    CLASS-METHODS class_constructor.
    METHODS constructor IMPORTING value TYPE /clean/severity.

ENDCLASS.

CLASS /clean/message_severity IMPLEMENTATION.

  METHOD class_constructor.
    warning = NEW /clean/message_severity( 'W' ).
    error = NEW /clean/message_severity( 'E' ).
  ENDMETHOD.

  METHOD constructor.
    me->value = value.
  ENDMETHOD.

ENDCLASS.
```

used as

```ABAP
IF log_contains( /clean/message_severity=>warning->value ).
```

### Interface Pattern

> [Enumerations](#enumerations) > [Patterns](#patterns) > [This section](#interface-pattern)

```ABAP
" inferior pattern
INTERFACE /dirty/message_severity.
  CONSTANTS:
    warning TYPE symsgty VALUE 'W',
    error   TYPE symsgty VALUE 'E'.
ENDINTERFACE.
```

used as

```ABAP
IF log_contains( /dirty/message_severity=>warning ).
```

### Collection Pattern

> [Enumerations](#enumerations) > [Patterns](#patterns) > [This section](#collection-pattern)

```ABAP
" inferior pattern
INTERFACE /dirty/message_constants.
  CONSTANTS:
    BEGIN OF message_severity,
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E',
    END OF message_severity,
    BEGIN OF message_lifecycle,
      transitional TYPE i VALUE 1,
      persisted    TYPE i VALUE 2,
    END OF message_lifecycle.
ENDINTERFACE.
```

used as

```ABAP
IF log_contains( /dirty/message_constants=>message_severity-warning ).
```

## Guidelines

> [Enumerations](#enumerations) > [This section](#guidelines)

### Use one development object per enumeration

> [Enumerations](#enumerations) > [Guidelines](#guidelines) > [This section](#use-one-development-object-per-enumeration)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.

CLASS /clean/document_type DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      sales_order    TYPE char02 VALUE '01',
      purchase_order TYPE char02 VALUE '02'.
ENDCLASS.
```

This simplifies searching for enumerations
because you can search for the name of the development object
instead of hassling with where-used lists and fulltext code searches.

Effective search is important as observations suggest
that being unable to find the required enumeration
causes people to quickly create constants
a second and third time in different places,
violating the don't-repeat-yourself principle. 

Separate development objects also improve cohesion of your classes
because consumers depend only on exactly what they need,
not some other enumerations that only accidentally happen
to reside in the same development object.

```ABAP
" anti-pattern
CLASS /dirty/common_constants DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF message_severity,
        warning TYPE symsgty VALUE 'W',
        error   TYPE symsgty VALUE 'E',
      END OF message_severity,
      BEGIN OF document_type,
        sales_order    TYPE char02 VALUE '01',
        purchase_order TYPE char02 VALUE '02',
      END OF document_type.
ENDCLASS.
```

### Prefer classes to interfaces

> [Enumerations](#enumerations) > [Guidelines](#guidelines) > [This section](#prefer-classes-to-interfaces)

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    CONSTANTS:
      warning TYPE symsgty VALUE 'W',
      error   TYPE symsgty VALUE 'E'.
ENDCLASS.
```

Classes allow adding supportive methods,
such as the often-encountered
`is_valid`, `equals`, `contains`, and `to_string` methods,
or enumeration-specific ones such as `is_more_severe_than`.

They also provide a natural place for unit tests,
especially if you added supportive methods,
but also for common cases such as
`in_sync_with_domain_fixed_vals`.

Moreover, classes enforce clean object orientation
through the additions `ABSTRACT` and `FINAL`.
Interfaces tempt people to "implement" them.
While this shortens their syntax by using the constants
without a leading `/dirty/message_severity=>`,
this kind of "inheritance out of convenience"
makes no sense in object orientation
and should be avoided.

```ABAP
" inferior pattern
INTERFACE /dirty/message_severity.
  CONSTANTS:
    warning TYPE symsgty VALUE 'W',
    error   TYPE symsgty VALUE 'E'.
ENDINTERFACE.
```

### Try to enforce type safety

> [Enumerations](#enumerations) > [Guidelines](#guidelines) > [This section](#try-to-enforce-type-safety)

```ABAP
METHODS log_contains
  IMPORTING
    minimum_severity TYPE REF TO /clean/message_severity.
```

The real advantage of enumerations in other programming languages
is not that they provide constants,
but that they provide _all_ constants,
meaning they enforce type safety
by making the compiler reject invalid values.

Without type safety, you still get helpful constants
but will find yourself repeating `is_valid( )` validations
all over the place.

```ABAP
" inferior pattern
METHODS log_contains
  IMPORTING
    minimum_severity TYPE symsgty.
```

## What about ENUM?

> [Enumerations](#enumerations) > [This section](#what-about-enum)

So far we have not seen an efficient wide-spread pattern
that exploits the ABAP keyword `ENUM`.
We assume there are such patterns
but that people haven't explored them in depth so far.

One of the problems with `ENUM` is that it
does not only create constants,
but also new data types alongside.
This makes it harder to apply it to common cases
where the data types already exist,
especially communication with APIs and the database.

If you know a good enum-based pattern,
or designed one on your own,
[let us know about it](https://github.com/SAP/clean-abap/blob/master/CONTRIBUTING.md).
