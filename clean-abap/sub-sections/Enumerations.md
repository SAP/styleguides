# Enumerations

> [Back to the guide](../CleanABAP.md)

ABAP does not support enumerations as natively and completely as
other programming languages.

Over time, ABAPers came up with two patterns that we consider clean
and recommend to realize enumerations,
the **[constant pattern](#constant-pattern)**
and the **[object pattern](#object-pattern)**.

The [interface pattern](#interface-pattern),
is also acceptable, but has some slight drawbacks.

Think twice before resorting to the
[collection pattern](#collection-pattern).
Although it has become widely spread through BOPF and looks clean,
it harbors the danger of degrading into a mess.

## Constant Pattern

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

## Object Pattern

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

## Interface Pattern

```ABAP
INTERFACE /dirty/message_severity.
  CONSTANTS:
    warning      TYPE symsgty VALUE 'W',
    error        TYPE symsgty VALUE 'E',
ENDINTERFACE.
```

used as

```ABAP
IF log_contains( /clean/message_severity=>warning ).
```

## Collection Pattern

```ABAP
" anti-pattern
INTERFACE /dirty/message_constants.
  CONSTANTS:
    BEGIN OF message_severity,
      warning      TYPE symsgty VALUE 'W',
      error        TYPE symsgty VALUE 'E',
    END OF message_severity,
    BEGIN OF message_lifecycle,
      transitional TYPE i       VALUE 1,
      persisted    TYPE i       VALUE 2,
    END OF message_lifecycle,
ENDINTERFACE.
```

used as

```ABAP
IF log_contains( /clean/message_constants=>message_severity-warning ).
```

## Enum Pattern

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ENUM,
        warning TYPE symsgty VALUE 'W',
        error   TYPE symsgty VALUE 'E',
      END OF ENUM.
ENDCLASS.

CLASS /clean/message_severity IMPLEMENTATION.
ENDCLASS.
```

used as

```ABAP
IF log_contains( /clean/message_constants=>warning ).
```

## Comparison

### Type Safety

The [object pattern](#object-pattern) is the only enumeration pattern
that adds type safety, meaning it prevents people from supplying
invalid values.

By requiring object references instead of plain character symbols,
you can prevent callers from supplying invalid content:

```ABAP
METHODS log_contains
  IMPORTING
    minimum_severity TYPE REF TO /clean/message_severity.

" not possible to do this
log_contains( '42' ).
```

The other patterns don't allow this, such that you will have to add
validity checks in various places.

### Methods

The class-based patterns [constant](#constant-pattern)
and [object](#object-pattern) allow adding methods
that simplify dealing with the enumeration,
for example common ones like

```ABAP
METHODS is_valid ...
METHODS contains ...
METHODS equals ...
METHODS to_string ...
```

or enumeration-specific ones such as

```ABAP
METHODS is_more_severe_than ...
```

### Tests

The class-based patterns [constant](#constant-pattern)
and [object](#object-pattern) allow adding unit tests
that ensure that your enumeration is correct,
for example to cover the [methods](#methods) you created for your enum
or for the common situation

```ABAP
METHOD in_sync_with_fixed_domain_vals.
```

Interface-based patterns have to store such tests in more obscure
locations.

### Cohesion

Classes that use an enumeration should depend on this enumeration,
and this enumeration alone.

Collecting different enumerations in the same development object,
as in the [collection pattern](#collection-pattern),
may couple consumers to undesired, unrelated values.

### Search

The patterns [constant](#constant-pattern), [object](#object-pattern),
and [interface](#interface) allow searching enumerations by searching
for the development object's name.

Patterns that collect multiple different enumerations in the same
development object must resort to a slower and less convenient
where-used or fulltext search.

Observations suggest that people search for a constant value
only a couple of seconds before resigning and violating the
don't-repeat-yourself principle by
"quickly creating it here as a local constant".

### Object-Orientation

A good enumeration pattern should support object orientation
instead of standing in its way or allowing weird practices.

The interface-based patterns [interface](#interface-pattern) and
[collection](#collection-pattern) tempt consumers to "implement"
the enumeration interface to shorten the syntax from

```ABAP
IF log_contains( /dirty/message_severity=>warning ).
```

to

```ABAP
IF log_contains( warning ).
```

However, although technically possible, this doesn't make sense from
an object-orientation point of view and should be avoided.

### Universality

Enumerations should be applicable to all parts of your programming,
including data exchange through APIs and with the database.

Although ABAP's built-in statement `ENUM` allows defining value lists
for variables and methods, it does not allow using those values
for data exchange.
As a consequence, you will find yourself defining the same value list
twice, once as constants, once as an enum, which contradicts the
don't-repeat-yourself principle.
Also, any data exchange needs an inbound or outbound conversion to
get the enum values, which quickly becomes tedious and inefficient.

The [object pattern](#object-pattern) also suffers from this weakness,
but at least offers the possibility.

The class-based patterns [constant](#constant-pattern) and
[object](#object-pattern) prevent misuse by making their classes
`ABSTRACT FINAL`, likewise shutting down inheritance and instantation.

The [constant](#constant-pattern) and [object pattern](#object-pattern)
support this
The  has procedural roots but
is still okay.

The [interface pattern](#interface-pattern

An enumeration should group all possible values for a data type in
one place and at one glance.

Constant | Object | Interface | Collection
--- | --- | --- | ---
X | X | X | X

The enumeration should be focused, such that classes that depend on it
depend only on the enumeration, not on other things that they don't
need.

Constant | Object | Interface | Collection
--- | --- | --- | ---
X | X | X | -



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