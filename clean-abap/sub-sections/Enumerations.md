# Enumerations

> [Back to the guide](../CleanABAP.md)

ABAP did not support enumerations as natively and completely as other programming languages before release 7.51.

ABAPers therefore were forced to think up their own solutions
and came up with a set of [patterns](#patterns) that can be
found in the majority of today's object-oriented ABAP code.

**Starting with release 7.51 native enumerated types are available and should be preferred where applicable.**
They offer [compatibility](#compatibility) features to easily refactor legacy enumeration patterns.
When deciding against native enumerations or wanting to design one of your own, consider the [guidelines](#guidelines).

- [Native enumerations](#native-enumerations)
  - [Compatibility](#compatibility)
- [Patterns](#patterns)
  - [Constant pattern](#constant-pattern)
  - [Object pattern](#object-pattern)
  - [Interface pattern](#interface-pattern)
  - [Collection pattern](#collection-pattern)
- [Guidelines](#guidelines)
  - [Use one development object per enumeration](#use-one-development-object-per-enumeration)
  - [Prefer classes to interfaces](#prefer-classes-to-interfaces)
  - [Try to enforce type safety](#try-to-enforce-type-safety)

## Native enumerations

> [Enumerations](#enumerations) > [This section](#native-enumerations)

Starting with release 7.51 ABAP offers a native definition of enumerated types with `TYPES BEGIN OF ENUM`.

```ABAP
CLASS /clean/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM type,
             warning,
             error,
           END OF ENUM type.
ENDCLASS.

CLASS /clean/message_severity IMPLEMENTATION.
ENDCLASS.
```

used as

```ABAP
IF log_contains( /clean/message_severity=>warning ).
```

> Note that the [`STRUCTURE` addition](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptypes_enum.htm#!ABAP_ADDITION_1@1@) **is not used**.
>
> One reason is that this would widen the API surface without the requiremend to do so.
> If the definition was `BEGIN OF ENUM type STRUCTURE severity` then `/dirty/message_severity=>severity` could be copied and passed around which is undesirable.
>
> Another reason is the additional grouping level that `STRUCTURE` introduces:
> If an enumeration value is used there is duplication that does not bring any semantic benefit:
> `/dirty/message_severity=>severity-warning` for example repeats the word "_severity_".
> A rather short-sighted reaction could be cutting that word from the class name.
> The remaining `/dirty/message=>severity`, however, misleads as for the purpose of the class `/dirty/message` - after all it deals only with message severities and not with messages in general.
> If further enumerations were to be added to that class its purpose would become unclear thus violating the single-responsibility principle.
>
> Thus declaring exactly one enumeration **without** `STRUCTURE` in a dedicated class should be preferred.
> It yields nicely addressable top-level enumeration values (see example at the beginning of this section).
> Prefer treating enumerations as first-class citizens and do not introduce unnecessary depth with structures.

### Compatibility

To integrate native enumerations with legacy code that uses constants the `BASE TYPE` addition is available:
```ABAP
CLASS /compatbl/message_severity DEFINITION PUBLIC ABSTRACT FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM type BASE TYPE symsgty,
            info      VALUE 'I',
            exit      VALUE 'X',
            undefined VALUE IS INITIAL,
          END OF ENUM type.
```
This allows a conversion from and to enumerated variables using the `CONV` operator.
```ABAP
"yields 'I'
DATA(severity_as_char) = CONV symsgty( /compatbl/message_severity=>info ). 

"yields /compatbl/message_severity=>exit
DATA(severity) = CONV /compatbl/message_severity=>type( 'X' ). 
```

The conversion operator is mandatory to satisfy the strict type check.
It needs to be incorporated at all places where APIs with legacy types are operated or whenever data is queried or persisted with ABAP SQL.

For example, if the method signature of `log_contains` with a single `IMPORTING` parameter typed as `symsgty` cannot be refactored it will have to be called like this:
```ABAP
IF log_contains( CONV #( /compatbl/message_severity=>warning ) ).
```

## Patterns

> [Enumerations](#enumerations) > [This section](#patterns)

If native `ENUM` cannot be used we recommend either
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
    METHODS constructor IMPORTING value TYPE symsgty.

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

used in a type-safe way as follows:

```ABAP
" modern signature: ... IMPORTING severity TYPE REF TO /clean/message_severity ...
IF log_contains( /clean/message_severity=>warning ).
```

In legacy code where existing signatures cannot be refactored the property `value` must be accessed and used to satisfy the legacy method parameter type:

```ABAP
" legacy signature: ... IMPORTING severity TYPE symsgty ...
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
<details>
  <summary>The local test class for the constant pattern can look like this:</summary>
  
```ABAP
CLASS ltcl_constant_pattern DEFINITION CREATE PRIVATE
 FOR TESTING
 RISK LEVEL HARMLESS
 DURATION SHORT.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS class_name TYPE classname VALUE '/CLEAN/MESSAGE_SEVERITY'.
    CONSTANTS domain_name TYPE domname  VALUE 'DOM_MSG_SEVERITY'.
    DATA domain_values TYPE STANDARD TABLE OF dd07v.
    DATA constants_of_enum_class TYPE abap_attrdescr_tab.

    METHODS setup.
    METHODS all_values_as_constant FOR TESTING.
    METHODS all_constants_in_domain FOR TESTING.
ENDCLASS.

CLASS ltcl_constant_pattern IMPLEMENTATION.
  METHOD setup.
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname   = domain_name
      TABLES
        dd07v_tab = domain_values
      EXCEPTIONS
        OTHERS    = 1.
    ASSERT sy-subrc = 0.

    DATA class_descr TYPE REF TO cl_abap_classdescr.
    class_descr ?= cl_abap_typedescr=>describe_by_name( class_name ).

    constants_of_enum_class = class_descr->attributes.
  ENDMETHOD.

  METHOD all_constants_in_domain.
    LOOP AT constants_of_enum_class INTO DATA(component).
    
      ASSIGN (class_name)=>(component-name) TO FIELD-SYMBOL(<value>).
      ASSERT sy-subrc = 0.
      
      IF NOT line_exists( domain_values[ domvalue_l = <value> ] ).
        cl_abap_unit_assert=>fail( |Component { component-name } not found in domain fix values| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD all_values_as_constant.
    DATA value_found TYPE abap_bool.

    LOOP AT domain_values INTO DATA(domain_value).

      CLEAR value_found.
      LOOP AT constants_of_enum_class INTO DATA(component).

        ASSIGN (class_name)=>(component-name) TO FIELD-SYMBOL(<value>).
        ASSERT sy-subrc = 0.

        IF  domain_value-domvalue_l = <value>.
          value_found = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF value_found = abap_false.
        cl_abap_unit_assert=>fail( |Domainvalue { domain_value-domvalue_l } not available as constant| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
```
</details>

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

The real advantage of enumerations in programming languages
is not that they provide constants,
but that they provide _all_ constants,
meaning they enforce type safety
by making the compiler reject invalid values.

Native enumerated types fulfill this criterion as the following method definition
```ABAP
METHODS log_contains
  IMPORTING
    minimum_severity TYPE /clean/message_severity=>type.
```
will allow correct usage like this
```ABAP
IF log_contains( /clean/message_severity=>warning ).
```
yet reject invalid calls such as
```ABAP
" syntax error
IF log_contains( 'W' ).
```
```ABAP
" runtime error
IF log_contains( CONV /clean/message_severity=>type( 'B' ) ).
```

If native enumeration cannot be used this is only achievable by the **[object pattern](#object-pattern)**:
```ABAP
METHODS log_contains
  IMPORTING
    minimum_severity TYPE REF TO /clean/message_severity.
```

Without type safety, you still get helpful constants
but will find yourself repeating `is_valid( )` validations
all over the place.

```ABAP
" inferior pattern...
METHODS log_contains
  IMPORTING
    minimum_severity TYPE symsgty.

" ...is not preventing illegal values:
IF log_contains( '?' ).
```
