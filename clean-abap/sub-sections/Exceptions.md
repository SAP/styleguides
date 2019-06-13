# Exceptions

What type of exception to use for what is a hot topic
that has been discussed controversially
across time and programming languages.

## The Ideal

According to discussions in the community,
the optimal way to deal with exceptions would be as follows.

First, we want to declare the exception in
the `lower_method` that actually throws it,
to not surprise callers:


```ABAP
METHODS lower_method
  RAISING
    /clean/flexible_exception.

METHOD lower_method.
  RAISE EXCEPTION NEW /clean/flexible_exception( ).
ENDMETHOD:
```

Then we want to let the exception bubble upwards through `middle_method`s
without forcing them to redeclare or catch the exception,
to avoid refactoring cascades if exceptions change:

```ABAP
METHODS middle_method.

METHOD middle_method.
  lower_method( ).
ENDMETHOD.
```

Finally, we want to catch the exception in some `upper_method` and handle it.

```ABAP
METHODS upper_method.

METHOD upper_method.
  TRY.
      middle_method( ).
    CATCH /clean/flexible_exception.
      " ...
  ENDTRY.
ENDMETHOD.
```

This is also exactly what Robert C. Martin advertises for Java,
where this pattern can be implemented with [unchecked exceptions](https://docs.oracle.com/javase/7/docs/api/java/lang/RuntimeException.html).

## What's in the Way

Excactly this ideal way is not possible in ABAP
because none of the available three exception types supports it:

### Why CX_STATIC_CHECK Doesn't Help

`cx_static_check` would force `middle_method`
to redeclare the `/clean/flexible_exception`.
Although the syntax check throws "only" a warning,
the ABAP Test Cockpit responds with an issue with Very High priority
that will prevent transport release in standard system setups.

Even if we were willing to accept these warnings,
without redeclaring `/clean/flexible_exception`,
`middle_method` would not forward it 
but trigger a `cx_sy_no_handler` exception
which ultimately leads to a dump.

### Why CX_DYNAMIC_CHECK Doesn't Help

`cx_dynamic_check` doesn't improve this.
Alhough it makes the syntax check accept the missing redeclaration
and catch block in `middle_method`,
the code will still trigger `cx_sy_no_handler`
if the exception is actually thrown.

### Why CX_NO_CHECK Doesn't Help

`cx_no_check` also doesn't help.
Although it makes the method bodies work,
the `METHODS lower_method` definition now is
no longer allowed to declare `/clean/flexible_exception`
and compilation fails with a syntax error.

### Why CX_SY_NO_HANDLER Doesn't Help

Catching the special exception `cx_sy_no_handler`
is a workaround that appears to get pretty near to the ideal way,
but also adds some imperfections that make it hard to recommend it:

```ABAP
METHOD upper_method.
  TRY.
      middle_method( ).
    CATCH cx_sy_no_handler INTO DATA(outer).
      DATA(inner) = outer->previous.
      " identify and branch on inner's type
  ENDTRY.
ENDMETHOD.
```

`cx_sy_no_handler` prevents using multiple `catch` branches
to handle different exceptions in different ways.

The code required to identify the actual exception -
either a series of trial-and-error casts,
or an [RTTI](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abenrtti.htm) request for the class name, followed by case branches -
is rather bulky and repetitive.

Catching `cx_sy_no_handler` everywhere also dilutes its original purpose -
to allow frameworks to handle bad plug-in code -
a case that you then may have to handle differently.

## Compromises

Community discussions suggest that
people rather accept refactoring cascades
than being surprised by undeclared exceptions.
As a consequence, we suggest to prefer checked exceptions
to unchecked ones in the way described in the guide.

A different sort of compromise is the mixed-case scenario:
use unchecked exceptions for _internal_ methods
that are fully under your team's control
and where people anticipate them,
and resort to checked exceptions for _borderline_ methods
that may be called from other stakeholders,
to clearly communicate where something can go wrong.
