# Exceptions

What type of exception to use is a hot topic
that has been discussed several times,
for example in [Issue 16](https://github.com/SAP/styleguides/issues/16).

This page describes the reasoning behind our current recommendations.

## The Ideal

According to discussions in the community,
the optimal way to deal with exceptions would be:

```ABAP
METHODS lower_method
  RAISING
    cx_flexible_exception.

METHOD lower_method.
  RAISE EXCEPTION NEW cx_flexible_exception( ).
ENDMETHOD:

METHOD middle_method.
  lower_method( ).
ENDMETHOD.

METHOD upper_method.
  TRY.
      middle_method( ).
    CATCH cx_flexible_exception.
      " ...
  ENDTRY.
ENDMETHOD.
```

We want to declare the exception in
the `lower_method` that actually throws it,
to not surprise callers.

Then we want to let the exception bubble upwards through `middle_method`s
without forcing them to redeclare or catch the exception.
This avoids refactoring cascades if exceptions change.

Finally, we want to catch the exception in some `upper_method` and handle it.

This is also exactly what Robert C. Martin advertises for Java.

## What's in the Way

However, excactly this way is not possible in ABAP
because none of the available three exception types supports it:

- `cx_static_check` forces `middle_method`
to redeclare the `cx_flexible_exception`.
Although this is "only a warning",
it results in an issue with Very High priority
that will prevent transport release in standard system setups.
Even if you are willing to accept this,
the code will not actually work but dump in `middle_method`
because there is no catch block.

- `cx_dynamic_check` doesn't improve this.
Alhough it accepts the missing redeclaration
and catch block in `middle_method`,
the code will still dump here if the exception is actually thrown.

- `cx_no_check` also doesn't help.
Although the method bodies work,
the `METHODS lower_method` definition now is
no longer allowed to declare `cx_flexible_exception`
and compilation fails with a syntax error.

## The Compromise

Community discussions suggest that
they will rather accept refactoring cascades
than be surprised by undeclared exceptions.
As a consequence, we suggest to prefer checked exceptions
to unchecked ones.