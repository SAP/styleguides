# Function Groups vs. Classes

> [Back to the guide](../CleanABAP.md)

New clean coders routinely ask for clarifying
the advantage of classes over function groups.

Think of a function group as a `global abstract final class`,
with functions, form routines, and global variables
as `static public` members.
 
This yields the following comparison:

- [No instantiation](#no-instantiation)
- [No inheritance](#no-inheritance)
- [No interfaces](#no-interfaces)
- [Weak substitution](#weak-substitution)
- [No overloading](#no-overloading)
- [Weak variable encapsulation](#weak-variable-encapsulation)
- [No method encapsulation](#no-method-encapsulation)

## No instantiation

You cannot create multiple instances of the same function group.
Makes it hard to prevent undesired sideways access to value-carrying fields.

## No inheritance

You cannot inherit from or let inherit function groups.
Prevents implementing some design patterns,
such as [Composite](https://en.wikipedia.org/wiki/Composite_pattern).

## No interfaces

You cannot provide two implementations for the same function group.
Prevents mocking function calls in unit tests without dedicated techniques such as test seams.

> You _can_ provide multiple functions with identical signatures
and exchange them at runtime with dynamic calls,
as described in [weak subsitution](#weak-substitution).
However, there is no real language support for this
and incompatible changes become apparent only at run time.
Compare this to interfaces, where failure to comply
with the signature leads to a syntax error at design time. 

## Weak substitution

You can store function names in variables and call them dynamically,
allowing you to redirect calls to other functions with identical signature.

```ABAP
DATA function_name TYPE char30.
CALL FUNCTION function_name [...]
```

This needs to be planned, though, and does not come as naturally
as in object-oriented designs, making it harder to implement design patterns
that overwrite methods, such as [Decorator](https://en.wikipedia.org/wiki/Decorator_pattern).

## No overloading

You cannot provide two functions with identical names but different parameters.
(This is not possible in ABAP OO either, by the way.)

## Weak variable encapsulation

Function groups seem to hide internal state in "private" variables.
This is usually good enough for everyday programming.

Looking closely however reveals that there is no real memory protection,
and the variables are open to intrusive statements.

```ABAP
ASSIGN ('(<report_name>)gv_global_variable')
  TO <field_symbol>`.
```

Classes do this better by preventing access to private object members. 
because they hide value on instance level with objects. 

## No method encapsulation

While form routines allow you to organize your code,
you cannot hide them from the outside world.

They remain visible to regular statements like: 

```ABAP
PERFORM set_buffer_true
  IN PROGRAM <some_program>.
```

Classes allow you to make methods private,
preventing outside access.

> Originally [answered on StackOverflow](https://stackoverflow.com/questions/55243044/function-groups-vs-classes/55244019#55244019).
