#### Function Groups vs. Classes

> [Back to the guide](../CleanABAP.md)

New clean coders routinely ask for clarifying
the advantage of classes over function groups.

Think of a function group as a `global abstract final class` with functions as `static public` members
and form routines and global variables as `static private` members -
with all positive and negative aspects this entails:

- **No instantiation.**
You cannot create multiple instances of the same function group.
Makes it hard to prevent undesired sideways access to value-carrying fields.

- **No inheritance.**
You cannot inherit from or let inherit function groups.
Prevents implementing some design patterns,
such as [Composite](https://en.wikipedia.org/wiki/Composite_pattern).

- **No interfaces.**
You cannot provide two implementations for the same function group.
Prevents mocking function calls in unit tests without dedicated techniques such as test seams.

- **No substitution.**
You cannot exchange a call to one function with a call to another one with different name but identical signature.
Makes it hard to implement design patterns that overwrite methods,
such as [Decorator](https://en.wikipedia.org/wiki/Decorator_pattern).

- **No overloading.**
You cannot provide two functions with identical names but different parameters.
(This is not possible in ABAP OO too, by the way.)

- **Variable encapsulation.**
Function groups can hide internal state in private variables.
Classes can do this a little better because they hide value on instance level with objects. 

- **Method encapsulation.**
Function groups can hide internal methods ("form routines");
they are equal to classes in this respect.

> Originally [answered on StackOverflow](https://stackoverflow.com/questions/55243044/function-groups-vs-classes/55244019#55244019).