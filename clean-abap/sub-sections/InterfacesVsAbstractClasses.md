# Interfaces vs. Abstract Classes

Although interfaces and abstract classes share some properties,
they are not equivalent and should not be confused.
In short, you might say that:

Interfaces were designed to share definitions,
while abstract classes were designed to share implementations.

In reverse, interfaces vs. abstract classes is not an either-or-question.
The two serve different purposes that complement each other
and can be added up nicely to clean patterns.

## Interfaces

```ABAP
INTERFACE /clean/blog_post.
  PUBLIC.
    METHODS publish.
ENDINTERFACE.
```

Interfaces allow connecting any kind of code,
as long as it satisfies the interface.

They don't impose any
side expectations or restrictions upon the implementation.

In turn, they don't help implementing the required methods
with default code or helper methods.

```ABAP
CLASS /clean/specific_blog_post DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /clean/blog_post.
ENDCLASS.

CLASS /clean/specific_blog_post IMPLEMENTATION.
  
  METHOD publish.
  ENDMETHOD.
  
ENDCLASS.
```

## Abstract classes

```ABAP
CLASS /clean/blog_post DEFINITION PUBLIC ABSTRACT CREATE PROTECTED.
  PUBLIC SECTION.
    METHODS publish ABSTRACT.
ENDCLASS.

CLASS /clean/blog_post IMPLEMENTATION.
ENDCLASS.
```

Abstract classes allow connecting only code that
fits the inheritance pattern.

On one hand, they unify code and design across objects-of-a-kind,
making it easier to implement things by providing default implementations,
frames, and helper methods.

On the other hand, they squeeze sub-classes into their predefined scheme,
and force them to accept any code they provide,

```ABAP
CLASS /clean/specific_blog_post DEFINITION
    PUBLIC CREATE PUBLIC
    INHERITING FROM /clean/blog_post.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS publish REDEFINITION.
ENDCLASS.

CLASS /clean/specific_blog_post IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.
  
  METHOD publish.
  ENDMETHOD.
  
ENDCLASS.
```

## Comparison

- An abstract class needs more code,
even if it is completely empty,
because you always have to provide an `IMPLEMENTATION`.

- A class can implement multiple interfaces,
but can inherit only one interface-like abstract class.

- Inheriting from the interface-like abstract class
disables inheriting from other classes,
preventing the sub-class from exploiting inheritance for other aspects.

- Sub-class constructors must call `super->constructor( )`,
even if the abstract class does not have a constructor.

- The abstract class has power over the sub-class's instantiation behavior,
being able to suppress instantiation completely with `CREATE PRIVATE`.

- The abstract class has power over the sub-class's code,
being able to add members and constructor code that may
simplify but also interfere with or even break the sub-class's code.
