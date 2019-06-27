# Composition vs. Inheritance

## Abstract Classes vs. Interfaces

```ABAP
CLASS /clean/blog_post DEFINITION PUBLIC ABSTRACT CREATE PROTECTED.
  PUBLIC SECTION.
    METHODS publish ABSTRACT.
ENDCLASS.

CLASS /clean/blog_post IMPLEMENTATION.
ENDCLASS.
```

```ABAP
INTERFACE /clean/blog_post.
  METHODS publish.
ENDINTERFACE.
```