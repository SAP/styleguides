# Avoid Encodings

> [Back to the guide](../CleanABAP.md)

- [Abstract](#abstract)
- [Reasoning](#reasoning)
- [Arguments](#arguments)
- [Avoiding name clashes](#avoiding-name-clashes)
- [Compromises](#compromises)

## Abstract

We encourage you to get rid of _all_ encoding prefixes.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

instead of the needlessly longer

```ABAP
" anti-pattern
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> Read more in _Chapter 2: Meaningful Names: Avoid Encodings_ of [Robert C. Martin's _Clean Code_].
> The examples in this style guide are written without prefixes to demonstrate the value.
>
> This section contradicts the sections [_Names of Repository Objects_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abennames_repos_obj_guidl.htm)
> and [_Program-Internal Names_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenprog_intern_names_guidl.htm)
> of the ABAP Programming Guidelines which recommend to use prefixes.
> We think that avoiding prefixes is the more modern and readable variant and that the guideline should be adjusted.
>
> Prefixing is one of the most controversially discussed topics in ABAP.
> [[1]](https://blogs.sap.com/2009/08/30/nomen-est-omen-abap-naming-conventions/)
> [[2]](https://blogs.sap.com/2016/02/05/fanning-the-flames-prefixing-variableattribute-names/)
> [[3]](https://blogs.sap.com/2018/04/30/are-30-characters-enough-to-make-your-code-better/)
> [[4]](https://blogs.sap.com/2018/05/11/all-your-abap-prefixes-are-belong-to-us/)

[Robert C. Martin's _Clean Code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## Reasoning

With ABAP there is a legacy practice of adding prefixes to each and everything, to encode things like

- `cl_` for classes
- `if_` for interfaces
- `is_` for an importing parameter
- `mo_` for a class member attribute
- `lt_` for a table-like variable
- `sc_` for a constant
- ...

This kind of prefixing is a relic from the early days of programming, when code was printed out and read on paper,
and you didn't want to flip around just to find some variable's type.
Modern development environments give easy access to data types, signatures, and object navigation,
such that it is no longer needed to get readable code.

## Arguments

Before you disagree, consider these:

- ABAP's 30 character limitation makes it hard enough to squeeze meaningful names
  into the available space without wasting another 3-4 characters for needless encodings.

- The disputes that arise over prefixes are not worth the effort:
  whether your constant starts with `sc_` or `cv_` does not really influence readability.

- Squeezing the at least five dimensions of variables
  (kind, direction, scope, type, mutability),
  into usually no more than two character prefixes
  leads to pointless conflicts.
  
- Different team styles create confusion:
  is `lr_` an object reference or a range table?
  You'll stumble over this in code that connects different things,
  for example your determinations within BOPF.

- Prefixes can be misleading.
  For example, despite their names,
  the "importing data" `id_business_partner`
  has nothing to do with the business partner's ID,
  and the "large object string" `lost_file_content`
  is not lost at all.

- Changes create needless work: turning a table from `STANDARD` to `SORTED` shouldn't require you
  to rename all variables from `lt_` to `lts_`.
  
- Prefixing doesn't make it easier to tell global from local things.
  If you fill a `gt_sum` from an `lt_sum`, both are still only sums and it's not clear what distinguishes the two.
  The better idea is to fill a `total_sum` from a `partial_sum`, or an `overall_result` from an `intermediate_result`.
  The name confusion described in
  [section _Program-Internal Names_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenprog_intern_names_guidl.htm)
  should thus be solved otherwise.

- Prefixing doesn't make it easier to recognize data types.
  For example, most Hungarian ABAP encodings
  distinguish variables from structures and tables,
  but don't reflect the way more important differentiation
  between floating point and packed numbers.

- If you follow Clean Code, your methods will become so short (3-5 statements)
  that prefixing is no longer necessary to tell importing from exporting parameters and local from global variables.

- The ABAP foundation doesn't prefix anymore,
  for example you won't find importing/exporting prefixes
  on the method parameters in `cl_abap_math`.

- Other languages like Java use absolutely no prefixes,
  and still Java code is perfectly readable.

## Avoiding name clashes 

One consequence of removing the prefixes is a potential name clash of entities which are in the the same namespace. So far it was a common practice to have interfaces and classes named identical, only differentiated by the prefix.

While this practice was not correct in the first place, as an interface is something more generic than an implementing class, there is now the need to be more precise in the naming of entities.

Name the interface more generic and the implementing classes more specific:

```ABAP
interface game_board.
  ...
endinterface.

class game_board_as_list definition.
  public section.
    interfaces game_board.
  ...
endclass.

class game_board_as_array definition.
  public section.
    interfaces game_board.
  ...
endclass.
```

To avoid name clashes with method e.g. importing parameters use the self reference `me->`:

```ABAP
class game_board_as_list definition.
  public section.
    methods constructor
      importing x_dimension type i
                y_dimension type i.
  private section.
    data x_dimension type i.
    data y_dimension type i.
endclass.

class game_board_as_list implementation.
  method constructor.
    me->x_dimension = x_dimension.
    me->y_dimension = y_dimension.
  endmethod.
endclass.
```

For tables and structures use singular and plural:

```ABAP
types: begin of coordinate,
         x type i,
         y type i,
       end of coordinate.
type coordinates type standard table of coordinate with default key.
```

## Compromises

The only prefix that ABAP forces you to use is your application's namespace,
to avoid conflicts with objects from other teams in the global dictionary, where every thing needs a unique name.

If this rule is too hard for you, consider a compromise:  
Avoid encodings in local contexts (within a method body, method parameters, local classes, etc.),
and apply them only to global objects that are stored in the same global Dictionary namespace.

We agree that following this suggestion will work out only if the code is already _clean_ in some other aspects,
especially short methods and good method and variable names.
While prefixes needlessly complicate a clean code with extra three characters in names,
they may be your only remaining lifeline in a thousand-line legacy function with cryptic variable names.
