# Avoid Encodings

> [Back to the guide](../CleanABAP.md)

We encourage you to get rid of _all_ encoding prefixes.

```ABAP
METHOD add_two_numbers.
  result = a + b.
ENDMETHOD.
```

instead of the needlessly longer

```ABAP
METHOD add_two_numbers.
  rv_result = iv_a + iv_b.
ENDMETHOD.
```

> Read more in _Chapter 2: Meaningful Names: Avoid Encodings_ of [Robert C. Martin's _Clean Code_].
> The examples in this style guide are written without prefixes to demonstrate the value.

> This section contradicts the sections [_Names of Repository Objects_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abennames_repos_obj_guidl.htm)
> and [_Program-Internal Names_](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenprog_intern_names_guidl.htm)
> of the ABAP Programming Guidelines which recommend to use prefixes.
> We think that avoiding prefixes is the more modern and readable variant and that the guideline should be adjusted.

> Prefixing is one of the most controversially discussed topics in ABAP.
> [[1]](https://blogs.sap.com/2009/08/30/nomen-est-omen-abap-naming-conventions/)
> [[2]](https://blogs.sap.com/2016/02/05/fanning-the-flames-prefixing-variableattribute-names/)
> [[3]](https://blogs.sap.com/2018/04/30/are-30-characters-enough-to-make-your-code-better/)
> [[4]](https://blogs.sap.com/2018/05/11/all-your-abap-prefixes-are-belong-to-us/)

[Robert C. Martin's _Clean Code_]: https://www.oreilly.com/library/view/clean-code/9780136083238/

## Reasoning

SAP has a bad, pervasive legacy practice of adding prefixes to each and everything, to encode things like

- kind, such as "cl_" for classes,
- direction, such as "is_" for an input parameter,
- scope, such as "mo_" for a class member,
- type, such as "lt_" for a table-like variable, and
- mutability, such as "sc_" for a constant.

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

- Different team styles create confusion: is "lr_" an object reference or a range table?
  You'll stumble over this in code that connects different things, for example your determinations within BOPF.

- Changes create needless work: turning a table from `STANDARD` to `SORTED` shouldn't require you
  to rename all variables from "lt_" to "lts_".

- Prefixing doesn't make it easier to tell global from local things.
  If you fill a `gt_sum` from an `lt_sum`, both are still only sums and it's not clear what distinguishes the two.
  The better idea is to fill a `total_sum` from a `partial_sum`, or an `overall_result` from an `intermediate_result`.
  The name confusion described in
  [section _Program-Internal Names_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenprog_intern_names_guidl.htm)
  should thus be solved otherwise.

- If you follow Clean Code, your methods will become so short (3-5 statements)
  that prefixing is no longer necessary to tell importing from exporting parameters and local from global variables.

- The ABAP foundation doesn't prefix anymore, for example you won't find importing/exporting prefixes
  on the method parameters in `cl_abap_math`.

- Other languages like Java use absolutely no prefixes, and still Java code is perfectly readable.

## Compromises

There is only one prefix that ABAP forces you to use: your application's namespace,
to avoid conflicts with objects from other teams in the global dictionary, where every thing needs a unique name.

If this rule is too hard for you, consider a compromise:
avoid encodings in local contexts (within a method body, method parameters, local classes, etc.),
and apply them only to global objects that are stored in the same global Dictionary namespace.

We agree that following this suggestion will work out only if the code is already _clean_ in some other aspects,
especially short methods and good method and variable names.
While prefixes needlessly complicate a clean method with two statements,
they may be your only remaining lifeline in a thousand-line legacy function with cryptic variable names.
