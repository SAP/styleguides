# Upper vs. Lower Case

> [Back to the guide](../CleanABAP.md)

We don't have clear guidance on
whether keywords and identifiers should be uppercased or not.

General perception is that we should lowercase keywords
and put identifiers in camel case.
This is the prevalent formatting in most modern programming languages
and commonly perceived as concise and readable style.

```ABAP
data maxWaitTimeInSeconds type i.
```

One of the reasons behind this is that the human eye parses mixed and
lower case faster than upper case, probably due to the more varied
and thus easier to distinguish shapes of the letters.

However, ABAP is case-insensitive,
such that camel-casing identifiers remains a mere optical improvement
that gets lost when pretty-printing and moving the code to the database.

```ABAP
MAXWAITTIMEINSECONDS
```

In response to this, ABAPers decided decades ago to prefer snake case
to camel case, which at least preserves the word borders.

```ABAP
max_wait_time_in_seconds
MAX_WAIT_TIME_IN_SECONDS
```

At the same time, they decided to uppercase ABAP keywords
to make them easier to spot
in black-and-white printouts and on monochrome displays.

```ABAP
DATA max_wait_time_in_seconds TYPE i.
```

Even though large color screens invalidated part of these arguments,
the keywords-uppercase-identifiers-lowercase format
turned into a de facto standard that rules
a vast majority of ABAP's global code base.

The [section _Case_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenlower_upper_case_guidl.htm)
follows this de facto standard.

The [section _Using Pretty Printer_ in the ABAP Programming Guidelines](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/index.htm?file=abenuse_pretty_printer_guidl.htm)
contradicts and says the recommendation _should_ actually be lowercase keywords and uppercase identifiers.
We do see foundation code that prefers lowercased keywords.

In summary, we can only recommend teams to discuss this
and agree on one common style.