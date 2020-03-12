# Modern ABAP Language Elements

As the ABAP language evolves,
new features often go unnoticed by developers.
This document highlights some of the additions
that improve readability of the code
and therefore should be in the tool set
of any professional ABAPer.

The new ABAP statements are only outlined in short;
refer to the ABAP online documentation for more details.

## Content

- [Functional statements and expressions in ABAP](#functional-statements-and-expressions-in-abap)
- [Inline declaration of variables](#inline-declaration-of-variables)
- [Compound Assignment Operators](#compound-assignment-operators)
- [Non-Initial Conditions](#non-initial-conditions)
- [Enumerations](#enumerations)
- [Internal Tables](#internal-tables)
  - [Count lines](#count-table-lines)
  - [Check existence of a table line](#check-existence-of-a-table-line)
  - [Access table key with uncertain result](#access-table-key-with-uncertain-result)
  - [Access table index](#access-table-index)
- [Conditions](#conditions)
  - [Conditional distinction](#conditional-distinction)
  - [Case distinction](#case-distinction)
  - [Case distinctions of reference types class and interface](#case-distinctions-of-reference-types-class-and-interface)
- [Conversions](#conversions)
  - [Cast data references](#cast-data-references)
  - [Create data references](#create-data-references)
  - [Convert data types](#convert-data-types)
  - [Copy fields with matching names](#copy-fields-with-matching-names)
- [Constructors](#constructors)
  - [Construct objects and data](#construct-objects-and-data)
  - [Construct data types](#construct-data-types)
  - [Filter tables](#filter-tables)
- [Character Handling](#character-handling)
  - [Lower and upper case conversion](#lower-and-upper-case-conversion)
- [Flow control](#flow-control)
  - [Loop with control break](#loop-with-control-break)
- [Interface implementation](#interface-implementation)
  - [Behavior on not implemented interface methods](#behavior-on-not-implemented-interface-methods)
  - [Partially implement interfaces in tests](#partially-implement-interfaces-in-tests)

## Functional statements and expressions in ABAP

Functional statements and expressions
have the advantage that they follow the principle of an assignment,
meaning the result is returned as a returning parameter,
thus can be directly used in assignments and can be chained.

Data type information in functional statements
can be abbreviated by a `#`
if the compiler can determine the data type from the context.

> **Keep the clean code principles in mind**
> when using these statements.
> They are very compact and powerful and may tempt you to
> compress your code so much that it becomes unintelligible. 

## Inline declaration of variables

Use the operators
[`DATA`](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abendata_inline.htm)
and
[`FIELD-SYMBOL`](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-us/abenfield-symbol_inline.htm)
to combine the declaration and initial value assignment
of a variable or field symbol.

Allowed at write positions
for which a type can be determined
statically from the context.
This _inferred_ type is given to the declared symbol.

```ABAP
DATA(text) = `This is a string`.

" old style
DATA text TYPE string.
text = `This is a string`.
```

```ABAP
DATA(result) = method_with_returning( ).

" old style
DATA result TYPE accounts_table.
result = method_with_returning( ).
```

```ABAP
method_with_exporting( IMPORTING parameter = DATA(accounts) ).

" old style
DATA accounts TYPE accounts_table.
method_with_exporting( IMPORTING parameter = accounts ).
```

```ABAP
LOOP AT accounts INTO DATA(account).
ENDLOOP.

" old style
DATA account TYPE account_structure.
LOOP AT accounts INTO account.
ENDLOOP.
```

```ABAP
READ TABLE accounts INTO DATA(account_sap) WITH KEY id = 5.

" old style
DATA account_sap TYPE account_structure.
READ TABLE account INTO DATA(account_sap) WITH u id = 5.
```

```ABAP
LOOP AT accounts ASSIGNING FIELD-SYMBOL(<account>).
ENDLOOP.

" old style
FIELD-SYMBOLS <account> TYPE account_structure.
LOOP AT accounts ASSIGNING (<account>).
ENDLOOP.
```

```ABAP
ASSIGN COMPONENT id OF account_sap TO FIELD-SYMBOL(<account_id>).

" old style
FIELD-SYMBOLS <account_id> TYPE account_id_type.
ASSIGN COMPONENT id OF account_sap TO <account_id>.
```

```ABAP
SELECT * FROM t000 INTO TABLE @DATA(clients).

" old style
DATA clients TYPE STANDARD TABLE OF t000.
SELECT * FROM t000 INTO TABLE clients.
```
## Compound Assignment Operators

SAP NetWeaver 7.54 introduces shorthand versions
for arithmetic assignments and string concatenation.
These assignments also allow expressions in the operand position.

Shorthand | Longhand  |
---|---|
x += 1.  | x = x + 1.  |
x -= 1.  | x = x - 1.  |
x *= 1.  | x = x * 1.  |
x /= 1.  | x = x / 1.  |
x &&= \`abc\`. | x = x && \`abc\`. |

## Non-Initial Conditions

The comparison `IS INITIAL` can be omitted in certain places.
This can, for example, be used to shorten Boolean comparisons:

```ABAP
IF is_valid( ).
  " method returned abap_true
ELSE.
  " method returned abap_false
ENDIF.
```

## Enumerations

Define [enumerations](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abaptypes_enum.htm#!ABAP_ADDITION_1@1@) instead of using constants.

```ABAP
TYPES:
  BEGIN OF ENUM scrum_status_type,
    open,
    in_progress,
    blocked,
    done,
  END OF ENUM scrum_status_type.

DATA(scrum_status) = open.
```

Old style:

```ABAP
CONSTANTS scrum_status_open       TYPE i VALUE 1.
CONSTANTS scrum_status_in_process TYPE i VALUE 2.
CONSTANTS scrum_status_blocked    TYPE i VALUE 3.
CONSTANTS scrum_status_done       TYPE i VALUE 4.

DATA scrum status TYPE i.
scrum_status = scrum_status_open.
```

## Internal Tables

### Count table lines

Count the number of lines of an internal table with
[`lines( )`](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abendescriptive_functions_table.htm).

```ABAP
DATA(number_of_lines) = lines( accounts ).
```

Old style:

```ABAP
DATA number_of_lines TYPE i.
DESCRIBE TABLE accounts LINES number_of_lines.
```

### Check existence of a table line

Check the existence of a line in an internal table,
use the function `line_exists( )` within an if-clause.

```ABAP
IF line_exists( accounts[ id = 4711 ] ).
  "line has been found
ENDIF.
```

Old style:

```ABAP
READ TABLE accounts WITH KEY id = 4711 TRANSPORTING NO FIELDS.
IF sy-subrc = 0.
  "line has been found
ENDIF.
```

### Access table key with uncertain result

```ABAP
DATA(account) = VALUE #( accounts[ id = '4711' ] OPTIONAL ).
```

By default, failing functional key accesses throw an exception.
The addition `VALUE ... OPTIONAL` suppresses this.

Old style:

```ABAP
TRY.
    account = accounts[ id = '4711' ]
  CATCH cx_sy_itab_line_not_found.
ENDTRY.
```

### Access table index

Access a specific index of an internal table directly, use the bracket notation `table_name[ ]`.

```ABAP
DATA(id_of_account_5) = accounts[ 5 ]-id.
```

Old style:

```ABAP
READ TABLE accounts INDEX 5 INTO DATA(account_5).
IF sy-subrc = 0.
  DATA(id_of_account_5) = account_5-id.
ENDIF.
```

## Conditions

### Conditional distinction

To evaluate conditions, use the `COND #( )` operator.

```ABAP
DATA(value) = COND #( WHEN status = open THEN 1
                      WHEN status = blocked THEN 3
                      ELSE 7 ).
```

Old style:

```ABAP
DATA value TYPE i.
IF status = open.
  value = 1.
ELSEIF status = blocked.
  value = 3.
ELSE.
  value = 7.
ENDIF.
```

> Alternatively you may use the [function `xsdbool( )`](../CleanABAP.md#use-xsdbool-to-set-boolean-variables)

### Case distinction

Evaluate case distinction with the `SWITCH #( )` operator

```ABAP
DATA(status) = SWITCH #( scrum_status
    WHEN scrum_status_open THEN status_waiting
    WHEN scrum_status_in_process THEN status_busy
    WHEN scrum_status_blocked THEN status_alarm
    WHEN scrum_status_done THEN status_ok ).
```

Old style:

```ABAP
DATA status TYPE status_enum.
CASE scrum_status.
  WHEN scrum_status_open.
    status = status_waiting.
  WHEN scrum_status_in_process.
    status = status_busy.
  WHEN scrum_status_blocked.
    status = status_alarm.
  WHEN scrum_status_done.
    status = status_ok.
ENDCASE.
```

### Case distinctions of reference types class and interface

Switch on a reference types class and interface using the `CASE` extension `TYPE OF`.

```ABAP
CASE type of account.
  WHEN TYPE bank_account INTO DATA(bank_account).
    " some processing ...
  WHEN OTHERS.
    " some processing ...
ENDCASE.
```

In a condition e.g. in an `IF` statement the `IS INSTANCE OF` operator can be used.

```ABAP
IF account IS INSTANCE OF bank_account.
  " some processing ...
ENDIF.
```

## Conversions

### Cast data references

Cast reference types to other reference types using the `CAST #( )` operator.

```ABAP
DATA(my_account) = CAST account( NEW bank_account( ) ).
```

Old style:

```ABAP
DATA my_account TYPE REF TO account.
CREATE OBJECT my_account TYPE bank_account.
```

or

```ABAP
DATA my_account TYPE REF TO account.
DATA my_bank_account TYPE REF TO bank_account.
CREATE OBJECT my_bank_account.
my_account ?= bank_account.
```

### Create data references

Create data references to structures and tables with the operator `REF #( )`.

```ABAP
DATA accounts TYPE accounts_table.
import_accounts_references( REF #( accounts ) ).
```

Old style:

```ABAP
DATA accounts TYPE accounts_table.
DATA accounts_reference TYPE REF TO accounts_type.
GET REFERENCE OF accounts INTO accounts_reference.
import_accounts_references( accounts_reference ).
```

### Convert data types

Use the operator `CONV #( )` to convert data types and save temporary variables.

```ABAP
method_takes_string( CONV #( a_char ) ).
```

Old style:

```ABAP
DATA a_string TYPE string.
a_string = a_char.
method_takes_string( a_string ).
```

### Copy fields with matching names

Copy fields with matching names from one data type to another with `corresponding #( )`.

```ABAP
target_structure = CORRESPONDING #( source_structure ).
```

Old style:

```ABAP
MOVE-CORRESPONDING source_structure TO target_structure.
```

> Caution: The two statements differ in behavior.
> The `CORRESPONDING( )` statement is a constructor statement, meaning all fields in the `target_structure` are initialized before the corresponding `source_structure` values are copied to the `target_sructure`
> The `MOVE-CORRESPONDING` statement in contrast leaves the content of the not matching fields in the `target_structure` untouched.

## Constructors

### Construct objects and data

Construct objects and data with the `NEW #( )` operator.

```ABAP
DATA(account) = NEW cl_account( ).

DATA(dref) = NEW struct_type( component_1 = 10
                              component_2 = 'a' ).
```

```ABAP
DATA(account) = CAST if_account( NEW cl_account( ) ).

DATA data_structure TYPE REF TO struct_type.
CREATE DATA data_structure.
data_reference->component_1 = 10.
data_reference->component_2 = 'a'.
```

Old style:

```ABAP
DATA account TYPE REF TO cl_account.
CREATE OBJECT account.
```

```ABAP
DATA account TYPE REF TO if_account.
CREATE OBJECT account TYPE cl_account.
```

### Construct data types

Construct structures and tables using the `VALUE #( )` operator.
It also constructs initial values for most data types.

> This statement is a life saver when writing ABAP unit tests.

Structure:

```ABAP
DATA(account) = VALUE account_structure( id = 5
                                         name = 'SAP' ).
```

Old style:

```ABAP
DATA account TYPE account_structure.
account-id = 5.
account-name = 'SAP'.
```

Table:

```ABAP
DATA(accounts) = VALUE accounts_table( ( id = 5  name = 'SAP' )
                                       ( id = 6  name = 'ABCDE' ) ).
```

Old style:

```ABAP
DATA accounts TYPE accounts_table.
DATA account TYPE account_structure.
account-id = 5.
account-name = 'SAP'.
INSERT account INTO TABLE accounts.
account-id = 6.
account-name = 'ABCDE'.
INSERT ACCOUNT INTO TABLE accounts.
```

Construct tables based on other tables:

```ABAP
result = VALUE #( FOR row IN input ( row-text ) ).
```

Old style:

```ABAP
LOOP AT input INTO DATA(row).
  INSERT row-text INTO TABLE result.
ENDLOOP.
```

### Filter tables

Construct a table as a subset of another stable using `FILTER #( )`.

```ABAP
bank_accounts = FILTER #( accounts
                          WHERE account_type = 'B' ).
```

Old style:

```ABAP
DATA bank_account TYPE bank_account.
LOOP AT accounts INTO bank_account WHERE account_type = 'B'.
  INSERT bank_account INTO TABLE bank_accounts.
ENDLOOP.
```

## Character Handling

### Lower and upper case conversion

Convert characters between cases using `to_upper( )` or `to_lower( )`.

```ABAP
DATA(uppercase) = to_upper( lowercase ).
DATA(lowercase) = to_lower( uppercase ).
```

Old style:

```ABAP
TRANSLATE lowercase TO UPPER CASE.
```

## Flow control

### Loop with control break

Process data on defined groups using the new features with the `loop` statement extension `group by ...` and `loop at group`.

```ABAP
LOOP AT accounts INTO DATA(account) GROUP BY grouping_id.
  " once per group before group ...
  LOOP AT GROUP account INTO DATA(account_group).
    " for each group member ...
  ENDLOOP.
  " once per group after group ...
ENDLOOP.
```

Old style:

```ABAP
DATA previous_grouping_id TYPE i.
DATA last_account TYPE account.
LOOP AT accounts INTO data(account).
  IF account-grouping_id <> previous_grouping_id.
    previous_grouping_id = account-grouping_id
    " once per group before group ...
    IF last_account IS NOT INITIAL.
      " once per group after group ...
    ENDIF.
  ENDIF.
  " for each group member ...
  last_account = account.
ENDLOOP.
IF last_account IS NOT INITIAL.
  " once per group after group
ENDIF.
```

## Interface implementation

### Behavior on not implemented interface methods

Define the effect on not implemented interface methods.

Add `DEFAULT IGNORE` to advise ABAP to handle the call to this method if not implemented as a call to an empty implementation.

```ABAP
INTERFACE account.
  METHODS new_method DEFAULT IGNORE.
ENDINTERFACE.
```

Add `DEFAULT FAIL` to advise ABAP to raise an exception `CX_SY_DYN_CALL_ILLEGAL_METHOD` if the not implemented method is called. This is the default behavior.

```ABAP
INTERFACE account.
  METHODS new_method default FAIL.
ENDINTERFACE.
```

### Partially implement interfaces in tests

Implement in tests for e.g. test doubles only the interface methods which you need and skip the not needed with the `partially implemented` extension of the `interfaces` statement.

```ABAP
INTERFACE account.
  METHODS add_account IMPORTING account TYPE account.
  METHODS delete_account IMPORTING account_id TYPE account_id.
  METHODS get_account IMPORTING account_id TYPE account_id
                      RETURNING value(result) TYPE account.
ENDINTERFACE.

CLASS test_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
  INTERFACES account PARTIALLY IMPLEMENTED.
  DATA account_stub TYPE account.
ENDCLASS.

CLASS test_double IMPLEMENTATION.
  METHOD productive~get.
    result = account_stub.
  ENDMETHOD.
ENDCLASS.
```
