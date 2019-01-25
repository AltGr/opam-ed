# opam-ed: a small command-line edition tool for the opam syntax

`opam-ed` can read and write files in the general
[opam syntax](http://opam.ocaml.org/doc/2.0/Manual.html#Commonfileformat). It
provides a small CLI with some useful commands for extracting or modifying the
file contents.

The program reads from stdin, writes to stdout, and processes each argument as
one of the commands listed below. See `opam-ed --help` for more options.

### Commands

(as extracted from the generated manpage)

- `get FIELD`
    Print out the value of the named FIELD.

- `field-list`
    List the field names present in the input.

- `field-items FIELD`
    Print out the items of FIELD, understood as a list, separated by
    newlines.

- `get-section SECTION`
    Extract and print the contents of the given SECTION.

- `add FIELD value`
    Add the given FIELD, with the given contents, to the file, if it
    didn't exist already

- `remove FIELD`
    Remove the given FIELD from the file, if present.

- `replace FIELD value`
    Replace the contents of the given FIELD, if found, by the given
    value.

- `add-replace FIELD value`
    Replace the contents of the given FIELD by the given value, adding
    the field if not present already. This is equivalent to the
    sequence 'remove FIELD' 'add FIELD value'

- `append FIELD value`
    Append the given value to the given FIELD, treated as a list. The
    field is created as a singleton if it didn't exist

- `prepend FIELD value`
    Prepend the given value to the given FIELD, treated as a list. The
    field is created as a singleton if it didn't exist

- `map FIELD command`
    Run the given shell command with each member of FIELD, treated as
    a list, as input, and replace it by the output of the command.

- `filter FIELD cmd`
    Run the given shell command with each member of FIELD, treated as
    a list, as input, and remove any member for which the command
    doesn't return 0.

- `replace-item FIELD value replacement`
    Replace the first item of the contents of FIELD, treated as a
    list, that is equal to value, with replacement. Nothing happens if
    value is not a member of the FIELD.

- `add-replace-item FIELD value replacement`
    Replace the first item of the contents of FIELD, treated as a
    list, that is equal to value, with replacement. replacement is
    appended to FIELD if value was not found.

- `remove-item FIELD value`
    Remove the first item of the contents of FIELD, treated as a list,
    that is equal to value, if any.

### License

Copyright 2017-2019 OCamlPro.
opam-ed is distributed under the terms of the GNU General Public License
version 2.1, with the special exception on linking describted in the file
LICENSE.
