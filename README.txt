This project is an attempt to provide a layer on RDNZL that provides:

- Improved type resolution (avoiding some deficiencies of
  Type::GetType() without modifying RDNZL). CL-CLR searches loaded
  assemblies for type names that are not assembly-qualified, which is
  what the .NET documentation recommends. (This may change to search
  only assemblies explicitly known to CL-CLR.)

- A similar but slightly different reader syntax, which eases the
  provision of ...

- ... read-time type resolution, ...

- ... incremental discovery of 'imports', and ...

- .. uniform syntax for properties and fields (and, in fact, all
  members).

- Elimination of the need to lookup types from names, in all common
  usage cases.


Some of these features could be incorporated directly in RDNZL, and in
fact could perform better that way.

This package also tries to preserve these important features of RDNZL:

- Small runtime footprint (particularly if the alternative reader is
  used to incrementally discover necessary CLR symbols)

- The ability to save, restore, and re-initialize an image.


The reader expands CLR names prefixed by a question mark, like this:

What             Syntax                   Expansion
-----------------------------------------------------------------------
type:            ?+typename               symbol
member:          ?membername              symbol
static member:   ?+typename?membername    (lambda (&rest args)
                                             (apply member 'type args))

(The last entry should be considered *very* experimental; it may be
dropped soon.)

The symbols returned are interned in package CLR-SYMBOLS, and imported
into the current package.

Instead of using the reader syntax, another possible approach is to
import types or namespaces. This creates (if necessary) all relevant
symbols and imports them into the current package. Then you can
program directly with the symbols, which will have to be escaped using
standard Lisp quoting syntax. (I have hacked Slime so that it can do
symbol-completion on mixed-case symbols.)
