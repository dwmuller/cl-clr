$Id$

Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

CL-CLR is a package that facilitates interaction between Common Lisp
programs and libraries that conform to the Common Language
Specification, running on the Common Language Runtime. It is built on
RDNZL, an excellent package that already solves all of the essential
difficulties of this.  CL-CLR provides some alternative ways of
arranging your code in an attempt to make CLS programming even easier,
more predictable, and more Lisp-like.

CL-CLR uses symbols to represent CLR types, and CLR member names. You
use these symbols in ways that are natural to Lisp programmers. The
member names are used as operators; the type symbols are used very
much like Lisp type symbols. Means are provided to load and track
assemblies, and to bind the contents of an entire namespace to Lisp
symbols.

If you are concerned with loading only as much CLR type infomation as
you need, to keep the size of your image minimal, you can use an
alternative reader syntax which incrementally locates and binds type
information from assemblies. Loading entire namespaces may be more
convenient for interactive exploration of CLR environments, but the
reader syntax is easy to use and provides read-time lookup of type
names in a list of used namespaces.

A symbol representing a type is interned in a package that has the
same name as the type's namespace, prefixed by "CLR!". These packages
are created as needed by CL-CLR, but you could predefine them if
that's more appropriate for your build environment. While CL-CLR is
running, the CLR type object is cached on the symbol's CL-CLR:TYPE
property. You can shut CL-CLR down to release these type objects,
e.g. prior to saving an image. (See the RDNZL documentation for more
on this.) When you re-initialize CL-CLR, the types objects will be
looked up and cached again.

A symbol representing a member name has the same name as the CLS
member, with a preceding period. The period helps distinguish these
symbols from type symbols, avoiding collisions (since type names can't
start with an unescaped period), and is evocative of how members are
referenced in many object-oriented languages. A member symbol is
interned in the package CLR-SYMBOLS, but it is also exported from
every namespace package that has a type with one or more members by
that name.

This arrangement means that you can treat namespaces like packages,
using them where appropriate, and dealing with type name collisions
exactly as you are used to doing already. Tools like Slime will see
only the types and member symbols that are relevant to the package
you're working in. (Although if you're using the alternative reader
syntax, rather than loading entire namespaces, a namespace package's
contents might not be complete.)

CL-CLR does not distinguish between CLR fields, properties, and
methods. They are all treated as functions on objects (if
instance-scoped) or type symbols (if statically scoped). SETF forms
will work with fields and properties in a natural manner. (Although
see the note under CLR Programming Gotchas.)

Examples, assuming that the current package has imported the
referenced CLR symbols from CLR-SYMBOLS:

  ;; Invoke an instance method:
  (|GetAssemblies| app)

  ;; Get/set an instance property or field value:
  (|FullName| obj)
  (setf (|FullName| obj) "NewName")

  ;; Invoke a static method:
  (|GetType| 'clr-symbols:|System.Type| "System.AppDomain")

  ;; Get/set a static property or field value:
  (|CurrentDomain| '|System.AppDomain|)
  (setf (|CurrentDomain| '|System.AppDomain|) new-domain)

Nested type names are denoted using the same conventions that are used
internally by the CLR, using a plus sign to separate the containing
type from the nested type:

  (let ((silly-object (new '|OuterType+InnerType+InnermostType|)))
    ...)

The reader makes it possible to reference the symbols fairly
conveniently, and provides both incremental definition of symbols and
type name resolution against a used namespace list. See that file for
details.


----------------------------------------------------------------------------
Prerequisites

CL-CLR uses CFFI for all interaction between Lisp and the CLR.

At the moment, some additional key functionality is implemented only
for LispWorks. (Specifically, freeing of CLR handles.)

----------------------------------------------------------------------------
Installation

Currently, you need to put the files CommonLispReflection.dll and
CommonLispReflection.DllInterface.dll in the same directory as your
Lisp executable. Then load CL-CLR using ASDF and you're good to go.

Additional assemblies must be located in the same place, or a
directory below it, or in the Global Assembly Cache (GAC).

The eventual plan is to install the CL-CLR DLLs in the GAC, giving you
more flexibility in the placement of your own assemblies.

The file CommonLispReflection.TestLibrary.dll is needed if you want to
run the unit tests, and should be located in the same place as the
other DLLs. You must have DirectX 9.x installed in order to run the
examples.

----------------------------------------------------------------------------
The alternative reader syntax.

You initiate the reader syntax by a call to USE-NAMESPACES, e.g.

(use-namespaces "System"
		"System.Windows.Forms")


The reader expands CLR names prefixed by a question mark, like this:

What             Syntax                   Expansion
---------------------------------------------------
type:            ?typename                type-symbol
member:          ?.membername             member-symbol

Type names can be either namespace-qualified, or simple. If simple,
they must uniquely identify a type in one of the currently used
namespaces. (No partial namespace qualification is allowed.) The
reader resolves type names during the read phase, and signals a clear
error if a type name cannot be resolved or if it has multiple possible
resolutions.

At the end of the file, or at least before any top-level forms are
evaluated that rely on CLR symbols, you must call
BIND-CLR-SYMBOLS. This expands to a form that arranges for the symbols
that were referenced via the reader syntax to be bound to their CLR
concepts when a compiled file is loaded. Passing an optional true
value to BIND-CLR-SYMBOLS causes the names of referenced types and
members to be printed at compile-time, which can be a useful
cross-reference.

Here is an example that creates a Direct3D device:

(use-namespaces "System"
                "System.Drawing"
                "System.Windows.Forms"
                "Microsoft.DirectX"
                "Microsoft.DirectX.Direct3D")

...

(defun make-device (form)
  (let ((presentParams (new '?PresentParameters)))
    (setf (?.Windowed presentParams) t
          (?.SwapEffect presentParams) (?.Discard '?SwapEffect))
    (new '?Device
         0
         (?.Hardware '?DeviceType)
         form
         (?.SoftwareVertexProcessing '?CreateFlags)
         presentParams)))
...

(bind-clr-symbols t)

Notice that fields, properties, and methods are all treated uniformly
like functions or accessors, regardless of whether they are static or
per-instance. To access static members, a type-symbol stands in the
place where an object instance would otherwise be given.

----------------------------------------------------------------------------
CLR Struct types

You must be very careful when working with struct types. They behave
differently from classes.

A good example occurs when working with an array of structs. Assume we
have a Point class with field members X, Y, and Z. Let's create an
array of three points:

(let ((parray (?.CreateInstance '?System.Array
                                (get-type-object '?Point)
				3)))
  ...

Now, let's set the first point, assuming we have a constructor
for Point that takes the initial values for X, Y, and Z:

  (setf (aref* parray 0) (new '?Point 25 12 50))

This works fine. Our next attempt, to modify one of the fields, will
silently fail:

  (setf (?.X (aref* parray 0)) 10)

Why? Because Point is a structure. AREF* actually returns a boxed copy
of the first point in PARRAY; we end up modifying the X value of this
copy, which is immediately lost. The original Point in PARRAY is never
touched. The first form, (setf (aref* ...) ...), works because a SETF
function is defined for AREF*.

There are other situations in which this unexpected boxing of structs
can cause problems. It's a headache for all CLR programmers, but
CL-CLR's dynamic method of interacting with objects can exacerbate the
problems.

----------------------------------------------------------------------------

Reader Dependence

Be aware that if you use the reader syntax, you won't, in general, be
able to redefine functions that depend on it without loading the
entire file. This reminder is relevant if you use Slime or some other
development environment that allows you to evaluate a fragment of code
from a file. The CLR symbols will be mis-interpreted as symbols that
actually start with a question mark, causing run-time errors.

----------------------------------------------------------------------------

Motivations for this project.

This project started as an attempt to provide a layer on Edi Weitz's
RDNZL. I was trying to provide:

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

- Elimination of the need to lookup types from names, other than
  during reading or loading, in all common usage cases.

Some of these features could have been incorporated directly in
RDNZL. However, I eventually wanted to experiment with some low-level
ideas for improving peformance and alternate methods for binding
arguments to parameters, so it became expedient to write my own
substrate. The project owes a deep debt to Edi Weitz and RDNZL,
however.

This package also tries to preserve these important features of RDNZL:

- Small runtime footprint (particularly if the alternative reader is
  used to incrementally discover necessary CLR symbols)

- The ability to save, restore, and re-initialize an image.


