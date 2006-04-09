This project is an experiment in an alternative to RDNZL.

Shortcomings that remain to be solved:

- Object identity. RDNZL does not deal with this. GCHandles converted to
  IntPtrs mask identity (you get a different IntPtr on each call for the
  same object), DotNetContainer proxy objects (the containers in RDNZL)
  mask identity again, and the Lisp class instances used to identifiably
  contain proxy references and facilitate finalization masks it yet again.
  Object.ReferenceEquals still works, but object identity in a Lisp sense
  may be a goal too far to be worth achieving. You'd have to do one hash
  lookup at this level to go from object to handle, and another to go
  from handle to Lisp object. You can't do just one or the other. You
  can eliminate one lookup if you can find a way to represent the Lisp
  object directly for storage down here.


A more Lisp-like binding of methods is a primary goal. Another one is
reduced memory management overhead for object representations.

Member selection is modelled partially after CLOS's algorithms for
selecting a primary method, but borrows from C#'s overload resolution 
algorithms in order to deal with the CLR's very different notions of
class and interface inheritance, and type convertabiliy. The first
argument of an instance member invocation is necessarily handled a bit
differently; we don't directly control that here. (Although we could,
if we really wanted to!) It provides the namespace within which members
are initially looked up, according to the normal CLI rules. Nil translates
to a null pointer, the type of which we treat as the subclass-of-all-
subclasses, as in Lisp. We also allow null pointers to represent a false
boolean value.

Lisp values are converted based primarily on the Lisp object's type as
follows:
FLOAT      Single or Double, depending on type and its range.
CHARACTER  Char
RATIO      Double
INTEGER    Byte, Int16, Int32, Int64, or Decimal depending on value. 
(EQ T)     Boolean (true)
NULL       null pointer (can bind to Boolean as false value)

STRING     String
PATHNAME   String

ARRAY      ???
LIST       ???
FUNCTION   ???
STREAM     ???
SYMBOL     ???

Applicable members are selected based on name, accessibility, and
the availability of a (non-narrowing) type conversion from each
argument type to its corresponding parameter type. This is complicated
slightly by 'params' parameters, aka variable-length parameter lists.

The best applicable member is selected using the C# rules for
"better conversions", but giving precedence to arguments earlier
in the argument list, as CLOS does. This means that some situations
which would be ambiguous in C# are not in Lisp.
    
The C# rules then include a list of rules involving the non-CLS
types, which we omit here but may or may not choose to implement
at some point.

If none of the above apply, then neither conversion is better.

We consider only what C# calls implicit numeric conversions and
implicit reference conversions.

We do not support C#'s implicit enumeration conversions (too confusing),
boxing conversions (unnecessary, since value types are already boxed
here), or implicit constant conversions (again, unnecessary). We also do 
not support user-defined implicit conversion via op_Implicit; these
must be explicitly requested by the programmer via casting.
