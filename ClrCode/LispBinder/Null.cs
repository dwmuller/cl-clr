using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection
{
    /// <summary>
    /// This class is used to only for it's type, which we treat as the
    /// run-time "type" of the null pointer.
    /// </summary>
    /// <remarks>
    /// typeof(Null) corresponds to Lisp's NULL system type. We treat NIL
    /// equivalently to a null pointer, but also allow it to be implicitly
    /// converted to the boolean false value.
    /// </remarks>
    class Null
    {
        /// <summary>
        /// The default (and only) constructor is private. We use this class
        /// for its unique type, not its value.
        /// </summary>
        private Null() { }
    }
}
