using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection
{
    /// <summary>
    /// A singleton class that can be returned by invocation mechanisms to
    /// indicate to a dynamic host language that no value was returned.
    /// </summary>
    /// <remarks>
    /// The standard Type.InvokeMember does not allow a caller to distinguish
    /// between a returned a null reference and no return value. This class is
    /// used as a placeholder by more advanced invocation mechanisms to avoid
    /// this limitation.
    /// </remarks>
    public class VoidReturn
    {
        VoidReturn () {}
        static VoidReturn instance = new VoidReturn();
        public static VoidReturn Instance {
            get {return instance;}
        }
    }
}
