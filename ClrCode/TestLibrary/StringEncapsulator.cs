using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    /// <summary>
    /// Encapsulates a string. An object of this type is used in unit tests
    /// that need a reference type. They can easily verify an object's
    /// identity via the contained string.
    /// </summary>
    public class StringEncapsulator
    {
        string name;
        public StringEncapsulator(string name)
        {
            this.name = name;
        }
        public override string ToString()
        {
            return name;
        }
    }
}
