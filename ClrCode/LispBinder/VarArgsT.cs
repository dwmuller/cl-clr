using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection
{
    /// <summary>
    /// Represents a collection of arguments for a member invocation that
    /// accepts a variable number of arguments.
    /// </summary>
    /// <typeparam name="ELEM_TYPE"></typeparam>
    /// <remarks>
    /// An object of this type must be used only as the last argument in an
    /// argument array. The LispBinder will match a VarArgs object to a
    /// parameter with the ParamArray attribute. The elements of the
    /// VarArgs.Array will become the trailing arguments in an invocation. The
    /// LispBinder will never match a plain array to a ParamArray parameter,
    /// as C# does.
    /// </remarks>
    public class VarArgsT<ELEM_TYPE> : VarArgs
    {
        ELEM_TYPE[] args;
        public VarArgsT(ELEM_TYPE[] obj)
        {
            args = obj;
        }
        public override Array Args
        {
            get
            {
                return args;
            }
        }
    };

}
