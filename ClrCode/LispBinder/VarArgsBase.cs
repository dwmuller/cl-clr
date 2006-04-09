using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection
{
    // This class is used to wrap a final argument in a call to indicate
    // that it should be treated as a list of varying arguments. The call
    // can then match only members whose final parameter is marked with
    // the ParamArrayAttribute, and then only if the object wrapped by
    // VarArgs matches the type of that parameter.
    public abstract class VarArgsBase
    {
        protected VarArgsBase() { }
        public abstract Array Args
        {
            get;
        }
        public abstract Type ElementType
        {
            get;
        }
        public static VarArgsBase Wrap(Array args)
        {
            // See http://weblogs.asp.net/pwelter34/archive/2005/08/13/422482.aspx;
            Type generic_type = typeof(VarArgs<int>).MakeGenericType();
            return (VarArgsBase)Activator.CreateInstance(
                generic_type.MakeGenericType(args.GetType().GetElementType()),
                args);
        }
    };

}
