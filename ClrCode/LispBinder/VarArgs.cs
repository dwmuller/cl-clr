using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection
{
    
    /// <summary>
    /// Wrapper class for an array representing a varying argument list.
    /// </summary>
    /// <remarks>
    /// This class is used to wrap a final argument in a call to indicate that
    /// it should be treated as a list of varying arguments. The call can then
    /// match only members whose final parameter is marked with the
    /// ParamArrayAttribute, and then only if the object wrapped by VarArgs
    /// matches the type of that parameter.
    ///
    /// This is abstract. The derived class, VarArgsT, is generic and takes
    /// the element type as a generic argument. We need to do this so that
    /// LispBinder can get a distinct Type object that includes information
    /// about the element type.
    /// </remarks>
    public abstract class VarArgs
    {
        protected VarArgs() { }
        public abstract Array Args
        {
            get;
        }
        public static Type TypeToArrayType(Type type)
        {
            return TypeToElementType(type).MakeArrayType();
        }
        public static Type TypeToElementType(Type type)
        {
            return type.GetGenericArguments()[0];
        }
        public static VarArgs Wrap(Array args)
        {
            // See http://weblogs.asp.net/pwelter34/archive/2005/08/13/422482.aspx;
            Type generic_type = typeof(VarArgsT<>);
            return (VarArgs)Activator.CreateInstance(
                generic_type.MakeGenericType(args.GetType().GetElementType()),
                args);
        }
    };

}
