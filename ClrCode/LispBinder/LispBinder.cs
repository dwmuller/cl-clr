using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Globalization;
using System.Diagnostics;
using TypeList = System.Collections.Generic.List<System.Type>;
using TypeConversionDictionary
    = System.Collections.Generic.SortedList<System.Type,
                                            System.Collections.Generic.List<System.Type>>;

namespace SpookyDistance.CommonLispReflection
{
    /// <summary>
    /// An implementation of System.Binder that uses Lisp-like rules for dynamic
    /// member invocation.
    /// </summary>
    public class LispBinder : Binder
    {

        #region Constructors

        /// <summary>
        /// Construct a LispBinder instance with specific conversion behaviors.
        /// </summary>
        /// <param name="implicit_double_narrowing">
        /// When true,indicates that a double argument value may be implicitly
        /// downcast to a single in order to match a parameter of type single.
        /// This is helpful when using a host language that does not support
        /// single floating values, such as LispWorks' implementation of Lisp on
        /// Windows.
        /// </param>
        /// <param name="implicit_enum_conversion">
        /// When true, allows integer argument values to be implicitly converted
        /// to Enum-derived types in order to match parameters. It is usually
        /// more efficient to represent enums as integer values in host
        /// languages, and this automatic conversion eliminates the need for
        /// explicit conversion by the caller.
        /// </param>
        public LispBinder(bool implicit_double_narrowing,
                          bool implicit_enum_conversion)
        {
            this.implicit_double_narrowing = implicit_double_narrowing;
            this.implicit_enum_conversion = implicit_enum_conversion;
            init();
            if (implicit_double_narrowing)
                AddNumericConversions(typeof(Double), typeof(Single));
        }
        
        /// <summary>
        /// Default constructor. Disables implicit double narrowing, enables
        /// implicit conversion from integers to enums.
        /// </summary>
        public LispBinder()
        {
            implicit_double_narrowing = false;
            implicit_enum_conversion = true;
            init();
        }


        #endregion
        #region System.Reflection.Binder Methods

        public override MethodBase SelectMethod(BindingFlags bindingAttr,
                                                MethodBase[] match,
                                                Type[] types,
                                                ParameterModifier[] modifiers)
        {
            if (match == null || types == null)
                throw new ArgumentNullException();
            object result = SelectMethods(match, true, ParamsFromMethodBase,
                                           types,
                                           TypeFromType);
            MethodInfo method = result as MethodInfo;
            if (method != null)
                return method;
            MemberInfo[] methods = result as MemberInfo[];
            if (methods != null)
                throw new AmbiguousMatchException("Multiple matches.");
            return null;
        }
        public override FieldInfo BindToField(BindingFlags bindingAttr,
                                              FieldInfo[] match,
                                              object value,
                                              CultureInfo culture)
        {
            return Type.DefaultBinder.BindToField(bindingAttr, match, value, culture);
        }
        public override PropertyInfo SelectProperty(BindingFlags bindingAttr,
                                                    PropertyInfo[] match,
                                                    Type returnType,
                                                    Type[] indexes,
                                                    ParameterModifier[] modifiers)
        {
            if (match == null || indexes == null)
                throw new ArgumentNullException();
            object result = SelectMethods(match, true, ParamsFromPropertyInfo,
                                           indexes, TypeFromType);
            PropertyInfo method = result as PropertyInfo;
            if (method != null)
                return method;
            MemberInfo[] methods = result as MemberInfo[];
            if (methods != null)
                throw new AmbiguousMatchException("Multiple matches.");
            return null;
        }
        public override MethodBase BindToMethod(BindingFlags bindingAttr,
                                                MethodBase[] match,
                                                ref object[] args,
                                                ParameterModifier[] modifiers,
                                                CultureInfo culture,
                                                String[] names,
                                                out object state)
        {
            state = null;
            if (match == null || args == null)
                throw new ArgumentNullException();
            object result = SelectMethods(match, true, ParamsFromMethodBase,
                                           args, TypeFromObject);
            MemberInfo[] methods = result as MemberInfo[];
            if (methods != null)
                throw new AmbiguousMatchException("Multiple matches.");
            MethodBase method = result as MethodBase;
            if (method == null)
                return null;
            object[] new_args = BindArgs(method.GetParameters(), args);
            if (new_args != args)
                state = args;
            args = new_args;
            return method;
        }
        public override void ReorderArgumentArray(ref object[] args,
                                                  object state)
        {
            object[] old_args = state as object[];
            if (old_args != null && old_args != args)
            {
                // Copy values back to original array, and give that back
                // to the caller. This should only occur when a variable
                // argument list was involved.

                // Copy required args first.
                Array.Copy(args, old_args, args.Length - 1);

                // Now copy the variable args.
                Array varargs = (Array)(args[args.Length - 1]);
                Array.Copy(varargs, 0, old_args, args.Length -1, varargs.Length);
                
                // Done.
                args = old_args;
            }
        }

        public override object ChangeType(object value,
                                          Type type,
                                          CultureInfo culture)
        {
            return ConvertTo(value, type);
        }

        #endregion
        #region Public Methods

        /// <summary>
        /// Similar to Type.InvokeMember, with a number of enhancements useful
        /// to dynamic languages such as Lisp.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="name"></param>
        /// <param name="bflags"></param>
        /// <param name="binder"></param>
        /// <param name="obj"></param>
        /// <param name="args"></param>
        /// <returns></returns>
        /// <remarks>
        /// See <see cref="Type.InvokeMember"/> for most of the mundane details.
        /// In addition to that functionality, this method:
        /// 
        /// - Recognizes a last argument of type VarArgs. This can only match a
        ///   ParamsArray parameter.
        /// - Returns a VoidReturn object if the invoked method does not return
        ///   a value.
        /// </remarks>
        public static object InvokeWithLispSemantics(Type type,
                                                     string name,
                                                     BindingFlags bflags,
                                                     Binder binder,
                                                     object obj,
                                                     object[] args)
        {
            if (obj == null && type == null)
                throw new Exception("InvokeWithLispSemantics() was called with neither a type nor an object.");
                
            // We have to do a two-step invocation, because simply calling
            // InvokeMember won't tell us when we've invoked a method with a
            // void return type, and we would like to accurately reflect this in
            // the returned value.

            if ((bflags & (BindingFlags.SetProperty | BindingFlags.SetField)) != BindingFlags.Default && args.Length < 1)
                throw new ArgumentException("Need at least one argument to invoke a property or field setter.");

            // BindingFlagsToMemberTypes will check for errors in the binding flags.
            MemberTypes mtypes = BindingFlagsToMemberTypes(bflags);

            if (mtypes == MemberTypes.Constructor && (obj == null))
            {
                // Special case -- object construction. Better to handle this here.
                return Activator.CreateInstance(type, bflags, binder, args, null);
            }
            
            if (type == null)
                type = obj.GetType();
                
            MemberInfo[] members
                = type.FindMembers(mtypes,
                                   bflags & (BindingFlags.Instance
                                                | BindingFlags.Static
                                                | BindingFlags.Public
                                                | BindingFlags.NonPublic
                                                | BindingFlags.DeclaredOnly
                                                | BindingFlags.FlattenHierarchy),
                                   Type.FilterName,
                                   name);
                                             
            if (members == null || members.Length == 0)
                throw new MissingMemberException(String.Format("No {0} member of {1} named \"{2}\" found.",
                                                               obj == null ? "static" : "instance",
                                                               type.FullName,
                                                               name));

            // Make sure the results are all of the same kind. This should always
            // be the case if effective_type is CLS-compliant.
            MemberTypes kind = members[0].MemberType;
            foreach (MemberInfo member in members)
            {
                if (kind != member.MemberType)
                    throw new Exception(String.Format("Multiple members named {0} of different kinds found in type {1}. Cannot invoke this member.",
                                                      name, type.FullName));
            }
            
            object result = null;
            if (kind == MemberTypes.Field)
            {
                FieldInfo fld_info = (FieldInfo)(members[0]);
                if (members.Length > 1)
                {
                    for (int i = 1; i < members.Length; ++i)
                    {
                        if (members[i].DeclaringType.IsSubclassOf(fld_info.DeclaringType))
                            fld_info = (FieldInfo)(members[i]);
                    }
                }
                if ((bflags & BindingFlags.GetField) != BindingFlags.Default)
                    result = fld_info.GetValue(obj);
                else
                    fld_info.SetValue(obj, args[0]);
            }
            else if (kind == MemberTypes.Property)
            {
                if (members.Length != 1)
                    throw new AmbiguousMatchException(String.Format("Multiple like-named properties {0}.{1} found.",
                                                                    type.FullName, name));
                // TODO: This isn't right. Properties can have multiple overloaded accessors.
                MethodBase method;
                if ((bflags & BindingFlags.GetField) != BindingFlags.Default)
                    method = ((PropertyInfo)members[0]).GetGetMethod();
                else
                    method = ((PropertyInfo)members[0]).GetSetMethod();
                object state;
                method = binder.BindToMethod(bflags, new MethodBase[] {method}, ref args, null, null, null, out state);
                result = method.Invoke(obj, args);
                binder.ReorderArgumentArray(ref args, state);
            }
            else if (kind == MemberTypes.Constructor)
            {
                MethodBase[] methods
                = Array.ConvertAll<MemberInfo, MethodBase>(members,
                                                           new Converter<MemberInfo, MethodBase>(delegate(MemberInfo m)
                                                           {
                                                                return (MethodBase)m;
                                                           }));
                object state;
                ConstructorInfo method = (ConstructorInfo)binder.BindToMethod(bflags, methods, ref args, null, null, null, out state);
                if (method != null)
                {
                    result = method.Invoke(args);
                    binder.ReorderArgumentArray(ref args, state);
                }
                else if (args.Length == 0 && type.IsValueType)
                {
                    // Weird special case: If a struct type has a defaulted constructor, it doesn't actually
                    // appear as a member. Hope I got that right.
                    result = Activator.CreateInstance(type);
                }
                else
                {
                    throw new MissingMemberException(String.Format("There is no {0} member of {1} named \"{2}\" that can take these arguments:\n  {3}",
                                                                   obj == null ? "static" : "instance",
                                                                   type.FullName,
                                                                   name,
                                                                   ArgsToStrings(args)));
                }
            }
            else
            {
                MethodBase[] methods
                = Array.ConvertAll<MemberInfo, MethodBase>(members,
                                                           new Converter<MemberInfo, MethodBase>(delegate(MemberInfo m)
                                                           {
                                                                return (MethodBase)m;
                                                           }));
                object state;
                MethodBase method = binder.BindToMethod(bflags, methods, ref args, null, null, null, out state);
                if (method == null)
                    throw new MissingMemberException(String.Format("There is no {0} member of {1} named \"{2}\" that can take these arguments:\n  {3}",
                                                                    obj == null ? "static" : "instance",
                                                                    type.FullName,
                                                                    name,
                                                                    ArgsToStrings(args)));
                result = method.Invoke(obj, args);
                binder.ReorderArgumentArray(ref args, state);
                if (result == null)
                {
                    MethodInfo method_info = method as MethodInfo;
                    if (method_info != null && method_info.ReturnType == typeof(void))
                        result = VoidReturn.Instance;
                }
            }
            return result;
        }

        #endregion
        #region Private Members

        TypeConversionDictionary numeric_conversions;
        bool implicit_double_narrowing;
        bool implicit_enum_conversion;
        
        private static object ArgsToStrings(object[] args)
        {
            StringBuilder str = new StringBuilder();
            foreach (object arg in args)
            {
                if (str.Length != 0)
                    str.Append("\n  ");
                str.Append(arg.GetType().FullName);
                str.Append(": ");
                str.Append(arg.ToString());
            }
            return str.ToString();
        }
        static Type ElementTypeFromVarArgsType(Type type)
        {
            if (!type.IsSubclassOf(typeof(VarArgs)))
                return null;
            return type.GetGenericArguments()[0];
        }

        class SuppressParamsKeyword
        {
            public readonly static SuppressParamsKeyword Instance = new SuppressParamsKeyword();
        };

        class TypeComparer : IComparer<Type>
        {
            public virtual int Compare(Type a, Type b)
            {
                return a.GUID.CompareTo(b.GUID);
            }
        };

        public void AddNumericConversions(Type from, params Type[] to)
        {
            TypeList to_list = new TypeList();
            foreach (Type t in to)
                to_list.Add(t);
            numeric_conversions.Add(from, to_list);
        }

        void init()
        {
            // implicit numeric conversions, from the C# standard:
            // 
            // From sbyte to short, int, long, float, double, or decimal.
            // From byte to short, ushort, int, uint, long, ulong, float, double, or decimal.
            // From short to int, long, float, double, or decimal.
            // From ushort to int, uint, long, ulong, float, double, or decimal.
            // From int to long, float, double, or decimal.
            // From uint to long, ulong, float, double, or decimal.
            // From long to float, double, or decimal.
            // From ulong to float, double, or decimal.
            // From char to ushort, int, uint, long, ulong, float, double, or decimal.
            // From float to double.
            //
            // The CLS-compliant subset (plus decimals):
            // From byte to short, int, long, float, double, or decimal.
            // From short to int, long, float, double, or decimal.
            // From int to long, float, double, or decimal.
            // From long to float, double, or decimal.
            // From char to int, long, float, double, or decimal.
            // From float to double.
            numeric_conversions = new TypeConversionDictionary(new TypeComparer());
            AddNumericConversions(typeof(SByte), typeof(Int16),
                                                  typeof(Int32),
                                                  typeof(Int64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(Byte), typeof(Int16),
                                                  typeof(UInt16),
                                                  typeof(Int32),
                                                  typeof(UInt32),
                                                  typeof(Int64),
                                                  typeof(UInt64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(Int16), typeof(Int32),
                                                  typeof(Int64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(UInt16), typeof(Int32),
                                                  typeof(UInt32),
                                                  typeof(Int64),
                                                  typeof(UInt64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(Int32), typeof(Int64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(UInt32), typeof(Int64),
                                                  typeof(UInt64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(Int64), typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(UInt64), typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
            AddNumericConversions(typeof(Char), typeof(UInt16),
                                                  typeof(Int32),
                                                  typeof(UInt32),
                                                  typeof(Int64),
                                                  typeof(UInt64),
                                                  typeof(Single),
                                                  typeof(Double),
                                                  typeof(Decimal));
        }
        static MemberTypes BindingFlagsToMemberTypes(BindingFlags bflags)
        {
            MemberTypes mtypes = 0;
            if ((bflags & BindingFlags.CreateInstance) != BindingFlags.Default)
            {
                if ((bflags & (BindingFlags.SetField | BindingFlags.SetProperty | BindingFlags.InvokeMethod
                               | BindingFlags.GetField | BindingFlags.GetProperty)) != BindingFlags.Default)
                    throw new ArgumentOutOfRangeException("Conflicting binding flags supplied to invoke_member().");
                mtypes = mtypes | MemberTypes.Constructor;
            }
            if ((bflags & BindingFlags.InvokeMethod) != BindingFlags.Default)
            {
                if ((bflags & (BindingFlags.SetField | BindingFlags.SetProperty)) != BindingFlags.Default)
                    throw new ArgumentOutOfRangeException("Conflicting binding flags supplied to invoke_member().");
                mtypes = mtypes | MemberTypes.Method;
            }
            if ((bflags & BindingFlags.GetProperty) != BindingFlags.Default)
            {
                if ((bflags & BindingFlags.SetProperty) != BindingFlags.Default)
                    throw new ArgumentOutOfRangeException("Conflicting binding flags supplied to invoke_member().");
                mtypes = mtypes | MemberTypes.Property;
            }                
            if ((bflags & BindingFlags.SetProperty) != BindingFlags.Default)
            {
                if ((bflags & BindingFlags.GetProperty) != BindingFlags.Default)
                    throw new ArgumentOutOfRangeException("Conflicting binding flags supplied to invoke_member().");
                mtypes = mtypes | MemberTypes.Property;
            }   
            if ((bflags & BindingFlags.GetField) != BindingFlags.Default)
            {
                if ((bflags & BindingFlags.SetField) != BindingFlags.Default)
                    throw new ArgumentOutOfRangeException("Conflicting binding flags supplied to invoke_member().");
                mtypes = mtypes | MemberTypes.Field;
            }
            if ((bflags & BindingFlags.SetField) != BindingFlags.Default)
            {
                if ((bflags & BindingFlags.GetField) != BindingFlags.Default)
                    throw new ArgumentOutOfRangeException("Conflicting binding flags supplied to invoke_member().");
                mtypes = mtypes | MemberTypes.Field;
            }
            return mtypes;
        }
        object ConvertTo(object obj, Type to)
        {
            if (obj == null)
            {
                if (to.IsAssignableFrom(typeof(bool)))
                    return (Boolean)(false);

                if (to.IsValueType)
                    throw new InvalidCastException("From null ptr to value type.");
            }

            Type from = obj.GetType();
            if (from == to)
                return obj;

            if (implicit_double_narrowing && to == typeof(Single) && from == typeof(Double))
                return Convert.ToSingle(obj);
                
            if (implicit_enum_conversion && to.IsEnum && IsConvertibleTo(from, Enum.GetUnderlyingType(to)))
                return Enum.ToObject(to, obj);

            // All other conversions should be handled automatically by Invoke.
            return obj;
        }
        bool IsConvertibleTo(Type from, Type to)
        {
            if (from == to)
                return true;

            if (from == typeof(Null))
            {
                if (to == typeof(Null))
                    return true;

                if (to.IsAssignableFrom(typeof(bool)))
                    return true;

                return !(to.IsValueType);
            }
            if (to == typeof(Null))
                return false;

            // Assignability appears to satisfy C# reference conversion rules.
            if (to.IsAssignableFrom(from))
                return true;

            if (from.IsArray && to.IsArray
                && to.GetElementType().IsAssignableFrom(from.GetElementType())
                && from.GetArrayRank() == to.GetArrayRank())
                return true;
                
            if (implicit_enum_conversion && to.IsEnum && IsConvertibleTo(from, Enum.GetUnderlyingType(to)))
                return true;
                
            TypeList to_list;
            bool found = numeric_conversions.TryGetValue(from, out to_list);
            if (found && to_list.Contains(to))
                return true;

            return false;
        }
        // Returns:
        // 0 if neither conversion is better,
        // 1 if conversion to T1 is better,
        // 2 if conversion to T2 is better
        int BetterConversion(Type s, Type t1, Type t2)
        {
            if (s == typeof(Null))
                return 0;

            // The C# rules for determining a better conversion are paraphrased
            // here from the standard. Given an argument S, a parameter of type
            // T1 and an implicit conversion C1 from S to T1, and another
            // parameter of type T2 and an implicit conversion C2 from S to T2,
            // the better conversion is determined as follows:
            //
            // - If T1 and T2 are the same, neither conversion is better.
            // - If S is T1, C1 is the better conversion.
            // - If S is T2, C2 is the better conversion.
            // - If an implicit conversion from T1 to T2 exists, and no implicit
            //   conversion from T2 to T1 exists, then C1 is the better
            //   conversion.
            // - If an implicit conversion from T2 to T1 exists, and no implicit
            //   conversion from T1 to T2 exists, then C2 is the better
            //   conversion.

            if (t1 == t2)
                return 0;
            if (s == t1)
                return 1;
            if (s == t2)
                return 2;
            if (IsConvertibleTo(t1, t2))
            {
                if (IsConvertibleTo(t2, t1))
                {
                    // Don't let implicit narrowing of double to single
                    // introduce ambiguity.
                    if (implicit_double_narrowing)
                    {
                        if (t1 == typeof(Single))
                        {
                            if (t2 == typeof(Double))
                                return 1;
                        }
                        else if (t2 == typeof(Single))
                        {
                            if (t2 == typeof(Double))
                                return 2;
                        }
                    }
                    return 0;
                }
                return 1;
            }
            if (IsConvertibleTo(t2, t1))
                return 2;
            return 0;
        }
        delegate Type TypeAccessor(object obj);
        int BetterArgsMatch(int n_args,
                            object[] args,
                            TypeAccessor type_accessor,
                            ParameterInfo[] params1,
                            ParameterInfo[] params2)
        {
            if (n_args == 0)
                return 0;
            for (int i = 0; i < n_args-1; ++i)
            {
                int match = BetterConversion(type_accessor(args[i]),
                                             params1[i].ParameterType,
                                             params2[i].ParameterType);
                if (match != 0)
                    return match;
            }
            // Treat the last argument special in case it's a VarArgs type.
            int last = n_args-1;
            Type last_arg_type = type_accessor(args[last]);
            if (last_arg_type.IsSubclassOf(typeof(VarArgs)))
                last_arg_type = VarArgs.TypeToArrayType(last_arg_type);
            return BetterConversion(last_arg_type,
                                    params1[last].ParameterType,
                                    params2[last].ParameterType);
        }
        // Returns the element type of the last argument in parameters,
        // if it's a param array denoting the ability to receive a
        // variable length argument list. Returns null if the
        // parameter list is empty or does not end in a param array.
        Type VaryingParamsType(ParameterInfo[] parameters)
        {
            if (parameters.Length == 0)
                return null;
            ParameterInfo last = parameters[parameters.Length - 1];
            if (last.GetCustomAttributes(typeof(ParamArrayAttribute), false) == null)
                return null;
            return last.ParameterType.GetElementType();
        }

        Type TypeFromObject(object obj)
        {
            if (obj == null)
                return typeof(Null);
            return obj.GetType();
        }
        Type TypeFromType(object obj)
        {
            if (obj == null)
                return typeof(Null);
            return (Type)(obj);
        }
        ParameterInfo[] ParamsFromPropertyInfo(object info)
        {
            return ((PropertyInfo)info).GetIndexParameters();
        }
        ParameterInfo[] ParamsFromMethodBase(object info)
        {
            return ((MethodBase)info).GetParameters();
        }
        bool ArgsConformToParams(ParameterInfo[] parameters,
                                             object[] args,
                                             TypeAccessor type_accessor)
        {
            int n_args = args.Length;
            int n_params = parameters.Length;
            int n_required = n_params;
            Type varying_type = VaryingParamsType(parameters);
            if (varying_type != null)
                --n_required;
            if (n_required > n_args)
                return false;
            if ((n_args > n_required) && varying_type == null)
                return false;
            for (int i = 0; i < n_required; ++i)
            {
                if (!IsConvertibleTo(type_accessor(args[i]), parameters[i].ParameterType))
                    return false;
            }
            if (n_args == n_required)
                return true;

            // If we get here, there are optional arguments, and
            // the parameter list allows varying args.

            if ((n_args - n_required) == 1)
            {
                Type var_args_type = ElementTypeFromVarArgsType(type_accessor(args[n_required]));
                if (var_args_type != null)
                    return varying_type.IsAssignableFrom(var_args_type);
            }

            // Check varying args for convertability to param array element type.
            bool matches = true;
            for (int i = n_required; matches && i < n_args; ++i)
                matches = IsConvertibleTo(type_accessor(args[i]), varying_type);
            if (!matches)
                return false;
            return true;
        }

        delegate ParameterInfo[] ParamsAccessor(object obj);

        object SelectMethods(MemberInfo[] match,
                             bool check_conformance,
                             ParamsAccessor params_accessor,
                             object[] args,
                             TypeAccessor type_accessor)
        {
            if (match == null)
                throw new ArgumentNullException();
            if (match.Length == 0)
                return null;
            int n_args = args.Length;

            // If the last argument is a VarArgs array, this will be
            // the type of its elements.
            Type var_args_type = null;
            if (n_args > 0)
                var_args_type = ElementTypeFromVarArgsType(type_accessor(args[n_args - 1]));

            MethodBase best = null;
            int best_n_required = 0;
            List<MethodBase> ties = null;
            foreach (MethodBase candidate in match)
            {
                ParameterInfo[] parameters = candidate.GetParameters();
                if (check_conformance && !ArgsConformToParams(parameters, args, type_accessor))
                    continue;
                if (best == null)
                {
                    best = candidate;
                    best_n_required = parameters.Length;
                    if (var_args_type == null && VaryingParamsType(parameters) != null)
                        --best_n_required;
                    continue;
                }
                int n_required = parameters.Length;
                Type varying_type = null;
                // If the argument list ends in a VarArgs object, then we
                // treat ParamArray parameters like ordinary parameters. Our
                // type-matching logic treats a VarArg object like an array.
                if (var_args_type == null)
                    varying_type = VaryingParamsType(parameters);
                if (varying_type != null)
                    --n_required;
                // 0 = tie
                // 1 = candidate is worse than best
                // 2 = candidate is better than best
                int matches = BetterArgsMatch(Math.Min(n_required, best_n_required),
                                              args,
                                              type_accessor,
                                              best.GetParameters(),
                                              parameters);
                // If it's a tie so far, but one takes a varying argument list
                // and the other doesn't, then the one without varying args
                // wins. However, we don't do this test if the argument list
                // ends in a VarArgs object.
                if (matches == 0 && var_args_type == null)
                {
                    Type best_varying_type = VaryingParamsType(best.GetParameters());
                    if (best_varying_type == null)
                    {
                        if (varying_type != null)
                            matches = 1;
                    }
                    else if (varying_type == null)
                    {
                        matches = 2;
                    }
                    else
                    {
                        // Hmm. Tie-breaker is based on the 'expanded forms' of
                        // both parameter lists, i.e. we pretend they have as
                        // many arguments as necessary of the varying-argument
                        // type.
                        for (int i = n_required; i < n_args; ++i)
                        {
                            matches = BetterConversion(type_accessor(args[i]), best_varying_type, varying_type);
                            if (matches != 0)
                                break;
                        }
                    }
                }
                // Sometimes we get two members from FindMembers that are
                // identical except for details that we don't understand. It
                // seems like one should shadow the other. A good example is
                // requesting GetType for a System.AppDomain object. We'll try
                // to differentiate based on the relative convertability of the
                // declaring types. This is basically the same logic as in
                // BetterConversion, except that we already know that the called
                // object (a real object or a type meta-object) is convertible
                // to both declaring types.
                if (matches == 0)
                {
                    if (IsConvertibleTo(best.DeclaringType, candidate.DeclaringType))
                    {
                        if (!IsConvertibleTo(candidate.DeclaringType, best.DeclaringType))
                            matches = 1;
                    }
                    else if (IsConvertibleTo(candidate.DeclaringType, best.DeclaringType))
                        matches = 2;
                }

                switch (matches)
                {
                    case 1: break;
                    case 0:
                        if (ties == null)
                        {
                            ties = new List<MethodBase>();
                            ties.Add(best);
                        }
                        ties.Add(candidate);
                        break;
                    case 2:
                        best = candidate;
                        best_n_required = n_required;
                        if (ties != null)
                            ties.Clear();
                        break;
                }
            }
            if (ties != null && ties.Count > 0)
                return ties;
            return best;
        }

        object[] BindArgs(ParameterInfo[] parameters,
                          object[] args)
        {
            int n_args = args.Length;
            object[] new_args
                = (n_args == parameters.Length) ? args : new object[parameters.Length];
            int n_required = parameters.Length;
            Type varying_args_type = VaryingParamsType(parameters);
            if (varying_args_type != null)
                --n_required;
            Debug.Assert(n_args >= n_required);
            for (int i = 0; i < n_required; ++i)
                new_args[i] = ConvertTo(args[i], parameters[i].ParameterType);
            if (varying_args_type != null)
            {
                if (n_required == n_args)
                {
                    // TODO: Is an empty array really necessary?
                    new_args[n_required] = System.Array.CreateInstance(varying_args_type, 0);
                }
                else
                {
                    VarArgs wrapped_varargs = args[n_required] as VarArgs;
                    if (wrapped_varargs != null)
                    {
                        new_args[n_required] = wrapped_varargs.Args;
                    }
                    else
                    {
                        // TODO: Possible optimization: If only one optional
                        // argument is present, we wouldn't need to allocate a
                        // new args array. However, we'd have to give some other
                        // indication to the caller that the last arg has to be
                        // 'unwrapped' after an invocation.
                        Array varargs = Array.CreateInstance(varying_args_type, n_args - n_required);
                        new_args[n_required] = varargs;
                        for (int i = 0; i < n_args - n_required; ++i)
                            varargs.SetValue(ConvertTo(args[i + n_required], varying_args_type), i);
                    }
                }
            }
            return new_args;
        }
        #endregion
    }
}
