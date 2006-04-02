// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

#include "stdafx.h"
#include <assert.h>
#include "LispBinder.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Globalization;
using namespace System::Reflection;
using namespace System::Runtime::InteropServices;
using namespace SpookyDistance::CommonLispReflection;

ref class SuppressParamsKeyword
{
public:
    static SuppressParamsKeyword^ Instance = gcnew SuppressParamsKeyword();
};

ref class NilTypeClass
{
};

ref class TypeComparer : IComparer<Type^>
{
public:
    virtual int Compare(Type^ a, Type^ b)
    {
        return String::Compare(a->Name, b->Name);
    }
};
void LispBinder::AddNumericConversions(Type^ from, ... array<Type^>^ to)
{
    TypeList^ to_list = gcnew TypeList;
    for each (Type^ t in to)
        to_list->Add(t);
    numeric_conversions->Add(from, to_list);
}
void LispBinder::init()
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
    numeric_conversions = gcnew TypeConversionDictionary(gcnew TypeComparer);
    AddNumericConversions(SByte::typeid,  Int16::typeid,
                                          Int32::typeid,
                                          Int64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(Byte::typeid,   Int16::typeid,
                                          UInt16::typeid,
                                          Int32::typeid,
                                          UInt32::typeid,
                                          Int64::typeid,
                                          UInt64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(Int16::typeid,  Int32::typeid,
                                          Int64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(UInt16::typeid, Int32::typeid,
                                          UInt32::typeid,
                                          Int64::typeid,
                                          UInt64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(Int32::typeid,  Int64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(UInt32::typeid, Int64::typeid,
                                          UInt64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(Int64::typeid,  Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(UInt64::typeid, Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
    AddNumericConversions(Char::typeid,   UInt16::typeid,
                                          Int32::typeid,
                                          UInt32::typeid,
                                          Int64::typeid,
                                          UInt64::typeid,
                                          Single::typeid,
                                          Double::typeid,
                                          Decimal::typeid);
}
LispBinder::LispBinder()
    : implicit_double_narrowing(false)
{
    init();
}
LispBinder::LispBinder(bool implicit_double_down)
    : implicit_double_narrowing(implicit_double_down)
{
    init();
    if (implicit_double_narrowing)
        AddNumericConversions(Double::typeid, Single::typeid);
}
Object^ LispBinder::ConvertTo(Object^ obj, Type^ to)
{
    if (to == NilTypeClass::typeid)
        throw gcnew InvalidCastException("Internal error: conversion to NilType attempted.");

    if (obj == nullptr)
    {
        if (to == NilTypeClass::typeid)
            return obj;

        if (to->IsAssignableFrom(bool::typeid))
            return safe_cast<Boolean^>(false);

        if (to->IsValueType)
            throw gcnew InvalidCastException("From null ptr to value type.");
    }

    Type^ from = obj->GetType();
    if (from == to)
        return obj;

    if (implicit_double_narrowing && to == Single::typeid && from == Double::typeid)
        return Convert::ToSingle(obj);

    // All other conversions should be handled automatically by Invoke.
    return obj;
}
bool LispBinder::IsConvertibleTo(Type^ from, Type^ to)
{
    if (from == to)
        return true;

    if (from == NilTypeClass::typeid)
    {
        if (to == NilTypeClass::typeid)
            return true;

        if (to->IsAssignableFrom(bool::typeid))
            return true;

        return !(to->IsValueType);
    }
    if (to == NilTypeClass::typeid)
        return false;

    // Assignability appears to satisfy C# reference conversion rules.
    if (to->IsAssignableFrom(from))
        return true;

    TypeList^ to_list;
    bool found  = numeric_conversions->TryGetValue(from, to_list);
    if (found && to_list->Contains(to))
        return true;

    return false;
}
// Returns:
// 0 if neither conversion is better,
// 1 if conversion to T1 is better,
// 2 if conversion to T2 is better
int LispBinder::BetterConversion(Type^ s, Type^ t1, Type^ t2)
{
    if (s == NilTypeClass::typeid)
        return 0;

    //The C# rules for determining a better conversion are paraphrased
    //here from the standard. Given an argument S, a parameter
    //of type T1 and an implicit conversion C1 from S to T1, and another
    //parameter of type T2 and an implicit conversion C2 from S to T2, the
    //better conversion is determined as follows:
    //
    //- If T1 and T2 are the same, neither conversion is better.
    //- If S is T1, C1 is the better conversion.
    //- If S is T2, C2 is the better conversion.
    //- If an implicit conversion from T1 to T2 exists, and no implicit
    //  conversion from T2 to T1 exists, then C1 is the better conversion.
    //- If an implicit conversion from T2 to T1 exists, and no implicit
    //  conversion from T1 to T2 exists, then C2 is the better conversion.

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
                if (t1 == Single::typeid)
                {
                    if (t2 == Double::typeid)
                        return 1;
                }
                else if (t2 == Single::typeid)
                {
                    if (t2 == Double::typeid)
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

int LispBinder::BetterArgsMatch(int n_args,
                                array<Object^>^ args,
                                Type^ (type_accessor)(Object^),
                                array<ParameterInfo^>^ params1,
                                array<ParameterInfo^>^ params2)
{
    for (int i = 0; i < n_args; ++i)
    {
        int match = BetterConversion(type_accessor(args[i]),
                                     params1[i]->ParameterType,
                                     params2[i]->ParameterType);
        if (match != 0)
            return match;
    }
    return 0;
}
// Returns the element type of the last argument in params,
// if it's a param array denoting the ability to receive a
// variable length argument list. Returns nullptr if the
// parameter list is empty or does not end in a param array.
Type^ LispBinder::VaryingParamsType(array<ParameterInfo^>^ params)
{
    if (!params->Length)
        return nullptr;
    ParameterInfo^ last = params[params->Length-1];
    if (!last->GetCustomAttributes(ParamArrayAttribute::typeid, false))
        return nullptr;
    return last->ParameterType->GetElementType();
}

Type^ LispBinder::TypeFromObject(Object^ obj)
{
    if (!obj)
        return NilTypeClass::typeid;
    return obj->GetType();
}
Type^ LispBinder::TypeFromType(Object^ obj)
{
    if (!obj)
        return NilTypeClass::typeid;
    return safe_cast<Type^>(obj);
}
array<ParameterInfo^>^ LispBinder::ParamsFromPropertyInfo(Object^ info)
{
    return safe_cast<PropertyInfo^>(info)->GetIndexParameters();
}
array<ParameterInfo^>^ LispBinder::ParamsFromMethodBase(Object^ info)
{
    return safe_cast<MethodBase^>(info)->GetParameters();
}
bool LispBinder::ArgsConformToParams(array<ParameterInfo^>^ params,
                                     array<Object^>^ args,
                                     Type^ (type_accessor)(Object^ obj))
{
    int n_args = args->Length;
    int n_params = params->Length;
    int n_required = n_params;
    Type^ varying_type = VaryingParamsType(params);
    if (varying_type)
        --n_required;
    if (n_required > n_args)
        return false;
    if ((n_args > n_required) && !varying_type)
        return false;
    for (int i = 0; i < n_required; ++i)
    {
        if (!IsConvertibleTo(type_accessor(args[i]), params[i]->ParameterType))
            return false;
    }
    if (n_args == n_required)
        return true;

    // If we get here, there are optional arguments, and
    // the parameter list allows varying args.

    if ((n_args-n_required) == 1)
    {
        Type^ var_args_type = VarArgsBase::ElementTypeFromVarArgsType(type_accessor(args[n_required]));
        if (var_args_type)
            return varying_type->IsAssignableFrom(var_args_type);
    }

    // Check varying args for convertability to param array element type.
    bool matches = true;
    for (int i = n_required; matches && i < n_args; ++i)
        matches = IsConvertibleTo(type_accessor(args[i]), varying_type);
    if (!matches)
        return false;
    return true;
}
Object^ LispBinder::SelectMethods (array<MemberInfo^>^ match,
                                   bool check_conformance,
                                   array<ParameterInfo^>^ (params_accessor)(Object^ info),
                                   array<Object^>^ args,
                                   Type^ (type_accessor)(Object^ obj))
{
    if ( match == nullptr)
        throw gcnew ArgumentNullException;
    if ( match->Length == 0)
        return nullptr;
    int n_args = args->Length;

    // If the last argument is a VarArgs array, this will be
    // the type of its elements.
    Type^ var_args_type = nullptr;
    if (n_args > 0)
        Type^ var_args_type = VarArgsBase::ElementTypeFromVarArgsType(type_accessor(args[n_args-1]));

    MethodBase^ best;
    List<MethodBase^>^ ties = nullptr;
    for each (MethodBase^ candidate in match)
    {
        array<ParameterInfo^>^ params = candidate->GetParameters();
        if (check_conformance && !ArgsConformToParams(params, args, type_accessor))
            continue;
        if (!best)
        {
            best = candidate;
            continue;
        }
        int n_required = params->Length;
        Type^ varying_type = nullptr;
        if (!var_args_type)
            varying_type = VaryingParamsType(params);
        if (varying_type)
            --n_required;
        // 0 = tie
        // 1 = candidate is worse than best
        // 2 = candidate is better than best
        int match = BetterArgsMatch(n_required,
                                    args,
                                    type_accessor,
                                    best->GetParameters(),
                                    params);
        // If it's a tie so far, but one takes a varying argument list and
        // the other doesn't, then the one without varying args wins.
        if (match == 0 && !var_args_type)
        {
            if (varying_type)
            {
                Type^ best_varying_type = VaryingParamsType(best->GetParameters());
                if (best_varying_type)
                {
                    // Hmm. Tie-breaker is based on the 'expanded forms' of both
                    // parameter lists, i.e. we pretend they have as many arguments
                    // as necessary of the varying-argument type.
                    for (int i = n_required; i < n_args; ++i)
                    {
                        match = BetterConversion(type_accessor(args[i]), best_varying_type, varying_type);
                        if (match)
                            break;
                    }
                }
            }
            else if (VaryingParamsType(best->GetParameters()))
            {
                match = 2;
            }
        }
        // Sometimes we get two members from FindMembers that are identical
        // except for details that we don't understand. It seems like one
        // should shadow the other. A good example is requesting GetType for
        // a System.AppDomain object. We'll try to differentiate based on the
        // relative convertability of the declaring types. This is basically
        // the same logic as in BetterConversion, except that we already know
        // that the called object (a real object or a type meta-object) is
        // a reference type and is convertible to both declaring types.
        if (match == 0)
        {
            if (IsConvertibleTo(best->DeclaringType, candidate->DeclaringType))
            {
                if (!IsConvertibleTo(candidate->DeclaringType, best->DeclaringType))
                    match = 1;
            }
            else if (IsConvertibleTo(candidate->DeclaringType, best->DeclaringType))
                match = 2;
        }
        
        switch (match)
        {
        case 1: break;
        case 0:
            if (!ties)
            {
                ties = gcnew List<MethodBase^>;
                ties->Add(best);
            }
            ties->Add(candidate);
            break;
        case 2:
            best = candidate;
            if (ties)
                ties->Clear();
            break;
        }
    }
    if (ties && ties->Count > 0)
        return ties;
    return best;
}

MethodBase^ LispBinder::SelectMethod (BindingFlags bindingAttr, 
                                  array<MethodBase^>^ match, 
                                  array<Type^>^ types, 
                                  array<ParameterModifier>^ modifiers)
{
    if (match == nullptr || types == nullptr)
        throw gcnew ArgumentNullException();
    Object^ result = SelectMethods(match, true, ParamsFromMethodBase,
                                   types,
                                   TypeFromType);
    if (MethodInfo^ method = dynamic_cast<MethodInfo^>(result))
        return method;
    if (array<MemberInfo^>^ methods = dynamic_cast<array<MemberInfo^>^>(result))
        throw gcnew AmbiguousMatchException("Multiple matches.");
    return nullptr;
}
FieldInfo^ LispBinder::BindToField (BindingFlags bindingAttr, 
                                array<FieldInfo^>^ match,
                                Object^ value,
                                CultureInfo^ culture)
{
    return Type::DefaultBinder->BindToField(bindingAttr, match, value, culture);
}
PropertyInfo^ LispBinder::SelectProperty (BindingFlags bindingAttr, 
                                      array<PropertyInfo^>^ match,
                                      Type^ returnType,
                                      array<Type^>^ indexes,
                                      array<ParameterModifier>^ modifiers)
{
    if (match == nullptr || indexes == nullptr)
        throw gcnew ArgumentNullException();
    Object^ result = SelectMethods(match, true, ParamsFromPropertyInfo, 
                                   indexes, TypeFromType);
    if (PropertyInfo^ method = dynamic_cast<PropertyInfo^>(result))
        return method;
    if (array<MemberInfo^>^ methods = dynamic_cast<array<MemberInfo^>^>(result))
        throw gcnew AmbiguousMatchException("Multiple matches.");
    return nullptr;
}
int LispBinder::ReturnArraySize(MethodInfo^ method)
{
    // The return value list has to be large enough to hold the
    // return count, and a series of index/value pairs for the
    // returned value and the output of each by-ref parameter.
    int return_size = 1;
    if (method->ReturnType != System::Void::typeid)
        return_size += 2;
    for each (ParameterInfo^ param in method->GetParameters())
    {
        if (param->ParameterType->IsByRef)
            return_size += 2;
    }
    return return_size;
}
array<Object^>^ LispBinder::BindArgs(array<ParameterInfo^>^ params,
                                     array<Object^>^ args)
{
    int n_args = args->Length;
    array<Object^>^ new_args
        = (n_args == params->Length) ? args : gcnew array<Object^>(params->Length);
    int n_required = params->Length;
    Type^ varying_args_type = VaryingParamsType(params);
    if (varying_args_type)
        --n_required;
    assert(n_args >= n_required);
    for (int i = 0; i < n_required; ++i)
        new_args[i] = ConvertTo(args[i], params[i]->ParameterType);
    if (varying_args_type)
    {
        if (n_required == n_args)
        {
            // TODO: Is an empty array really necessary?
            new_args[n_required] = System::Array::CreateInstance(varying_args_type, 0);
        }
        else
        {
            VarArgsBase^ wrapped_varargs = dynamic_cast<VarArgsBase^>(args[n_required]);
            if (wrapped_varargs)
            {
                new_args[n_required] = wrapped_varargs->Args;
            }
            else
            {
                // TODO: Possible optimization: If only one optional argument
                // is present, we wouldn't need to allocate a new args array.
                // However, we'd have to give some other indication to the
                // caller that the last arg has to be 'unwrapped' after an
                // invocation.
                Array^ varargs = Array::CreateInstance(varying_args_type, n_args - n_required);
                new_args[n_required] = varargs;
                for (int i = 0; i < n_args-n_required; ++i)
                    varargs->SetValue(ConvertTo(args[i+n_required], varying_args_type), i);
            }
        }
    }
    return new_args;
}
MethodBase^ LispBinder::BindToMethod (BindingFlags bindingAttr,
                                      array<MethodBase^>^match,
                                      array<Object^>^%args,
                                      array<ParameterModifier>^ modifiers,
                                      CultureInfo^ culture,
                                      array<String^>^names,
                                      [Out]Object^% state )
{
    state = nullptr;
    if ( match == nullptr || args == nullptr)
        throw gcnew ArgumentNullException;
    Object^ result = SelectMethods(match, true, ParamsFromMethodBase,
                                   args, TypeFromObject);
    if (array<MemberInfo^>^ methods = dynamic_cast<array<MemberInfo^>^>(result))
        throw gcnew AmbiguousMatchException("Multiple matches.");
    MethodInfo^ method = dynamic_cast<MethodInfo^>(result);
    if (!method)
        return nullptr;
    array<Object^>^ new_args = BindArgs(method->GetParameters(), args);
    if (new_args != args)
        state = args;
    args = new_args;
    return method;
}
void LispBinder::ReorderArgumentArray (array<Object^>^% args,
                                       Object^ state)
{
    if (array<Object^>^ old_args = dynamic_cast<array<Object^>^>(state))
    {
        if (old_args != args)
        {
            // Copy values back to original array, and give that back
            // to the caller. This should only occur when a variable
            // argument list was involved.
            // TODO: We held onto the caller's original arg array from
            // BindToMethod, and now we're modifying it. Is that really
            // kosher? Not sure.

            // Copy required args first.
            Array::Copy(args, old_args, args->Length - 1);

            // Now copy the variable args.
            if (old_args->Length > args->Length)
            {
                Array^ varargs = safe_cast<Array^>(args[args->Length-1]);
                Array::Copy(varargs, 0, old_args, args->Length, varargs->Length);
            }
            args = old_args;
        }
    }
}

Object^ LispBinder::ChangeType (Object^ value,
                            Type^ type,
                            CultureInfo^ culture)
{
    return ConvertTo(value, type);
}

