// This is the main DLL file.

#include "stdafx.h"
#include "DllInterface.h"
#include "LispBinder.h"
#include <assert.h>
#include <cstring>

using namespace System;
using namespace System::Reflection;
using namespace System::Runtime::InteropServices;
using namespace SpookyDistance::CommonLispReflection;

ref class TreatAs
{
public:
    Type^   effective_type;
    Object^ value;
};

// This is used internally to convert an object into a handle that we can
// safely give to the Lisp system.
static inline clr_handle GetHandleFromObject(Object^ object)
{
    if (object == nullptr)
        return 0;
    return GCHandle::ToIntPtr(GCHandle::Alloc(object)).ToPointer();
}

// This is used to get the object referred to by a handle.
static inline Object^ GetObjectFromHandle(clr_handle handle)
{
    if (!handle)
        return nullptr;
    return ((GCHandle)IntPtr(handle)).Target;
}

 // The Lisp system calls this to free an object for garbage collection.
 void ReleaseObjectHandle(clr_handle handle)
{
    if (handle)
        ((GCHandle)IntPtr(handle)).Free();
}

//////////////////////////////////////////////////////////////////////////////
// Exported entry points


// Everything starts from an application domain object:
clr_handle GetRootAppDomain()
{
    return GetHandleFromObject(AppDomain::CurrentDomain);
}

ref class InvocableMemberPredicate {
    LispBinder^ binder;
    String^ name;
    int n_args;
    array<Object^>^ args;
    bool allow_varying_args;
public:
    InvocableMemberPredicate(LispBinder^ b,
                             String^ nm,
                             bool allow_varargs,
                             int nargs,
                             array<Object^>^ a)
        : binder(b),
          name(nm), allow_varying_args(allow_varargs),
          n_args(nargs),
          args(a)
    {
    }
    bool IsInvocable(MemberInfo^ member, Object^)
    {
        if (member->Name == name)
            return true;
        if (MethodBase^ method = dynamic_cast<MethodBase^>(member))
        {
            if (method->IsSpecialName
                && method->Name->EndsWith(name)
                && method->Name->StartsWith("get_")
                && method->Name->Length == (name->Length + 4))
            {
                return binder->ArgsConformToParams(method->GetParameters(),
                                                   allow_varying_args,
                                                   0, n_args, args,
                                                   LispBinder::TypeFromObject
                                                   );
            }
        }
        return false;
    }
};

clr_handle MakeObjectArray(int n)
{
    return GetHandleFromObject(gcnew array<Object^>(n));
}

clr_handle GetArrayElement(clr_handle arry, int index)
{
    return GetHandleFromObject(safe_cast<Array^>(GetObjectFromHandle(arry))->GetValue(index));
}
void SetArrayElement(clr_handle arry, int index, clr_handle obj)
{
    safe_cast<Array^>(GetObjectFromHandle(arry))->SetValue(GetObjectFromHandle(obj), index);
}
int IsSimpleType(clr_handle obj, const char* type_name)
{
    return GetObjectFromHandle(obj)->GetType()->FullName == gcnew String(type_name);
}

void InvokeMember(clr_handle object_handle,
                  clr_handle type_handle,
                  const char* name,
                  int allow_varying_args,
                  int n_args,
                  clr_handle arg_return)
{
    array<Object^>^ args = safe_cast<array<Object^>^>(GetObjectFromHandle(arg_return));
    try
    {
        BindingFlags flags = BindingFlags::Public | BindingFlags::FlattenHierarchy;
        Object^ object = GetObjectFromHandle(object_handle);
        Type^ type = safe_cast<Type^>(GetObjectFromHandle(type_handle));
        if (object)
        {
            flags = flags | BindingFlags::Instance;
            if (!type)
                type = object->GetType();
        }
        else
        {
            flags = flags | BindingFlags::Static;
        }
        assert(type);

        LispBinder^ binder = gcnew LispBinder(false);
        String^ strname = gcnew String(name);
        MemberFilter^ predicate = gcnew MemberFilter(gcnew InvocableMemberPredicate(binder, strname, false, n_args, args),
                                                     &InvocableMemberPredicate::IsInvocable);
        // In this search, note that we're not interested in properties,
        // but rather in their getters. The predicate object will pick
        // those out for us.
        array<MemberInfo^>^ candidates
            = type->FindMembers(MemberTypes::Method | MemberTypes::Field,
                                flags,
                                predicate,
                                nullptr);
        if (!candidates || candidates->Length == 0)
            throw gcnew MissingMethodException(strname);
        // According to CLR rules, a name can't be overloaded by kind.
        // It's easy to create a class that violates this in C#, using
        // the new keyword. As it happens, we will handle this OK unless
        // a method or parameter taking no arguments conflicts with a
        // similar parameter, method, or field of the same name.
        MemberTypes kind = candidates[0]->MemberType;
        for each (MemberInfo^ member in candidates)
        {
            if (member->MemberType != kind)
                throw gcnew AmbiguousMatchException(String::Format("Conflicting kinds of members with name {0}.", strname));
        }

        switch (kind)
        {
        case MemberTypes::Method:
            {
                Object^ members = binder->SelectMethods(candidates,
                                                        false, // Don't check type conformance, we already did.
                                                        LispBinder::ParamsFromMethodBase,
                                                        allow_varying_args != 0,
                                                        0, n_args, args, LispBinder::TypeFromObject);

                if (MethodInfo^ method = dynamic_cast<MethodInfo^>(members))
                {
                    array<ParameterInfo^>^ params = method->GetParameters();
                    array<Object^>^ real_args = args;
                    if (args->Length != params->Length)
                        real_args = gcnew array<Object^>(params->Length);
                    binder->BindArgs(params, allow_varying_args != 0, 0, n_args, args, real_args);
                    Object^ result = method->Invoke(object, real_args);

                    // Now pack all the outputs back into args. We have to
                    // be very careful here -- real_args and args may refer
                    
                    // We start by packing the by-ref output values at the
                    // end of the array to make room.
                    int by_ref_i = args->Length;
                    for (int j = params->Length-1; j >= 0; --j)
                    {
                        if (params[j]->ParameterType->IsByRef)
                            args[--by_ref_i] = real_args[j];
                    }
                    // Now build the final output going forwards:
                    int i = 0;
                    if (method->ReturnType != System::Void::typeid)
                    {
                        args[i++] = (args->Length - by_ref_i) + 1;
                        args[i++] = safe_cast<Int32^>(0);
                        args[i++] = result;
                    }
                    else
                    {
                        args[i++] = (args->Length - by_ref_i);
                    }

                    for (int j = 0; j < params->Length; ++j)
                    {
                        if (params[j]->ParameterType->IsByRef)
                        {
                            args[i++] = j + 1;
                            args[i++] = args[by_ref_i++];
                        }
                    }
                }
                else if (array<MethodBase^>^ methods = dynamic_cast<array<MethodBase^>^>(members))
                {
                    throw gcnew AmbiguousMatchException(String::Format("Multiple matches for call to {0}.", strname));
                }
                else
                {
                    throw gcnew MissingMethodException(String::Format("No matching method named {0} found.", strname));
                }
            }
            break;
        case MemberTypes::Field:
            {
                if (candidates->Length > 1)
                    throw gcnew AmbiguousMatchException(String::Format("Multiple matches for referend to field {0}.", strname));
                if (FieldInfo^ field = dynamic_cast<FieldInfo^>(candidates[0]))
                {
                    // Fields are a lot easier than methods. :)
                    args[0] = safe_cast<Int32^>(1);
                    args[1] = safe_cast<Int32^>(0);
                    args[2] = field->GetValue(object);
                }
            }
            break;
        default:
            throw gcnew Exception("Internal error: bad member type selected.");
        }
    }
    catch (TargetInvocationException^ e)
    {
        args[0] = -1;
        args[1] = e->GetBaseException();
    }
    catch (Exception^ e)
    {
        args[0] = -1;
        args[1] = e;
    }
    
}

//////////////////////////////////////////////////////////////////////////////
// Boxing functions

#define DEF_BOX(name, c_type) \
    clr_handle Box##name(c_type value) \
    { \
        return GetHandleFromObject(safe_cast<System::name^>(value)); \
    } \
    c_type Unbox##name(clr_handle handle) \
    { \
        return safe_cast<c_type>(GetObjectFromHandle(handle)); \
    }

DEF_BOX(Byte,    unsigned char);
DEF_BOX(Int16,   short);
DEF_BOX(Int32,   int);
DEF_BOX(Int64,   long long);
DEF_BOX(Double,  double);
DEF_BOX(Single,  float);

clr_handle BoxString(const char* value)
{
    return GetHandleFromObject(gcnew System::String(value));
}
const char* UnboxString(clr_handle handle)
{
    return reinterpret_cast<const char*>(Marshal::StringToHGlobalAnsi(safe_cast<String^>(GetObjectFromHandle(handle))).ToPointer());
}
clr_handle BoxChar(int value)
{
    return GetHandleFromObject(static_cast<System::Char^>((wchar_t)value));
}
int UnboxChar(clr_handle handle)
{
    return *safe_cast<Char^>(GetObjectFromHandle(handle));
}
clr_handle BoxBoolean(int value)
{
    return GetHandleFromObject(static_cast<System::Boolean^>(value != 0));
}
int UnboxBoolean(clr_handle handle)
{
    return *safe_cast<Boolean^>(GetObjectFromHandle(handle)) == true;
}
clr_handle BoxSingleFromDouble(double value)
{
    return GetHandleFromObject(safe_cast<Single^>((float)value));
}
double UnBoxDoubleFromSingle(clr_handle handle)
{
    return *safe_cast<Single^>(GetObjectFromHandle(handle));
}
