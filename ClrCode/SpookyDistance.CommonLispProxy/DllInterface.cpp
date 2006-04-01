// This is the main DLL file.

#include "stdafx.h"
#include "SpookyDistance.CommonLispProxy.h"
#include "LispBinder.h"
#include <assert.h>

using namespace System;
using namespace System::Reflection;
using namespace System::Runtime::InteropServices;


ref class TreatAs
{
public:
    Type^   effective_type;
    Object^ value;
};

// This is used internally to convert an object into a handle that we can
// safely give to the Lisp system.
static inline void* GetHandleFromObject(Object^ object)
{
    if (object == nullptr)
        return 0;
    return GCHandle::ToIntPtr(GCHandle::Alloc(object)).ToPointer();
}

// This is used to get the object referred to by a handle.
static inline Object^ GetObjectFromHandle(void* handle)
{
    if (!handle)
        return nullptr;
    return safe_cast<Object^>(((GCHandle)IntPtr(handle)).Target);
}

 // The Lisp system calls this to free an object for garbage collection.
 void ReleaseObjectHandle(void* handle)
{
    if (handle)
        ((GCHandle)IntPtr(handle)).Free();
}

void InvokeMember(Type^ type,
                  Object^ object,
                  const wchar_t* name,
                  int binding_flags,
                  int n_args,
                  void* arg_handles[],
                  InvocationResult* result)
{
    array<Object^>^ args = gcnew array<Object^>(n_args);
    for (int i = 0; i < n_args; ++i)
        args[i] = GetObjectFromHandle(arg_handles);
    Object^ returned = type->InvokeMember(gcnew String(name),
                                          (BindingFlags)binding_flags,
                                           nullptr, // default binder
                                           object,
                                           args);
        result->caught_exception = 0;
        result->object_handle = GetHandleFromObject(returned);
}
void SetMember(void* new_value_handle,
               Type^ type,
               Object^ object,
               const wchar_t* name,
               int binding_flags,
               int n_args,
               void* arg_handles[],
               InvocationResult* result)
{
    array<Object^>^ args = gcnew array<Object^>(n_args + 1);
    for (int i = 0; i < n_args; ++i)
        args[i] = GetObjectFromHandle(arg_handles);
    args[n_args] = GetObjectFromHandle(new_value_handle);
    Object^ returned = type->InvokeMember(gcnew String(name),
                                          (BindingFlags)binding_flags,
                                           nullptr, // default binder
                                           object,
                                           args);
        result->caught_exception = 0;
        result->object_handle = GetHandleFromObject(returned);
}
//////////////////////////////////////////////////////////////////////////////
// Exported entry points


// Everything starts from an application domain object:
void* GetRootAppDomain()
{
    return GetHandleFromObject(AppDomain::CurrentDomain);
}
void InvokeInstanceMember(void* object_handle,
                          const wchar_t* name,
                          int binding_flags,
                          int n_args,
                          void* arg_handles[],
                          InvocationResult* result)
{
    try
    {
        Object^ object = GetObjectFromHandle(object_handle);
        Type^ type = object->GetType();
        InvokeMember(type, object, name, binding_flags, n_args,
                    arg_handles, result);
    }
    catch (Exception^ e)
    {
        result->caught_exception = 1;
        result->object_handle = GetHandleFromObject(e);
    }
}
void InvokeStaticMember(void* type_handle,
                        const wchar_t* name,
                        int binding_flags,
                        int n_args,
                        void* arg_handles[],
                        InvocationResult* result)
{
    try
    {
        Type^ type = (Type^)GetObjectFromHandle(type_handle);
        InvokeMember(type, nullptr, name, binding_flags,
                     n_args, arg_handles, result);
    }
    catch (Exception^ e)
    {
        result->caught_exception = 1;
        result->object_handle = GetHandleFromObject(e);
    }
}
void SetInstanceMember(void* new_value_handle,
                       void* object_handle,
                       const wchar_t* name,
                       int binding_flags,
                       int n_args,
                       void* arg_handles[],
                       InvocationResult* result)
{
    try
    {
        Object^ object = GetObjectFromHandle(object_handle);
        Type^ type = object->GetType();
        SetMember(new_value_handle, type, object,
                  name, binding_flags, n_args,
                  arg_handles, result);
    }
    catch (Exception^ e)
    {
        result->caught_exception = 1;
        result->object_handle = GetHandleFromObject(e);
    }
}
void SetStaticMember(void* new_value_handle,
                     void* type_handle,
                     const wchar_t* name,
                     int binding_flags,
                     int n_args,
                     void* arg_handles[],
                     InvocationResult* result)
{
    try
    {
        Type^ type = (Type^)GetObjectFromHandle(type_handle);
        SetMember(new_value_handle, type, nullptr,
                  name, binding_flags,
                  n_args, arg_handles, result);
    }
    catch (Exception^ e)
    {
        result->caught_exception = 1;
        result->object_handle = GetHandleFromObject(e);
    }
}

ref class InvocableMemberPredicate {
    LispBinder^ binder;
    String^ name;
    array<Object^>^ args;
    bool allow_varying_args;
public:
    InvocableMemberPredicate(LispBinder^ b,
                             String^ nm,
                             bool allow_varargs,
                             array<Object^>^ a)
        : binder(b),
          name(nm), allow_varying_args(allow_varargs),
          args(a)
    {
    }
    bool IsInvocable(MemberInfo^ member, Object^)
    {
        if (member->Name == name)
            return true;
        if (MethodBase^ method = safe_cast<MethodBase^>(member))
        {
            if (method->IsSpecialName
                && method->Name->EndsWith(name)
                && method->Name->StartsWith("get_")
                && method->Name->Length == (name->Length + 4))
            {
                return binder->ArgsConformToParams(method->GetParameters(),
                                                   allow_varying_args,
                                                   0, args->Length, args,
                                                   LispBinder::TypeFromObject
                                                   );
            }
        }
        return false;
    }
};
void InvokeMember(void* object_handle,
                  void* type_handle,
                  const wchar_t* name,
                  int allow_varying_args,
                  int n_args,
                  void* arg_return)
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
        MemberFilter^ predicate = gcnew MemberFilter(gcnew InvocableMemberPredicate(binder, strname, false, args),
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
                                                        0, args, LispBinder::TypeFromObject);

                if (MethodInfo^ method = safe_cast<MethodInfo^>(members))
                {
                    array<ParameterInfo^>^ params = method->GetParameters();
                    array<Object^>^ real_args = args;
                    if (args->Length != params->Length)
                        real_args = gcnew array<Object^>(params->Length);
                    binder->BindArgs(params, allow_varying_args != 0, 0, args, real_args);
                    Object^ result = method->Invoke(object, real_args);
                    // Now pack all the outputs back into args. We have to
                    // be very careful here -- real_args and args may refer
                    // to the same array. (See LispBinder::BindArgs().)
                    
                    // We start by packing the by-ref output values at the
                    // end of the array to make room.
                    int by_ref_i = args->Length;
                    for (int j = params->Length-1; j >= 0; --j)
                    {
                        if (params[j]->ParameterType->IsByRef)
                            args[--by_ref_i] = real_args[j];
                    }
                    // Now build the final output going forwards:
                    int i = 1;
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
                else if (array<MethodBase^>^ methods = safe_cast<array<MethodBase^>^>(members))
                {
                    throw gcnew AmbiguousMatchException(String::Format("Multiple matches for call to {0}.", strname));
                }
                else
                {
                    throw gcnew MissingMethodException(String::Format("No matching method named {0} found.", strname));
                }
            }
        case MemberTypes::Field:
            {
                if (candidates->Length > 1)
                    throw gcnew AmbiguousMatchException(String::Format("Multiple matches for referend to field {0}.", strname));
                if (FieldInfo^ field = safe_cast<FieldInfo^>(candidates[0]))
                {
                    // Fields are a lot easier than methods. :)
                    args[0] = safe_cast<Int32^>(1);
                    args[1] = safe_cast<Int32^>(0);
                    args[2] = field->GetValue(object);
                }
            }
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
