// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

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

ref class InvocableMemberPredicate {
    LispBinder^ binder;
    String^ name;
    array<Object^>^ args;
public:
    InvocableMemberPredicate(LispBinder^ b,
                             String^ nm,
                             array<Object^>^ a)
        : binder(b),
          name(nm),
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
                                                   args,
                                                   LispBinder::TypeFromObject);
            }
        }
        return false;
    }
};

ref class ExceptionReturn
{
public:
    Exception^ exception;
    ExceptionReturn(Exception^ e)
        : exception(e)
    {
    }
};
ref class VoidReturn
{
    VoidReturn () {}
    static VoidReturn^ instance = gcnew VoidReturn;
    static clr_handle  instanceHandle = GetHandleFromObject(Instance);
public:
    static property VoidReturn^ Instance {
        VoidReturn^ get () {return instance;}
    }
    static property clr_handle  InstanceHandle {
        clr_handle get () {return instanceHandle;}
    }
};

//////////////////////////////////////////////////////////////////////////////
// Exported entry points

clr_handle returned_exception(clr_handle e_handle)
{
    ExceptionReturn^ e = dynamic_cast<ExceptionReturn^>(GetObjectFromHandle(e_handle));
    if (e)
        return GetHandleFromObject(e->exception);
    return 0;
}

int is_void_return(clr_handle v_handle)
{
    return v_handle == VoidReturn::InstanceHandle;
}

// Everything starts from an application domain object:
clr_handle get_default_app_domain()
{
    return GetHandleFromObject(AppDomain::CurrentDomain);
}

clr_handle wrap_varargs_array(clr_handle args)
{
    Array^ arry = safe_cast<Array^>(GetObjectFromHandle(args));
    return GetHandleFromObject(LispBinder::VarArgsBase::WrapVarArgs(arry));
}

clr_handle make_object_array(int n)
{
    return GetHandleFromObject(gcnew array<Object^>(n));
}

clr_handle get_array_element(clr_handle arry, int index)
{
    return GetHandleFromObject(safe_cast<Array^>(GetObjectFromHandle(arry))->GetValue(index));
}
void set_array_element(clr_handle arry, int index, clr_handle obj)
{
    safe_cast<Array^>(GetObjectFromHandle(arry))->SetValue(GetObjectFromHandle(obj), index);
}
// The Lisp system calls this to free an object for garbage collection.
 void release_object_handle(clr_handle handle)
{
    // Don't release a persistent singleton handle! (In release
    // builds, we don't want to pay the overhead of this check.)
    assert(handle != VoidReturn::InstanceHandle);
    if (handle)
        ((GCHandle)IntPtr(handle)).Free();
}

int is_simple_type(clr_handle obj, const char* type_name)
{
    return GetObjectFromHandle(obj)->GetType()->FullName == gcnew String(type_name);
}

clr_handle invoke_member(clr_handle object_handle,
                         clr_handle type_handle,
                         const char* name,
                         clr_handle args_handle)
{
    array<Object^>^ args = safe_cast<array<Object^>^>(GetObjectFromHandle(args_handle));
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
        MemberFilter^ predicate = gcnew MemberFilter(gcnew InvocableMemberPredicate(binder, strname, args),
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

        Object^ result = nullptr;
        switch (kind)
        {
        case MemberTypes::Method:
            {
                Object^ members = binder->SelectMethods(candidates,
                                                        false, // Don't check type conformance, we already did.
                                                        LispBinder::ParamsFromMethodBase,
                                                        args, LispBinder::TypeFromObject);

                if (MethodInfo^ method = dynamic_cast<MethodInfo^>(members))
                {
                    array<ParameterInfo^>^ params = method->GetParameters();
                    array<Object^>^ real_args = binder->BindArgs(params, args);

                    result = method->Invoke(object, real_args);

                    if (real_args != args)
                        binder->ReorderArgumentArray(real_args, args);

                    if (method->ReturnType == System::Void::typeid)
                        return VoidReturn::InstanceHandle;
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
                    result = field->GetValue(object);
            }
            break;
        default:
            throw gcnew Exception("Internal error: bad member type selected.");
        }
        return GetHandleFromObject(result);
    }
    catch (TargetInvocationException^ e)
    {
        return GetHandleFromObject(gcnew ExceptionReturn(e->GetBaseException()));
    }
    catch (Exception^ e)
    {
        return GetHandleFromObject(gcnew ExceptionReturn(e));
    }
    
}


//////////////////////////////////////////////////////////////////////////////
// Boxing functions

#define DEF_BOX(name, c_type) \
    clr_handle box_##name(c_type value) \
    { \
        return GetHandleFromObject(safe_cast<System::name^>(value)); \
    } \
    c_type unbox_##name(clr_handle handle) \
    { \
        return safe_cast<c_type>(GetObjectFromHandle(handle)); \
    }

DEF_BOX(Byte,    unsigned char);
DEF_BOX(Int16,   short);
DEF_BOX(Int32,   int);
DEF_BOX(Int64,   long long);
DEF_BOX(Double,  double);
DEF_BOX(Single,  float);

clr_handle box_String(const char* value)
{
    return GetHandleFromObject(gcnew System::String(value));
}
const char* unbox_String(clr_handle handle)
{
    return reinterpret_cast<const char*>(Marshal::StringToHGlobalAnsi(safe_cast<String^>(GetObjectFromHandle(handle))).ToPointer());
}
clr_handle box_Char(int value)
{
    return GetHandleFromObject(static_cast<System::Char^>((wchar_t)value));
}
int unbox_Char(clr_handle handle)
{
    return *safe_cast<Char^>(GetObjectFromHandle(handle));
}
clr_handle box_Boolean(int value)
{
    return GetHandleFromObject(static_cast<System::Boolean^>(value != 0));
}
int unbox_Boolean(clr_handle handle)
{
    return *safe_cast<Boolean^>(GetObjectFromHandle(handle)) == true;
}
clr_handle box_SingleFromDouble(double value)
{
    return GetHandleFromObject(safe_cast<Single^>((float)value));
}
double unbox_DoubleFromSingle(clr_handle handle)
{
    return *safe_cast<Single^>(GetObjectFromHandle(handle));
}
