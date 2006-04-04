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
clr_handle get_system_type(const char* name)
{
    return GetHandleFromObject(Type::GetType(gcnew String(name)));
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
int array_length(clr_handle arry)
{
    return safe_cast<Array^>(GetObjectFromHandle(arry))->Length;
}

int binding_flag(const char* name)
{
    BindingFlags flags = safe_cast<BindingFlags>(BindingFlags::typeid->InvokeMember(gcnew String(name),
                                                                                    BindingFlags::Static
                                                                                    | BindingFlags::GetField 
                                                                                    | BindingFlags::Public,
                                                                                    nullptr, nullptr, nullptr));
    return safe_cast<Int32>(Convert::ChangeType(flags, Int32::typeid));
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

clr_handle invoke_member(clr_handle type_handle,
                         const char* name_str,
                         int flags,
                         clr_handle binder_handle,
                         clr_handle object_handle,
                         clr_handle args_handle)
{
    try
    {
        // Let's unwrap all our presents:
        Object^ object = GetObjectFromHandle(object_handle);
        Object^ type_as_object = GetObjectFromHandle(type_handle);
        Type^ type   = type_as_object ? safe_cast<Type^>(type_as_object) : object->GetType();
        String^ name  = gcnew String(name_str);
        Object^ args_as_object = GetObjectFromHandle(args_handle);
        array<Object^>^ args = args_as_object ? safe_cast<array<Object^>^>(args_as_object) : nullptr;

        BindingFlags binding_flags = BindingFlags(flags);

        Object^ result = type->InvokeMember(name, binding_flags, gcnew LispBinder(true), object, args);

        if (result)
        {
            if (result->GetType() == Void::typeid)
                return VoidReturn::InstanceHandle;
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
clr_handle make_lisp_binder(int allow_double_narrowing)
{
    return GetHandleFromObject(gcnew LispBinder(allow_double_narrowing != 0));
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
