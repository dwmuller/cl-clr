// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

#include "stdafx.h"
#include "DllInterface.h"
#include <assert.h>
#include <cstring>

using namespace System;
using namespace System::Reflection;
using namespace System::Runtime::InteropServices;
using namespace SpookyDistance::CommonLispReflection;

namespace SpookyDistance
{
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

    ref class ExceptionReturn
    {
    public:
        Exception^ exception;
        ExceptionReturn(Exception^ e)
            : exception(e)
        {
        }
    };

    clr_handle MakeExceptionReturnHandle(Exception^ e)
    {
        return GetHandleFromObject(gcnew ExceptionReturn(e));
    }
}
using namespace SpookyDistance;

//////////////////////////////////////////////////////////////////////////////
// Exported entry points


clr_handle returned_exception(clr_handle e_handle)
{
    ExceptionReturn^ e = dynamic_cast<ExceptionReturn^>(GetObjectFromHandle(e_handle));
    if (e)
        return GetHandleFromObject(e->exception);
    return 0;
}

clr_handle get_system_type(const char* name)
{
    try
    {
        return GetHandleFromObject(Type::GetType(gcnew String(name)));
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}
clr_handle make_array(int n, clr_handle element_type_handle)
{
    try
    {
        Type^ element_type = safe_cast<Type^>(GetObjectFromHandle(element_type_handle));
        return GetHandleFromObject(Array::CreateInstance(element_type, n));
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}
clr_handle get_array_element(clr_handle arry, int index)
{
    try
    {
        return GetHandleFromObject(safe_cast<Array^>(GetObjectFromHandle(arry))->GetValue(index));
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}
clr_handle set_array_element(clr_handle arry, int index, clr_handle obj)
{
    try
    {
        safe_cast<Array^>(GetObjectFromHandle(arry))->SetValue(GetObjectFromHandle(obj), index);
        return 0;
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}

clr_handle enum_value(clr_handle type_handle, const char* name)
{
    try
    {
        Object^ type_as_object = GetObjectFromHandle(type_handle);
        Type^ type = safe_cast<Type^>(type_as_object);
        return GetHandleFromObject(Enum::Parse(type, gcnew String(name)));
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}
// The foreign client system calls this to free an object for garbage collection.
 void release_object_handle(clr_handle handle)
{
    // Don't release a persistent singleton handle! (In release
    // builds, we don't want to pay the overhead of this check.)
    assert(!is_void_return(handle));
    if (handle)
        ((GCHandle)IntPtr(handle)).Free();
}

int type_type_code(const char* type_name)
{
    try
    {
        return safe_cast<Int32>(Convert::ChangeType(Enum::Parse(TypeCode::typeid, gcnew String(type_name)), Int32::typeid));
    }
    catch (Exception^)
    {
        return -1;
    }
}
int object_type_code(clr_handle object_handle)
{
    try
    {
        Object^ object = GetObjectFromHandle(object_handle);
        return safe_cast<Int32>(Convert::ChangeType(Type::GetTypeCode(object->GetType()), Int32::typeid));
    }
    catch (Exception^)
    {
        return -1;
    }
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
        Type^   type = dynamic_cast<Type^>(type_as_object);
        if (type_as_object && !type)
            throw gcnew ArgumentException("The first argument to invoke_member() is not null, and is not a System.Type object.");
        Object^ binder_as_object = GetObjectFromHandle(binder_handle);
        Binder^ binder = dynamic_cast<Binder^>(binder_as_object);
        if (binder_as_object && !binder)
            throw gcnew ArgumentException("The fourth argument to invoke_member() is not null, and is not a System.Reflection.Binder object.");
        String^ name  = gcnew String(name_str);
        Object^ args_as_object = GetObjectFromHandle(args_handle);
        array<Object^>^ args = dynamic_cast<array<Object^>^>(args_as_object);
        if (type_as_object && !type)
            throw gcnew ArgumentException("The last argument to invoke_member() is not null, and is not an array of System.Object.");

        BindingFlags bflags = BindingFlags(flags);
        Object^ result = LispBinder::InvokeWithLispSemantics(type, name, bflags, binder, object, args);
        return GetHandleFromObject(result);
    }
    catch (TargetInvocationException^ e)
    {
        return MakeExceptionReturnHandle(e->GetBaseException());
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
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

//////////////////////////////////////////////////////////////////////////////
// Lisp-specific functions
clr_handle make_lisp_binder(int allow_double_narrowing)
{
    try
    {
        return GetHandleFromObject(gcnew LispBinder(allow_double_narrowing != 0, true));
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}

int is_void_return(clr_handle v_handle)
{
    return GetObjectFromHandle(v_handle) == VoidReturn::Instance;
}

clr_handle wrap_varargs_array(clr_handle args_handle)
{
    try
    {
        Array^ args = safe_cast<Array^>(GetObjectFromHandle(args_handle));
        return GetHandleFromObject(VarArgs::Wrap(args));
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}

