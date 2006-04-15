// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

#include "stdafx.h"
#include "DllInterface.h"
#include <assert.h>
#include <cstring>

using namespace System;
using namespace System::Reflection;
using namespace System::Reflection::Emit;
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

    int n_outstanding_handles = 0;

    // This is used internally to convert an object into a handle that we can
    // safely give to the Lisp system.
    static inline clr_handle GetHandleFromObject(Object^ object)
    {
        if (object == nullptr)
            return 0;
        ++n_outstanding_handles;
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

    ref class ForeignCallback
    {
        int identifier;
        foreign_callback *callback;
        release_callback *release;
    public:
        ~ForeignCallback()
        {
            release(identifier);
        }
        ForeignCallback(int id, foreign_callback* cb, release_callback* rel)
        {
            identifier = id;
            callback = cb;
            release = rel;
        }
        Object^ Invoke(array<Object^>^ args)
        {
            clr_handle args_handle = 0;
            clr_handle result_handle = 0;
            try
            {
                args_handle = GetHandleFromObject(args);
                result_handle = callback(identifier, args->Length, args_handle);
                return GetObjectFromHandle(result_handle);
            }
            finally
            {
                release_object_handle(result_handle);
                release_object_handle(args_handle);
            }
        }
    };
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
    if (handle)
    {
        --n_outstanding_handles;
        ((GCHandle)IntPtr(handle)).Free();
    }
}
clr_handle new_object_handle(clr_handle obj)
{
    return GetHandleFromObject(GetObjectFromHandle(obj));
}
int number_of_unreleased_handles()
{
    return n_outstanding_handles;
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
// Callbacks

clr_handle make_callback_delegate(int id,
                                  foreign_callback* callback,
                                  release_callback* release,
                                  clr_handle delegate_type_handle)
{
    // We create the delegate as a dynamic method. A delegate created this way
    // is a static method. It takes as arguments: First, the instance to which the
    // delegate is bound; then, the aguments dictated by the delegate type.
    //
    // What our delegate actually does is copy the delegate arguments (but not the
    // bound object instance) to an array of objects. The instance that we bind
    // the delegate to is of type ForeignCallback, which acts as intermediary to
    // the C-style callback function. ForeignCallback.Invoke takes the argument
    // array as its sole argument, and invokes the callback.
    //
    // ForeignCallback also has a finalizer that lets the foreign system know when
    // the callback is no longer needed.
    //
    try
    {
        Type^ type = safe_cast<Type^>(GetObjectFromHandle(delegate_type_handle));
        ForeignCallback^ fobj = gcnew ForeignCallback(id, callback, release);

        MethodInfo^ invoke_method = type->GetMethod("Invoke");
        Type^ return_type = invoke_method->ReturnType;
        array<ParameterInfo^>^ parameter_infos = invoke_method->GetParameters();
        array<Type^>^ parameter_types = gcnew array<Type^>(parameter_infos->Length + 1);
        for (int i = parameter_infos->Length; i > 0; --i)
            parameter_types[i] = parameter_infos[i-1]->ParameterType;
        parameter_types[0] = ForeignCallback::typeid;

        DynamicMethod^ delegate_method = gcnew DynamicMethod("Dynamic_CL_CLR_Delegate",
                                                             return_type,
                                                             parameter_types,
                                                             ForeignCallback::typeid->Module);
        ILGenerator^ gen = delegate_method->GetILGenerator(256);
        LocalBuilder^ array_local = gen->DeclareLocal(array<Object^>::typeid);

        // The goal is to take all arguments and pass them in an array to
        // ForeignCallback.Invoke, called on the specific ForeignCallback
        // object that we just created.
        gen->Emit(OpCodes::Ldc_I4, parameter_infos->Length);
        gen->Emit(OpCodes::Newarr, Object::typeid);
        // Save the array reference in a local variable.
        gen->Emit(OpCodes::Stloc, array_local);

        for (int i = 0; i < parameter_infos->Length; ++i)
        {
            // Note that the first arg to this routine is the instance ref,
            // other args start at offset 1.
            ParameterInfo^ param = parameter_infos[i];

            // Push the array ref, index and value
            gen->Emit(OpCodes::Ldloc, array_local);
            gen->Emit(OpCodes::Ldc_I4, i);
            gen->Emit(OpCodes::Ldarg, i+1);
            // If it's a value type, box it.
            if (param->ParameterType->IsValueType)
                gen->Emit(OpCodes::Box, parameter_infos[i]->ParameterType);
            // store the value in the array
            gen->Emit(OpCodes::Stelem_Ref);
        }

        // Call ForeignCallback.Invoke.
        gen->Emit(OpCodes::Ldarg_0);            // Push object reference.
        gen->Emit(OpCodes::Ldloc, array_local); // Push array reference.
        MethodInfo^ callback_method = ForeignCallback::typeid->GetMethod("Invoke");
        gen->Emit(OpCodes::Call, callback_method);

        // Deal with the returned value, if any.
        if (return_type == Void::typeid)
            gen->Emit(OpCodes::Pop); // Discard return value
        else
        {
            LocalBuilder^ local_result = gen->DeclareLocal(Object::typeid);
            gen->Emit(OpCodes::Stloc, local_result);
            gen->Emit(OpCodes::Ldloc, local_result);
            if (return_type->IsValueType)
            {

                array<Type^>^ types = { Object::typeid, Type::typeid };
                MethodInfo^ convert_method = Convert::typeid->GetMethod("ChangeType", types);
                gen->Emit(OpCodes::Ldtoken, return_type);
                gen->Emit(OpCodes::Call, Type::typeid->GetMethod("GetTypeFromHandle"));
                gen->Emit(OpCodes::Call, convert_method);
                gen->Emit(OpCodes::Unbox_Any, return_type);
            }
        }
        gen->Emit(OpCodes::Ret);

        // Wrap it up as a delegate bound to the ForeignCallback object.
        Delegate^ result = delegate_method->CreateDelegate(type, fobj);

        return GetHandleFromObject(result);
    }
    catch (Exception^ e)
    {
        return MakeExceptionReturnHandle(e);
    }
}

clr_handle invoke_delegate(clr_handle delegate_handle, clr_handle args_handle)
{
    try
    {
        Delegate^ delegate = safe_cast<Delegate^>(GetObjectFromHandle(delegate_handle));
        array<Object^>^ args = safe_cast<array<Object^>^>(GetObjectFromHandle(args_handle));
        return GetHandleFromObject(delegate->DynamicInvoke(args));
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

