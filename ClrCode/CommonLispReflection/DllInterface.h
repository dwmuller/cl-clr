// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

#pragma once

extern "C" {
    typedef void* clr_handle;
    // invoke_member is analogous to System.Type.InvokeMember. It can
    // invoke a method, retrieve a property value, or retrieve a
    // field value.
    //
    // The return value is a handle. If an exception is thrown, it
    // is a handle for which returned_exception() will return a
    // non-null handle. If the invocation returns no value, the
    // returned handle will evoke a true result from is_void_return().
    // Otherwise, it is the handle of the result value.
    //
    // Note that the void return handle is a persistent singleton. Do
    // not release it!
    //
    // object_handle is a handle for the object on which the member is 
    // to be found, or a null pointer for a static method call.
    //
    // type_handle is a handle of a System.Type object to be used for
    // member lookup. If null, then the object_handle must not be null,
    // and the object's runtime time is used.
    //
    // name is the name of the member.
    //
    // args_handle is the handle of an System.Array of System.Object,
    // the arguments.  If no arguments are required, this may be null.
    // On return, array elements corresponding to by-ref parameters
    // may have been updated. If an exception is indicated, the
    // state of the args array contents is unreliable.
    _declspec(dllexport) clr_handle invoke_member(clr_handle object_handle,
                                                  clr_handle type_handle,
                                                  const char* name,
                                                  clr_handle args_handle);

    // In order to use invoke_member, we need a few functions to bootstrap
    // ourselves:
    _declspec(dllexport) clr_handle returned_exception(clr_handle);
    _declspec(dllexport) int is_void_return(clr_handle);
    _declspec(dllexport) clr_handle get_default_app_domain();
    _declspec(dllexport) clr_handle make_object_array(int n);
    _declspec(dllexport) clr_handle get_array_element(clr_handle arry, int index);
    _declspec(dllexport) void set_array_element(clr_handle arry, int index, clr_handle obj);
    _declspec(dllexport) clr_handle wrap_varargs_array(clr_handle args);
    _declspec(dllexport) void release_object_handle(clr_handle handle);
    _declspec(dllexport) int is_simple_type(clr_handle obj, const char* type);

    // Would like to use stdint.h names here, but VS doesn't have that file.
    _declspec(dllexport) clr_handle box_Byte   (unsigned char);
    _declspec(dllexport) clr_handle box_Int16  (short);
    _declspec(dllexport) clr_handle box_Int32  (int);
    _declspec(dllexport) clr_handle box_Int64  (long long);
    _declspec(dllexport) clr_handle box_String (const char*);
    _declspec(dllexport) clr_handle box_Double (double);
    _declspec(dllexport) clr_handle box_Single (float);
    _declspec(dllexport) clr_handle box_Char   (int);
    _declspec(dllexport) clr_handle box_Boolean(int);
    _declspec(dllexport) clr_handle box_SingleFromDouble(double);

    _declspec(dllexport) unsigned char unbox_Byte   (clr_handle);
    _declspec(dllexport) short         unbox_Int16  (clr_handle);
    _declspec(dllexport) int           unbox_Int32  (clr_handle);
    _declspec(dllexport) long long     unbox_Int64  (clr_handle);
    _declspec(dllexport) const char*   unbox_String (clr_handle);
    _declspec(dllexport) double        unbox_Double (clr_handle);
    _declspec(dllexport) float         unbox_Single (clr_handle);
    _declspec(dllexport) int           unbox_Char   (clr_handle);
    _declspec(dllexport) int           unbox_Boolean(clr_handle);
    _declspec(dllexport) double        unbox_DoubleFromSingle(clr_handle);
}
