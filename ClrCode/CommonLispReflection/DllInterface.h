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
    // object_handle is handle for the object on which the member is 
    // to be found, or a null pointer for a static method call.
    //
    // type_handle is a handle of a System.Type object to be used for
    // member lookup. If null, then the object_handle must not be null,
    // and the object's runtime time is used.
    //
    // name is the name of the member.
    //
    // args_return must be the handle of a array<Object^>, which is used
    // to both pass in arguments and return the result value and by-ref
    // output values. On return, the first element is -1 if an exception was
    // thrown; a single exception object handle follows. Otherwise, the first
    // argument indicates the number of index/value pairs that follow. Index
    // 0 denotes the returned value. Index 1 denotes the value provided by the
    // first argument, if it was a by-ref argument. Etc. All indexes are
    // optional, and appear in ascending order.
    //
    // In order to accomodate all situations, the size of the input
    // args_return array must be 3 + N, where N is the number of arguments.
    //
    // TODO: Consider using a void* array instead, to avoid the need
    // to box the count and indexes on output. Have to figure out how
    // to unpack such a beast using a Lisp FFI.
    _declspec(dllexport) void invoke_member(clr_handle object_handle,
                                            clr_handle type_handle,
                                            const char* name,
                                            int n_args,
                                            clr_handle arg_return);

    // In order to use InvokeMember, we need a few functions to bootstrap
    // ourselves:
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
