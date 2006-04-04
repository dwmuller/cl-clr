// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

#pragma once

extern "C" {
    // All of the functions int the DLL interface deal with clr_handle
    // values. A CLR handle designates a CLR object, and keeps it from
    // being garbage-collected. To release an object for collection, call
    // release_object_handle().
    //
    // Note that two different handles may designate the same object.
    typedef void* clr_handle;

    // Releases a handle, allowing the referenced CLR object to be
    // garbage-collected eventually.
    //
    // If more than one handle references an object, the latter won't
    // be garbage-collected until some time after its last handle has
    // been released.
    _declspec(dllexport) void release_object_handle(clr_handle handle);


    // invoke_member calls System.Type.InvokeMember. It can
    // invoke a method, retrieve a property value, or retrieve a
    // field value. The arguments are as described for Type.InvokeMember,
    // with one exception. If type_handle is the null handle (zero),
    // then the type object used for the call will be the one returned
    // by a call to GetType() on the object referenced by object_handle.
    //
    // args_handle may be the null handle if no arguments are supplied.
    // On return, array elements corresponding to by-ref parameters
    // may have been updated. If an exception is indicated, (see below)
    // the args array contents are unreliable.
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
    _declspec(dllexport) clr_handle invoke_member(clr_handle type_handle,
                                                  const char* name_str,
                                                  int flags,
                                                  clr_handle binder_handle,
                                                  clr_handle object_handle,
                                                  clr_handle args_handle);

    // In order to use invoke_member, we need a few functions to mark
    // input arguments in various ways and to interpret returned results.

    // Make an object array. The primary use for this is in preparing argument
    // arrays for invoke_member. The following three functions are used to efficiently
    // manipulate the array contents, and will work with any System.Array object.
    _declspec(dllexport) clr_handle make_object_array(int n);
    _declspec(dllexport) clr_handle get_array_element(clr_handle arry, int index);
    _declspec(dllexport) void set_array_element(clr_handle arry, int index, clr_handle obj);
    _declspec(dllexport) int array_length(clr_handle arry);

    // If the input handle represents an exception that resulted from a call
    // to invoke_member, then this returns a handle for the exception.
    _declspec(dllexport) clr_handle returned_exception(clr_handle);

    // If the input handle represents a void return type (see invoke_member()),
    // this return 1, otherwise 0.
    _declspec(dllexport) int is_void_return(clr_handle);

    // Returns a handle to the default System.AppDomain object. This is a useful
    // starting point for all kinds of activities, especially for finding types.
    _declspec(dllexport) clr_handle get_default_app_domain();

    // This returns a handle to a type object for the named system type. This
    // is important for bootstrapping, letting you get to basic types like System.Type
    // and System.Convert. It's not reliable for non-system types unless the name
    // is assembly-qualified.
    _declspec(dllexport) clr_handle get_system_type(const char* name);

    // Can't do much in the way of invoking without binding flags. This converts
    // a binding flag name string to its corresponding bitmask.
    _declspec(dllexport) int binding_flag(const char* name);

    // Returns a handle to a new LispBinder object. These objects derive from
    // System.Binder. The LispBinder has several interesting characteristics
    // that make it more suitable than the default binder for dynamic languages
    // like Lisp. These are documented elsewhere.
    //
    // If the allow_double_narrowing argument is non-zero, then the binder
    // allows double arguments to match single float parameters, and narrows
    // them implicitly. This is useful if the host language does not support
    // the single float type, as is the case for at least one popular Lisp
    // implementation.
    _declspec(dllexport) clr_handle make_lisp_binder(int allow_double_narrowing);

    // Wrap an argument array (a handle to a CLR array) in a marker object
    // recognized by the Lisp binder. The marker object tells the binder to
    // treat the array as the value to pass to a parameter that has the
    // ParamsArrayAttribute verbatim, without further wrapping. This must
    // be used only with the last argument of an argument list. This is
    // the LispBinder's way of avoiding the odd rules that C# uses to
    // decide if a last argument is to be treated as an array of optional
    // parameters, or as a single optional parameter. Using this wrapper makes
    // the former usage explicit; the latter usage is always used otherwise.
    _declspec(dllexport) clr_handle wrap_varargs_array(clr_handle args);

    // Take an object and the name of a type, and return true if the 
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
