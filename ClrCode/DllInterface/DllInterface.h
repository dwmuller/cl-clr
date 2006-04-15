// $Id:$
//
// Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.

#pragma once

extern "C" {
    // All of the functions int the DLL interface deal with clr_handle values.
    // A CLR handle designates a CLR object, and keeps it from being
    // garbage-collected. To release an object for collection, call
    // release_object_handle().
    //
    // Note that two different handles may designate the same object.
    typedef void* clr_handle;

    // Releases a handle, allowing the referenced CLR object to be
    // garbage-collected eventually.
    //
    // If more than one handle references an object, the latter won't be
    // garbage-collected until some time after its last handle has been
    // released.
    _declspec(dllexport) void release_object_handle(clr_handle handle);

    // There are rare situations in which it is important to create a
    // duplicate handle for an object, e.g. when you know a receiver of a
    // handle will free it but the caller isn't done with the object.
    // This function gets a new handle for an object.
    _declspec(dllexport) clr_handle new_object_handle(clr_handle obj);

    // To assist with debugging and measurement, this function returns
    // the number of allocated-but-unrelease handles.
    _declspec(dllexport) int number_of_unreleased_handles();

    // invoke_member calls System.Type.InvokeMember. It can invoke a method,
    // retrieve a property value, or retrieve a field value. The arguments are
    // as described for Type.InvokeMember, with one exception. If type_handle
    // is the null handle (zero), then the type object used for the call will
    // be the one returned by a call to GetType() on the object referenced by
    // object_handle.
    //
    // args_handle may be the null handle if no arguments are supplied. On
    // return, array elements corresponding to by-ref parameters may have been
    // updated. If an exception is indicated, (see below) the args array
    // contents are unreliable.
    //
    // The return value is a handle. If an exception is thrown, it is a handle
    // for which returned_exception() will return a non-null handle. If the
    // invocation returns no value, the returned handle will evoke a true
    // result from is_void_return(). Otherwise, it is the handle of the result
    // value.
    //
    // Note that the void return handle is a persistent singleton. Do not
    // release it!
    //
    _declspec(dllexport) clr_handle invoke_member(clr_handle type_handle,
                                                  const char* name_str,
                                                  int flags,
                                                  clr_handle binder_handle,
                                                  clr_handle object_handle,
                                                  clr_handle args_handle);

    // In order to use invoke_member, we need a few functions to mark
    // input arguments in various ways and to interpret returned results.

    // Make an array. The primary use for this is in preparing argument
    // arrays for invoke_member. The following three functions are used to
    // efficiently manipulate the array contents, and will work with any
    // System.Array object. Note that set_array_element() normally returns
    // zero, unless an exception is thrown, in which case see
    // returned_exception().
    _declspec(dllexport) clr_handle make_array(int n, clr_handle element_type_handle);
    _declspec(dllexport) clr_handle get_array_element(clr_handle arry, int index);
    _declspec(dllexport) clr_handle set_array_element(clr_handle arry, int index, clr_handle obj);

    // If the input handle represents an exception that resulted from a call
    // to any function defined in this file, then this returns a handle for
    // the exception.
    _declspec(dllexport) clr_handle returned_exception(clr_handle);

    // If the input handle represents a void return type (see
    // invoke_member()), this return 1, otherwise 0.
    _declspec(dllexport) int is_void_return(clr_handle);

    // This returns a handle to a type object for the named system type. This
    // is important for bootstrapping, letting you get to basic types like
    // System.Type and System.Convert. It's not reliable for non-system types
    // unless the name is assembly-qualified.
    _declspec(dllexport) clr_handle get_system_type(const char* name);

    // Can't do much in the way of invoking without binding flags. This
    // provides an easy way to convert any enum name, given its type object,
    // to an enum value.
    _declspec(dllexport) clr_handle enum_value(clr_handle type, const char* name);

    // Wrap an argument array (a handle to a CLR array) in a marker object
    // recognized by the Lisp binder. The marker object tells the binder to
    // treat the array as the value to pass to a parameter that has the
    // ParamsArrayAttribute verbatim, without further wrapping. This must be
    // used only with the last argument of an argument list. This is the
    // LispBinder's way of avoiding the odd rules that C# uses to decide if a
    // last argument is to be treated as an array of optional parameters, or
    // as a single optional parameter. Using this wrapper makes the former
    // usage explicit; the latter usage is always used otherwise.
    _declspec(dllexport) clr_handle wrap_varargs_array(clr_handle args_handle);

    // Take the name of a type, and return the type code. This may only work
    // for system type names. Returns -1 on error. See
    // System.Type.GetTypeCode.
    _declspec(dllexport) int type_type_code(const char* type_name);

    // Take an object, and return its type's type code. Returns -1 on error.
    _declspec(dllexport) int object_type_code(clr_handle object_handle);

    /////////////////////////////////////////////////////////////////////////
    // Callbacks

    // Prototype of callback functions. The identifier is the one given
    // make_callback_delegate(), and args_handle is the handle of a
    // System.Array of System.Object. The caller is responsible for releasing
    // the handle when the callback returns; the callback function must not
    // keep a reference to the array. The argument count is provided to spare
    // the callback the need to call back into the CLR for the array length.
    typedef clr_handle (foreign_callback)(int id, int n_args, clr_handle args_handle);

    // Prototype of a callback release function. This is called when a
    // delegate is finalized. See make_callback_delegate().
    typedef void (release_callback)(int id);

    // Create a delegate that will call back to the given callback function.
    // Whenever the delegate is invoked, the callback will be invoked with the
    // given identifier. When the delegate is finalized, the release callback
    // will be invoked with the identifier.
    _declspec(dllexport) clr_handle make_callback_delegate(int id,
                                                           foreign_callback* callback,
                                                           release_callback* release,
                                                           clr_handle delegate_type_handle);

    // Invoke a delegate with the given arguments. The caller is responsible
    // for releasing the argument array after the call. This is useful for testing.
    _declspec(dllexport) clr_handle invoke_delegate(clr_handle callback_handle,
                                                    clr_handle args_handle);
    /////////////////////////////////////////////////////////////////////////
    // Box various basic types as CLR reference objects.
    //
    // Would like to use stdint.h names here, but VS doesn't have that file.
    // These will not return exception indicators; no exceptions should be
    // possible short of imminent asteroidal impact.
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

    // Unbox various basic types. Be careful with these; handing
    // an inappropriate value throws an exception, likely causing
    // system failure.
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

    // LispBinder
    _declspec(dllexport) clr_handle make_lisp_binder(int allow_double_narrowing);
}
