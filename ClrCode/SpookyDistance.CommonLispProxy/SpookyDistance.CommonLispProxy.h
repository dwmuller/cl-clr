// SpookyDistance.CommonLispProxy.h

#pragma once

extern "C" {
    struct InvocationResult
    {
        void* object_handle;
        int   caught_exception;
    };
    _declspec(dllexport) void ReleaseObjectHandle(void* handle);
    _declspec(dllexport) void* GetRootAppDomain();
    _declspec(dllexport) void InvokeInstanceMember(void* object_handle,
                                                   const wchar_t* name,
                                                   int n_args,
                                                   void* arg_handles[],
                                                   InvocationResult*);
    _declspec(dllexport) void InvokeStaticMember(void* object_handle,
                                                 const wchar_t* name,
                                                 int n_args,
                                                 void* arg_handles[],
                                                 InvocationResult*);
    _declspec(dllexport) void SetInstanceMember(void* new_value_handle,
                                                void* object_handle,
                                                const wchar_t* name,
                                                int n_args,
                                                void* arg_handles[],
                                              InvocationResult*);
    _declspec(dllexport) void SetStaticMember(void* new_value_handle,
                                              void* object_handle,
                                              const wchar_t* name,
                                              int n_args,
                                              void* arg_handles[],
                                              InvocationResult*);

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
    // to box count and indexes on output.
    _declspec(dllexport) void InvokeMember(void* object_handle,
                                           void* type_handle,
                                           const wchar_t* name,
                                           int n_args,
                                           void* arg_return);
}
