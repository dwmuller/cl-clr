// SpookyDistance.CommonLispProxy.h

#pragma once

extern "C" {
    _declspec(dllexport) void ReleaseObjectHandle(void* handle);
    _declspec(dllexport) void* GetRootAppDomain();
    _declspec(dllexport) void* InvokeMember(void* object_handle, const wchar_t* name, int n_args, void* arg_handles[]);
}
