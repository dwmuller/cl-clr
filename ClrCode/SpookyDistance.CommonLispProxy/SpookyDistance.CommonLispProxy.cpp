// This is the main DLL file.

#include "stdafx.h"
#include <assert.h>
#include "SpookyDistance.CommonLispProxy.h"

using namespace System;
using namespace System::Reflection;
using namespace System::Runtime::InteropServices;

/*
    This file is an experiment in an alternative to RDNZL. The main motivation was to build in
    some reasonably efficient support for application domains. This is fairly difficult.
    Instance method calls across application domains via marshalled-by-ref objects is easy,
    but one has to take care that no object is unwrapped or unserialized in a domain unnecessarily,
    since this causes CLR to try to load the supporting assembly for that object's type. My thinking
    was that something like the RDNZL container could also serve as a cross-domain proxy to 
    reference objects in other domains. The basic idea looks promising; you just need to make
    sure that the code here is loadable in all participating domains, probably by installing it
    in the GAC.

    I stopped working on this because of some other hard questions that arose, which I don't have
    answers to:

    - Object identity. RDNZL does not deal with this. Proxies (containers in RDNZL) mask identity
      once, and the Lisp class used to identifiably contain proxy references and facilitate
      finalization masks it some more. Working around this is expensive, involving hash table
      lookups and weak references. Getting rid of the Lisp layer would require certain
      implementation-specific features from the Lisp FFI and finalization system which I haven't
      investigated. (Finalization is not a standard part of Lisp.)

    - Reference arguments/parameters. To really capture the semantics of these correctly in Lisp,
      every method invocation would have to be treated as a macro call, letting the caller
      tag arguments as references. In the presence of such references, it would expand to an
      invocation followed by SETF forms to updated the Lisp places with result values from
      the reference parameters.

*/
namespace SpookyDistance {
    namespace DomainHost {

        ref class InvocationResult;

        ref class Proxy : MarshalByRefObject
        {
        public:
            Proxy(Object^ object, Type^ declared_type)
                : obj(object), type(declared_type)
            {
            }
            Proxy(Object^ object)
                : obj(object), type(object->GetType())
            {
            }
            Proxy^ ApparentType();
            Type^ UnwrapApparentType();
            Object^ UnwrapTarget();
            InvocationResult^ InvokeMember(String^ member_name, array<Object^>^ args);
        private:
            Object^ obj;
            Type^ type;
        };

        ref class InvocationResult : MarshalByRefObject
        {
        public:
            static InvocationResult^ TheNullResult = gcnew InvocationResult(nullptr);
            static InvocationResult^ TheVoidResult = gcnew InvocationResult(nullptr);

            InvocationResult(Proxy^ obj, bool exception)
                :IsException(exception), result(obj) {} 
            InvocationResult(Proxy^ obj)
                :IsException(false), result(obj) {} 

            bool IsException;
            property bool IsNull {
                bool get() {return this == TheNullResult;}
            }
            property bool IsVoid {
                bool get() {return this == TheVoidResult;}
            }
            Proxy^ result;

        };

        static void UnwrapArg(Object^ arg, Object^& real_arg)
        {
            if (Proxy^ proxy = safe_cast<Proxy^>(arg))
            {
                real_arg = proxy->UnwrapTarget();
            }
            else
            {
                // Not yet sure if we should ever get here. Perhaps simple types
                // and/or marshal-by-value types won't be wrapped in a proxy.
                assert(0); 
                real_arg = arg;
            }
        }
        static void UnwrapArgAndType(Object^ arg,
                                     Object^& real_arg,
                                     Type^&   apparent_type)
        {
            if (Proxy^ proxy = safe_cast<Proxy^>(arg))
            {
                real_arg = proxy->UnwrapTarget();
                apparent_type = proxy->UnwrapApparentType();
            }
            else
            {
                // Not yet sure if we should ever get here. Perhaps simple types
                // and/or marshal-by-value types won't be wrapped in a proxy.
                assert(0); 
                real_arg = arg;
                apparent_type = arg->GetType();
            }
        }

        static void UnwrapArgs(array<Object^>^ args,
                               array<Object^>^& real_args)
        {
            int len = args->Length;
            real_args = gcnew array<Type^>(len);
            for (int i = 0; i < len; ++i)
            {
                Object^ real_arg;
                UnwrapArg(args[i], real_arg);
                real_args[i] = real_arg;
            }
        }

        static void UnwrapArgsAndTypes(array<Object^>^ args,
                                       array<Object^>^& real_args,
                                       array<Type^>^& types)
        {
            int len = args->Length;
            real_args = gcnew array<Type^>(len);
            types = gcnew array<Type^>(len);
            for (int i = 0; i < len; ++i)
            {
                Object^ real_arg;
                Type^ apparent_type;
                UnwrapArgAndType(args[i], real_arg, apparent_type);
                real_args[i] = real_arg;
                types[i] = apparent_type;
            }
        }

        static Proxy^ GetField(Object^ instance,
                               array<MemberInfo^>^ members)
        {
            // CLS-compliant types cannot overload fields.
            if (members->Length > 1)
                throw gcnew Exception(
                String::Format("Unable to select from overloaded fields \"{0}\" of type \"{1}\".",
                              members[0]->Name, members[0]->ReflectedType->FullName));
            FieldInfo^ fi = safe_cast<FieldInfo^>(members[0]);
            return gcnew Proxy(fi->GetValue(instance));
        }
        static Proxy^ GetProperty(Object^ instance,
                                  array<MemberInfo^>^ members,
                                  array<Object^>^ indexes)
        {
            PropertyInfo^ member;
            array<Object^>^ real_args;
            if (members->Length == 1) // Common case
            {
                member = safe_cast<PropertyInfo^>(members[0]);
                UnwrapArgs(indexes, real_args);
            }
            else
            {
                array<PropertyInfo^>^ pis = gcnew array<PropertyInfo^>(members->Length);
                for (int i = 0; i < members->Length; ++i)
                    pis[i] = safe_cast<PropertyInfo^>(members[i]);
                array<Type^>^ types;
                UnwrapArgsAndTypes(indexes, real_args, types);
                member = Type::DefaultBinder->SelectProperty(BindingFlags::Default,
                                                             pis,
                                                             nullptr,
                                                             types,
                                                             nullptr);
            }
            return gcnew Proxy(member->GetValue(instance, real_args), member->PropertyType);
        }
        static Proxy^ InvokeMethod(Object^ instance,
                                   array<MemberInfo^>^ members,
                                   array<Object^>^ args)
        {

            return nullptr;
        }
        Proxy^ Proxy::ApparentType()
        {
            return gcnew Proxy(type, Type::typeid);
        }
        Type^ Proxy::UnwrapApparentType()
        {
            return type;
        }
        Object^ Proxy::UnwrapTarget()
        {
            return obj;
        }
        InvocationResult^ Proxy::InvokeMember(String^ member_name, array<Object^>^ args)
        {
            try
            {
                // We could use System.Type.InvokeMember here in one step, except
                // for the fact that we need to know the 'apparent' type of the
                // invocation result. So we're forced to do a separate lookup
                // of the member info structure, because it provides that info.
                // We will, however, try to emulate System.Type.InvokeMember in
                // that we will treat property and field access as if they were
                // the invocation of a method. The downside to this is that
                // we do not deal well with like-named members of different kinds.
                // But note that types that define these are not CLS-compliant.
                //
                // Note that we use the apparent type, not the real type, for
                // looking up methods.
                array<MemberInfo^>^ members = type->GetMember(member_name);
                if (members->Length == 0)
                {
                    throw gcnew Exception(
                      String::Format("No instance member \"{0}\" found for type \"{1}\".",
                                     member_name, type->FullName));
                }

                // Let's look at the first member info, and assume that they're all the 
                // same kind.
                Proxy^ result;
                switch (members[0]->MemberType)
                {
                case MemberTypes::Field:
                    if (args->Length > 0)
                        throw gcnew Exception("No arguments allowed in field accessor calls.");
                    result = GetField(obj, members);
                    break;
                case MemberTypes::Property:
                    result = GetProperty(obj, members, args);
                    break;
                case MemberTypes::Method:
                    result = InvokeMethod(obj, members, args);
                    break;
                default:
                    throw gcnew Exception(
                        String::Format("Member \"{0}\" of type \"{1}\" is an unknown or unhandled kind.",
                                       member_name, type->FullName));
                }
                return gcnew InvocationResult(result);
            }
            catch (Exception^ e)
            {
                return gcnew InvocationResult(gcnew Proxy(e), true);
            }
        }

        // The Lisp system calls this to free an object for garbage collection.
        void ReleaseObjectHandle(void* handle)
        {
            ((GCHandle)IntPtr(handle)).Free();
        }

        // This is used internally to convert an object into a handle that we can
        // safely give to the Lisp system.
        static inline void* GetHandleFromObject(Object^ object)
        {
            return GCHandle::ToIntPtr(GCHandle::Alloc(object)).ToPointer();
        }

        // This is used to get the object referred to by a handle.
        static inline Object^ GetObjectFromHandle(void* handle)
        {
            return safe_cast<Object^>(((GCHandle)IntPtr(handle)).Target);
        }

        // Everything starts from an application domain object:
        void* GetRootAppDomain()
        {
            return GetHandleFromObject(gcnew Proxy(AppDomain::CurrentDomain, System::AppDomain::typeid));
        }
        void* InvokeMember(void* object_handle, const wchar_t* name, int n_args, void* arg_handles[])
        {
            Object^ object = GetObjectFromHandle(object_handle);
            Proxy^ proxy = safe_cast<Proxy^>(object);
            array<Object^>^ args = gcnew array<Object^>(n_args);
            for (int i = 0; i < n_args; ++i)
                args[i] = GetObjectFromHandle(arg_handles);
            return GetHandleFromObject(proxy->InvokeMember(gcnew String(name), args));
        }
        void* ApparentTypeOf(void* object_handle)
        {
            Object^ object = GetObjectFromHandle(object_handle);
            if (Proxy^ proxy = dynamic_cast<Proxy^>(object))
                return GetHandleFromObject(proxy->ApparentType());
            return GetHandleFromObject(gcnew Proxy(object->GetType(), Type::typeid));
        }
    }
}