using System;
using System.Collections.Generic;
using System.Text;

namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    public delegate void DoSomethingFromNothingDelegate();
    public delegate void DoSomethingFromObjectDelegate(object arg);
    public delegate int ReturnIntFromNothingDelegate();
    public delegate object ReturnObjectFromArgsDelegate(int value_arg,
                                                        StringEncapsulator ref_arg1,
                                                        StringEncapsulator ref_arg2);
    
    public class EventGeneratorClass1
    {
        public event DoSomethingFromNothingDelegate DoSomethingFromNothing;
        public event DoSomethingFromObjectDelegate DoSomethingFromObject;
        public event ReturnIntFromNothingDelegate ReturnIntFromNothing;
        public event ReturnObjectFromArgsDelegate ReturnObjectFromArgs;
        public void RaiseDoSomethingFromNothing()
        {
            DoSomethingFromNothing();
        }
        public void RaiseDoSomethingFromObject(object arg)
        {
            DoSomethingFromObject(arg);
        }
        public int RaiseReturnIntFromNothing()
        {
            return ReturnIntFromNothing();
        }
        public object RaiseReturnObjectFromArgs(int value_arg,
                                                StringEncapsulator ref_arg1,
                                                StringEncapsulator ref_arg2)
        {
            return ReturnObjectFromArgs(value_arg, ref_arg1, ref_arg2);
        }    
    }
}
