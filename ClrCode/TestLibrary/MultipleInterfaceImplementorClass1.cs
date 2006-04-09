namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    public class MultipleInterfaceImplementorClass1 : Interface1, Interface2
    {
        public string MI1()
        {
            return "MultipleInterfaceImplementorClass1.MI1()";
        }
        string pi1 = "MultipleInterfaceImplementorClass1.PI1";
        public string PI1
        {
            get { return pi1; }
            set { pi1 = value; }
        }
    }

}
