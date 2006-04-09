namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    public class InterfaceImplementorClass1 : Interface1
    {
        public string MI1()
        {
            return "InterfaceImplementorClass1.MI1()";
        }
        string pi1 = "InterfaceImplementorClass1.PI1";
        public string PI1
        {
            get { return pi1; }
            set { pi1 = value; }
        }
    }
}
