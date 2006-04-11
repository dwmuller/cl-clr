namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    /// <summary>
    /// This class derives from ConcreteClass1, but declares no
    /// members of its own.
    /// </summary>
    public class TrivialDerivedClass1 : ConcreteClass1
    {
        string name;
        public TrivialDerivedClass1()
        {
            name = "None";
        }
        public TrivialDerivedClass1(string name)
        {
            this.name = name;
        }
        public override string ToString()
        {
            return name;
        }
    }
}
