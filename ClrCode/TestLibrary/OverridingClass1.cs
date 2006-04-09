namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    /// <summary>
    /// This class derives from ConcreteClass1 and overrides
    /// all virtual members.
    /// </summary>
    public class OverridingClass1 : ConcreteClass1
    {
        public override string MCV1()
        {
            return "OverridingClass1.MCV1()";
        }
        string pcv1 = "OverridingClass1.PCV1";
        public override string PCV1
        {
            get { return pcv1; }
            set { pcv1 = value; }
        }
    }
}
