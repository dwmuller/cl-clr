namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    /// <summary>
    /// This class contains a variety of concrete member types.
    /// </summary>
    public class ConcreteClass1
    {
        #region Instance Members
        public string MC1()
        {
            return "ConcreteClass1.MC1()";
        }

        string pc1 = "ConcreteClass1.PC1";
        public string PC1
        {
            get { return pc1; }
            set { pc1 = value; }
        }

        public string FC1 = "ConcreteClass1.FC1";

        public void MC2()
        {
        }
        #endregion
        #region Static Members

        // Static method
        public static string MCS1()
        {
            return "ConcreteClass1.MCS1()";
        }

        // Static property
        static string pcs1 = "ConcreteClass1.PCS1";
        public static string PCS1
        {
            get { return pcs1; }
            set { pcs1 = value; }
        }

        // Static field
        public static string FCS1 = "ConcreteClass1.FCS1";

        #endregion
        #region Virtual Members
        // Virtual method
        public virtual string MCV1()
        {
            return "ConcreteClass1.MCV1()";
        }
        // Virtual property
        string pcv1 = "ConcreteClass1.PCV1";
        public virtual string PCV1
        {
            get { return pcv1; }
            set { pcv1 = value; }
        }
        #endregion
    }
}
