namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    /// <summary>
    /// This class derives from ConcreteClass1 and hides all of its members.
    /// </summary>
    public class ShadowingClass1 : ConcreteClass1
    {
        #region Instance Members

        public new string MC1()
        {
            return "ShadowingClass1.MC1()";
        }

        string pc1 = "ShadowingClass1.PC1";
        public new string PC1
        {
            get { return pc1; }
            set { pc1 = value; }
        }

        public new string FC1 = "ShadowingClass1.FC1";

        #endregion
        #region Static Members

        // Static method
        public new static string MCS1()
        {
            return "ShadowingClass1.MCS1()";
        }

        // Static property
        static string pcs1 = "ShadowingClass1.PCS1";
        public new static string PCS1
        {
            get { return pcs1; }
            set { pcs1 = value; }
        }

        // Static field
        public new static string FCS1 = "ShadowingClass1.FCS1";

        #endregion
        #region Virtual Members
        // Virtual method
        public new virtual string MCV1()
        {
            return "ShadowingClass1.MCV1()";
        }
        // Virtual property
        string pcv1 = "ShadowingClass1.PCV1";
        public new virtual string PCV1
        {
            get { return pcv1; }
            set { pcv1 = value; }
        }
        #endregion
    }

}
