namespace SpookyDistance.CommonLispReflection.TestLibrary
{
    public class OverloadingClass1
    {
        public string MC1_Select(int i, string s1, string s2)
        {
            return string.Format("Selected {0} from (string, string): {1}",
                                 i, (i == 0) ? s1 : s2);

        }
        public string MC1_Select(int i, string s1, int s2)
        {
            return string.Format("Selected {0} from (string, int): {1}",
                                 i, (i == 0) ? (object)s1 : (object)s2);

        }
        public string MC1_Select(int i, double s1, int s2)
        {
            return string.Format("Selected {0} from (double, int): {1}",
                                 i, (i == 0) ? s1 : s2);

        }
        public string MC1_Select(int i, params object[] args)
        {
            return string.Format("Selected {0} from (params object[]): {1}", i, args[i]);
        }
    }
}
