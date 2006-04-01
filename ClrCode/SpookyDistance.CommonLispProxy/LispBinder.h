ref class LispBinder : System::Reflection::Binder
{
    typedef System::Collections::Generic::List<System::Type^> TypeList;
    typedef System::Collections::Generic::SortedDictionary<System::Type^, TypeList^> TypeConversionDictionary;
    TypeConversionDictionary^ numeric_conversions;
    bool implicit_double_narrowing;

    void AddNumericConversions(System::Type^ from, ... array<System::Type^>^ to);
    void init();

public:
    LispBinder();
    // Providing "true" to this constructor allows doubles to be implicitly
    // narrowed to singles in order to match signatures and invoke members.
    LispBinder(bool implicit_double_down);

    // Low-level operations on types
    System::Object^ ConvertTo(System::Object^ obj, System::Type^ to);
    bool IsConvertibleTo(System::Type^ from, System::Type^ to);
    int BetterConversion(System::Type^ s, System::Type^ t1, System::Type^ t2);

    // Low-level operations on collections of arguments or types and parameter lists
    bool ArgTypesConform(int begin, int end,
                         array<System::Object^>^ args,
                         System::Type^ (type_accessor)(System::Object^),
                         array<System::Reflection::ParameterInfo^>^ params);
    int BetterArgsMatch(int begin, int end,
                        array<System::Object^>^ args,
                        System::Type^ (type_accessor)(System::Object^),
                        array<System::Reflection::ParameterInfo^>^ params1,
                        array<System::Reflection::ParameterInfo^>^ params2);
    bool ArgsConformToParams(array<System::Reflection::ParameterInfo^>^ params, bool allow_varying_args,
                             int begin_args, int end_args, array<System::Object^>^ args,
                             System::Type^ (type_accessor)(System::Object^ obj));

    // Static helper function to check for a "parameter array" parameter in the last
    // position of a parameter list. Returns nullptr if there is none.
    static System::Type^ VaryingArgsType(array<System::Reflection::ParameterInfo^>^ params);

    // Static helper functions that can be passed to the above.
    static System::Type^ TypeFromObject(System::Object^ obj); // Calls GetType
    static System::Type^ TypeFromType(System::Object^ obj);   // Assumes obj is a Type
    static array<System::Reflection::ParameterInfo^>^ ParamsFromPropertyInfo(System::Object^ info); // Assumes info is a PropertyInfo
    static array<System::Reflection::ParameterInfo^>^ ParamsFromMethodBase(System::Object^ info);   // Assumes info is a MethodBase

    // Set up an args array for an invocation call, given a parameter list
    // and a list of actual arguments. The input and output arg lists may be
    // the same. The output arg list length must exactly match the params
    // list length.
    void BindArgs(array<System::Reflection::ParameterInfo^>^ params,
                                     bool allow_varying_args,
                                     int begin_args,
                                     array<System::Object^>^ args,
                                     array<System::Object^>^ new_args);

    System::Object^ SelectMethods (array<System::Reflection::MemberInfo^>^ match,
                                   bool check_conformance,
                                   array<System::Reflection::ParameterInfo^>^ (params_accessor)(System::Object^ info),
                                   bool allow_varying_args,
                                   int begin_args,
                                   array<System::Object^>^ args,
                                   System::Type^ (type_accessor)(System::Object^ obj));

    // Implementation of abstract Binder methods.
    virtual System::Reflection::MethodBase^ SelectMethod (System::Reflection::BindingFlags bindingAttr, 
                                      array<System::Reflection::MethodBase^>^ match, 
                                      array<System::Type^>^ types, 
                                      array<System::Reflection::ParameterModifier>^ modifiers) override;
    virtual System::Reflection::FieldInfo^ BindToField (System::Reflection::BindingFlags bindingAttr, 
	                                array<System::Reflection::FieldInfo^>^ match,
                                    System::Object^ value,
                                    System::Globalization::CultureInfo^ culture) override;
    virtual System::Reflection::PropertyInfo^ SelectProperty (System::Reflection::BindingFlags bindingAttr, 
                                          array<System::Reflection::PropertyInfo^>^ match,
                                          System::Type^ returnType,
                                          array<System::Type^>^ indexes,
                                          array<System::Reflection::ParameterModifier>^ modifiers) override;
    int ReturnArraySize(System::Reflection::MethodInfo^ method);
    virtual System::Reflection::MethodBase^ BindToMethod (System::Reflection::BindingFlags bindingAttr,
                                      array<System::Reflection::MethodBase^>^match,
                                      array<System::Object^>^%args,
                                      array<System::Reflection::ParameterModifier>^ modifiers,
                                      System::Globalization::CultureInfo^ culture,
                                      array<System::String^>^names,
                                      [System::Runtime::InteropServices::Out]System::Object^% state ) override;
    virtual void ReorderArgumentArray (array<System::Object^>^% args,
                                       System::Object^ state) override;
    virtual System::Object^ ChangeType (System::Object^ value,
                                System::Type^ type,
                                System::Globalization::CultureInfo^ culture) override;
};