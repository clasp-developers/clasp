define(`__BEGIN_CLASS_DEFINITION',
typedef	boost::shared_ptr<$1> RP$2;
typedef	boost::weak_ptr<$1> WP$2;
class $1 : public $3 { 
    public:
      static boost::shared_ptr<$1> nil() { return rootClassManager().nilFromClassSID<$1>($1::_classSID()); }; 
      virtual bool isNil() const { return this==$1::nil().get(); };
    public: 
        typedef $3	Base; 
        typedef boost::shared_ptr<$1>	shared_ptr; 
        typedef boost::weak_ptr<$1>	weak_ptr; 
    public: 
	static string _className() { return "$2";};
	static string _baseClassName() { return $3::_className();};
	static uint _classSID() { return $1::___classSID; };
	static uint _baseClassSID() { return $3::___classSID; };
	static void ___setClassSID(uint i) { $1::___classSID = i; };
	static uint ___classSID;
)

define(`__END_CLASS_DEFINITION',})
