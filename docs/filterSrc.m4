define(`__BEGIN_CLASS_DEFINITION',
typedef	boost::shared_ptr<$2> RP$3;
typedef	boost::weak_ptr<$2> WP$3;
class $2 : public $4 { 
    public:
      static boost::shared_ptr<$1> nil() { return rootClassManager().nilFromClassId<$1>($1::_classId()); }; 
      virtual bool isNil() const { return this==$1::nil().get(); };
    public: 
        typedef $4	Base; 
        typedef boost::shared_ptr<$2>	shared_ptr; 
        typedef boost::weak_ptr<$2>	weak_ptr; 
    public: 
	static string _className() { return "$3";};
	static string _baseClassName() { return $4::_className();};
	static uint _classId() { return $2::___classId; };
	static uint _baseClassId() { return $4::___classId; };
	static void ___setClassId(uint i) { $2::___classId = i; };
	static uint ___classId;
)

define(`__END_CLASS_DEFINITION',})
