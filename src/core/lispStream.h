#ifndef	lispStream_H //[
#define lispStream_H

#define MERGE_FDSTREAM

#include <fstream>
#include <iostream>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include "foundation.h"
#include "object.h"
#include "numerics.h"
#include "character.h"
#include "pathname.fwd.h"
#include "lispVector.fwd.h"
#include "sourceFileInfo.fwd.h"
#include "strWithFillPtr.fwd.h"
#include "intStackQueue.h"


namespace core
{

// Define types of streams
// See ecl object.h:600

#define	C_STREAM	1


    typedef enum { brcl_stream_mode_input, brcl_stream_mode_output, brcl_stream_mode_io } BrclStreamModeEnum;

    class StreamCursor
    {
    private:
	/*! Tell that _LineNumber/_Column mean something */
	bool		_CursorIsValid;
	/*! Keep track of line number and column - if not valid return 0 */
	LongLongInt	_LineNumber;
	/*! Keep track of column - if not valid return 0 */
	uint		_Column;
    public:
	StreamCursor(): _CursorIsValid(true), _LineNumber(1), _Column(0) {};
    public:
	void advanceLineNumber(int num=1)
	{
	    this->_LineNumber += num;
	    this->_Column = 0;
	}
	void advanceColumn(int num=1)
	{
	    this->_Column++;
	}
	void invalidate() { this->_CursorIsValid = false;};
	void advanceForChar(char c)
	{
	    if (c == '\n' || c == '\r')
		this->advanceLineNumber();
	    else
		this->advanceColumn();
	}
	void backup() {
	    this->_Column--;
	}
    public:
	LongLongInt lineNumber() const { return this->_LineNumber;};
	uint column() const { return this->_Column;};
	bool atStartOfLine() const { return this->_Column==0;};
	bool isValid() const { return this->_CursorIsValid;};
    };


    /*! Used to store one unput char */
    class UnputChar
    {
    private:
	bool		_HasUnputChar;
	int		_UnputChar;
    public:
	UnputChar() : _HasUnputChar(false) {};
	void clear() { this->_HasUnputChar = false;};
	LongLongInt tellg() { return this->_HasUnputChar ? -1 : 0; };
	void putback(brclChar c) { this->_UnputChar = c; this->_HasUnputChar = true;};
	int get_char() { this->_HasUnputChar = false; return this->_UnputChar;};
	int peek_char() { return this->_UnputChar;};
	bool has_unput_char() { return this->_HasUnputChar;};
	void updateCursor(StreamCursor& c) { c.backup();};
    };



	// Line counting stuff for input streams
#define CURSOR_HANDLING_FUNCTIONS() \
	virtual uint lineNumber() const {return this->_Cursor.lineNumber();}; 		\
	virtual uint column() const {return this->_Cursor.column();};				\
	virtual void invalidateCursor() {this->_Cursor.invalidate();}			\
	virtual void advanceLineNumber(int num=1) {this->_Cursor.advanceLineNumber(num);}; \
	virtual void advanceColumn(int num=1) {this->_Cursor.advanceColumn(num);}; \

// last


};






namespace core
{
    SMART(Stream);
    class Stream_O : public T_O
    {
	LISP_BASE1(T_O);
	LISP_CLASS(core,ClPkg,Stream_O,"stream");
	DECLARE_INIT_GLOBALS();
    public:
	/*! Return the stream name as a pathname*/
	Pathname_sp pathname() const;

	virtual bool inputStreamP() const {return false;};
	virtual bool outputStreamP() const {return false;};
	virtual bool interactiveStreamP() const {return false;};

        virtual void setInteractive(bool i) { SUBIMP(); };


	virtual T_sp streamElementType() const { SUBIMP();};

	// Input stream methods
	virtual bool eof() const {SUBIMP();};
	virtual int peek_char() {SUBIMP();};
	/*! Low level character get */
	virtual int _get() {SUBIMP();};
	/*! Low level putback character */
	virtual void putback(char c) {SUBIMP();};
	/*! Low level listen function, returns the number of characters available to read */
	virtual int listen() {SUBIMP();};

	/*! Clear-input */
	virtual void clearInput();

	/*! Return next character and advance cursor */
	virtual int get(){SUBIMP();};

	/*! read_char see CLHS - return the next character in the stream */
//	virtual T_sp read_char(bool eof_error_p, T_sp eof_value, bool recursive_p){SUBIMP();};


	  
	virtual void unread_char(brclChar c) {SUBIMP();};

//	virtual void readLine(string& buf, bool& hitEof );
	virtual int read(unsigned char* buffer, int num );
	virtual LongLongInt gcount() {SUBIMP();};

	virtual LongLongInt tell() {SUBIMP();};
	virtual void seek(LongLongInt pos) {SUBIMP();};
	virtual LongLongInt fileSize() {SUBIMP();};


	// Output stream methods
	virtual int writeBytes(const char* c, int n);
	virtual void writeStr(string const& str);
	virtual void writeChar(brclChar c) {SUBIMP();};
	virtual void writeByte(Integer_sp c) {SUBIMP();};
	virtual void writeln(string const& str) {SUBIMP();};
	virtual Fixnum_sp writeVector(Vector_sp vec,Fixnum_sp start, Fixnum_sp end);

	virtual uint outputColumn() const {SUBIMP();};

	/*! This is forceOutput */
	virtual void flush() {SUBIMP();};

	/*! Clear-input */
	virtual void clearOutput() {SUBIMP();};
	virtual void finishOutput() {this->flush();};
	void forceOutput() { this->flush();};


	virtual bool atStartOfLine() const {SUBIMP();};

	// Line counting stuff for input streams
	virtual uint lineNumber() const {SUBIMP();};
	virtual uint column() const {SUBIMP();};
	virtual SourceFileInfo_sp sourceFileInfo() const {SUBIMP();};
	virtual void invalidateCursor() {SUBIMP();};
	virtual void advanceLineNumber(int num=1) {SUBIMP();};
	virtual void advanceColumn(int num=1) {SUBIMP();};

	virtual bool good() const {SUBIMP();};


	virtual string readEntireFile() {SUBIMP();};

	// Other
	virtual T_sp close(bool abort=false);
        Stream_O() {};
        virtual ~Stream_O() {};
    };

};
TRANSLATE(core::Stream_O);




namespace core
{
    SMART(Stream);
    class AnsiStream_O : public Stream_O
    {
	LISP_BASE1(Stream_O);
	LISP_CLASS(core,ExtPkg,AnsiStream_O,"AnsiStream");
	DECLARE_INIT_GLOBALS();
    public:
	virtual bool inputStreamP() const {return false;};
	virtual bool outputStreamP() const {return false;};


	// Input stream methods
	virtual bool eof() const {SUBIMP();};
	virtual int peek_char() {SUBIMP();};
	/*! Low level character get */
	virtual int _get() {SUBIMP();};
	/*! Low level putback character */
	virtual void putback(char c) {SUBIMP();};

	/*! Return next character and advance cursor */
	virtual int get();

	/*! read_char see CLHS - return the next character in the stream */
//	virtual T_sp read_char(bool eof_error_p, T_sp eof_value, bool recursive_p);


	  
	virtual void unread_char(brclChar c);

//	virtual void readLine(string& buf, bool& hitEof ) {SUBIMP();};
//	virtual int read(unsigned char* buffer, int num ){SUBIMP();};
	virtual LongLongInt gcount() {SUBIMP();};

	virtual LongLongInt tell() {SUBIMP();};
	virtual void seek(LongLongInt pos) {SUBIMP();};
	virtual LongLongInt fileSize() {SUBIMP();};


	// Output stream methods
	virtual void writeChar(brclChar c) {SUBIMP();};
	virtual void writeln(string const& str) {SUBIMP();};

	virtual void flush() {SUBIMP();};

	virtual bool atStartOfLine() const {SUBIMP();};
	// Line counting stuff for input streams
	virtual uint lineNumber() const {SUBIMP();};
	virtual uint column() const {SUBIMP();};
	virtual SourceFileInfo_sp sourceFileInfo() const {SUBIMP();};
	virtual void invalidateCursor() {SUBIMP();};
	virtual void advanceLineNumber(int num=1) {SUBIMP();};
	virtual void advanceColumn(int num=1) {SUBIMP();};

	virtual bool good() const {SUBIMP();};


	virtual string readEntireFile() {SUBIMP();};

	// Other
//	virtual T_sp close(bool abort=false);
	DEFAULT_CTOR_DTOR(AnsiStream_O);
    };

};
TRANSLATE(core::AnsiStream_O);





namespace core
{
    class FileStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,FileStream_O,"file-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(FileStream_O);
	
    private: // instance variables here
	
    public: // Functions here
    }; // FileStream class
    
}; // core namespace
TRANSLATE(core::FileStream_O);



namespace core
{
    class FileInStream_O : public FileStream_O
    {
	LISP_BASE1(FileStream_O);
	LISP_CLASS(core,CorePkg,FileInStream_O,"FileInStream");
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(FileInStream_O);
    public:
	void initialize();
	
    private: // instance variables here
	SourceFileInfo_sp 	_SourceFileInfo;
        std::ifstream	_InStream;
	UnputChar	_Unput;
	/*! Keep track of the line pos in the file */
	StreamCursor		_Cursor;
    public:
	static FileInStream_sp create(Pathname_sp fileSpec);
	static FileInStream_sp make(T_sp fileDesig);

    public: // Functions here
	virtual bool inputStreamP() const {return true;};
	virtual string readEntireFile();
	LongLongInt tell();
	int listen();
	void clearInput();
	void seek(LongLongInt);
	LongLongInt fileSize();
	SourceFileInfo_sp sourceFileInfo() const { return this->_SourceFileInfo;};
//	int read(unsigned char* buffer, int num );
	LongLongInt gcount();
	int _get();
	/*! Low level putback character */
	virtual void putback(char c);
	int peek_char();
//	void readLine(string& buf, bool& hitEof);
	bool good() const;
	bool eof() const;

	string __repr__() const;

	// Line counting stuff for input streams
	CURSOR_HANDLING_FUNCTIONS();

	T_sp close(bool abort=false);

    }; // FileInStream class
    
}; // core namespace
TRANSLATE(core::FileInStream_O);




namespace core
{
    class FileOutStream_O : public FileStream_O
    {
	LISP_BASE1(FileStream_O);
	LISP_CLASS(core,CorePkg,FileOutStream_O,"FileOutStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(FileOutStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit FileOutStream_O(core::Class_sp const& mc) : T_O(mc), FileStream(mc) {};
//    virtual ~FileOutStream_O() {};
    public:
	void initialize();

	
    private: // instance variables here
	Pathname_sp	_ActivePathname;
	bool		_RenameToOriginalOnClose;
	Pathname_sp	_OriginalPathname;
        std::ofstream	_Stream;
	StreamCursor	_OutputCursor;

    public:
 	static FileOutStream_sp create(Pathname_sp currentPath, std::ios_base::openmode mode);
	static FileOutStream_sp createTemporary(Pathname_sp temporaryPath, Pathname_sp originalPath, std::ios_base::openmode mode);

	
	
    public: // Functions here
	virtual bool outputStreamP() const {return true;};

	LongLongInt tellp() { return this->_Stream.tellp();};
	void seekp(LongLongInt pos) {this->_Stream.seekp(pos,std::ios_base::beg);};
	void flush() { this->_Stream.flush();};
	virtual void writeChar(brclChar c);
	virtual void writeln(const string& str);
	virtual uint outputColumn() const;
	virtual bool atStartOfLine() const;
	virtual T_sp close(bool abort=false);




    }; // FileOutStream class
    
}; // core namespace
TRANSLATE(core::FileOutStream_O);





namespace core
{
    class FileInCompressedStream_O : public FileStream_O
    {
	LISP_BASE1(FileStream_O);
	LISP_CLASS(core,CorePkg,FileInCompressedStream_O,"FileInCompressedStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(FileInCompressedStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit FileInCompressedStream_O(core::Class_sp const& mc) : T_O(mc), FileStream(mc) {};
//    virtual ~FileInCompressedStream_O() {};
    public:
	void initialize();
	
    public:
	typedef boost::iostreams::filtering_istream	StreamType;
    private: // instance variables here
	SourceFileInfo_sp 	_SourceFileInfo;
        std::ifstream	_RawStream;
	StreamType	_In;
	StreamCursor	_Cursor;
	UnputChar	_Unput;
    public:
	static FileInCompressedStream_sp createGzip(Pathname_sp fileSpec);

    public: // Functions here
	virtual bool inputStreamP() const {return true;};


	void clearInput();
	int listen();

	StreamType& stream() { return this->_In;};
	SourceFileInfo_sp sourceFileInfo() const { return this->_SourceFileInfo;};

	int _get();
	/*! Low level putback character */
	virtual void putback(char c);

//	virtual void readLine(string& sbuf, bool& hitEof);
//	virtual int read(unsigned char* buffer, int num );
	virtual LongLongInt gcount();

	virtual LongLongInt fileSize();
	bool good() const;
	virtual bool eof() const;
	virtual T_sp close(bool abort=false);

	virtual int peek_char();

	// Line counting stuff for input streams
	CURSOR_HANDLING_FUNCTIONS();



    }; // FileInCompressedStream class
    
}; // core namespace
TRANSLATE(core::FileInCompressedStream_O);




namespace core
{
    class FileOutCompressedStream_O : public FileStream_O
    {
	LISP_BASE1(FileStream_O);
	LISP_CLASS(core,CorePkg,FileOutCompressedStream_O,"FileOutCompressedStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(FileOutCompressedStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit FileOutCompressedStream_O(core::Class_sp const& mc) : T_O(mc), FileStream(mc) {};
//    virtual ~FileOutCompressedStream_O() {};
    public:
	void initialize();
	
    public:
	typedef boost::iostreams::filtering_ostream	StreamType;

    private: // instance variables here
	Pathname_sp	_ActivePathname;
	bool		_RenameActiveToOriginalOnClose;
	Pathname_sp		_OriginalPathname;
        std::ofstream	_RawStream;
	StreamType	_Out;
	StreamCursor	_Cursor;
    public:
	static FileOutCompressedStream_sp createGzip(Pathname_sp path);
	static FileOutCompressedStream_sp createGzipTemporary(Pathname_sp activePath, Pathname_sp originalPath);
	
	
    public: // Functions here
	virtual bool outputStreamP() const {return true;};


	LongLongInt tellp() { IMPLEMENT_ME();};
	void seekp(LongLongInt pos) { IMPLEMENT_ME();};
	void flush();
	virtual void writeChar(brclChar c);
	virtual void writeln(const string& str);
	virtual uint column() const;
	virtual bool atStartOfLine() const;
	virtual T_sp close(bool abort=false);




    }; // FileOutCompressedStream class
    
}; // core namespace
TRANSLATE(core::FileOutCompressedStream_O);









namespace core
{
    class StringStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,StringStream_O,"string-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(StringStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit StringStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
//    virtual ~StringStream_O() {};
	
    private: // instance variables here
	
	
    public: // Functions here
    }; // StringStream class
    
}; // core namespace
TRANSLATE(core::StringStream_O);



namespace core
{
    class StringInputStream_O : public StringStream_O
    {
	LISP_BASE1(StringStream_O);
	LISP_CLASS(core,CorePkg,StringInputStream_O,"StringInputStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(StringInputStream_O);
    public: // ctor/dtor for classes with shared virtual base
//    explicit StringInputStream_O(core::Class_sp const& mc) : T_O(mc),AnsiStream(mc) {};
//    virtual ~StringInputStream_O() {};
    public:
	void initialize();
	
    private: // instance variables here
        std::istringstream	_Stream;
	StreamCursor		_Cursor;
	UnputChar		_Unput;
    public:	// Creation class functions
	static StringInputStream_sp create(string const& contents);
	
	static StringInputStream_sp make(Str_sp string, Fixnum_sp start, T_sp end);
    public: // Functions here
	virtual bool inputStreamP() const {return true;};

	void clearInput();


	virtual SourceFileInfo_sp sourceFileInfo() const;

	virtual int peek_char();
	/*! Low level putback character */
	virtual void putback(char c);
	int _get();
	int listen();

//	virtual void readLine(string& sbuf, bool& hitEof);

	virtual void seek(LongLongInt pos);
	virtual LongLongInt tell();

	
	virtual LongLongInt fileSize();
//	virtual int read(unsigned char* buffer, int num );
	virtual LongLongInt gcount();
	virtual string readEntireFile();
	bool good() const;
	virtual bool eof() const;
	virtual T_sp close(bool abort=false);


	// Line counting stuff for input streams
	CURSOR_HANDLING_FUNCTIONS();




    }; // StringInputStream class
    
}; // core namespace
TRANSLATE(core::StringInputStream_O);
template<> struct gctools::GCInfo<core::StringInputStream_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = true;
    static bool constexpr Moveable = false;
    static bool constexpr Atomic = false;
};



namespace core
{
    class StringOutStream_O : public StringStream_O
    {
	LISP_BASE1(StringStream_O);
	LISP_CLASS(core,CorePkg,StringOutStream_O,"StringOutStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(StringOutStream_O);
    public:
	void initialize();
	
    private: // instance variables here
	StrWithFillPtr_sp _String;
	StreamCursor 	_OutputCursor;
    public:
	static StringOutStream_sp make();
	static StringOutStream_sp create(StrWithFillPtr_sp str);
	
    public: // Functions here
	StringOutStream_O( const StringOutStream_O& ss ); //!< Copy constructor

	virtual bool outputStreamP() const {return true;};

	void clear();	
	virtual void writeChar(brclChar c);
	virtual void writeln(const string& str);
	virtual bool atStartOfLine() const;
	virtual uint outputColumn() const;

	virtual void flush();

	virtual LongLongInt tell();

	/*! Return the string contents of the stream */
	string str();

	T_sp close(bool abort=false);

    }; // StringOutStream class
    
}; // core namespace
TRANSLATE(core::StringOutStream_O);
template<> struct gctools::GCInfo<core::StringOutStream_O> {
    static bool constexpr NeedsInitialization = true;
    static bool constexpr NeedsFinalization = false;
    static bool constexpr Moveable = true;
    static bool constexpr Atomic = false;
};






namespace core
{
    class FDStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,CorePkg,FDStream_O,"fd-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
        FDStream_O() : Base()
                     , _FStream(NULL)
                     , _Buffer(NULL)
                     , _Closeable(true)
#ifdef MERGE_FDSTREAM
                     , _RenameToOriginalOnClose(false)
                     , _OriginalPathname(_Nil<Pathname_O>())
#endif
        {};


	virtual ~FDStream_O();
	
    protected: // instance variables here
	FILE*	_FStream;
	char*	_Buffer;
	bool	_Closeable;
#ifdef MERGE_FDSTREAM
// From FDInStream
	SourceFileInfo_sp 		_SourceFileInfo;
	/*! Keep track of the line pos in the file */
	StreamCursor		_Cursor;
	LongLongInt		_gcount;
	UnputChar		_Unput;
// From FDOutStream
	Pathname_sp	_ActivePathname;
	bool		_RenameToOriginalOnClose;
	Pathname_sp	_OriginalPathname;
	StreamCursor	_OutputCursor;
#endif
    public:
	static FDStream_sp makeFromFileDescriptor(const string& name,
						  int fileDescriptor, 
						  Symbol_sp direction, /* :input, :output, :io */
						  T_sp elementType,
						  T_sp externalFormat );
    public: // Functions here
	virtual BrclStreamModeEnum getStreamMode() {SUBIMP();};
	void throw_if_no_file_descriptor() const;

	FDStream_sp setBufferingMode(Symbol_sp bufferModeSymbol);

	T_sp close(bool abort=false);
    }; // FDStream class
    
}; // core namespace
TRANSLATE(core::FDStream_O);



namespace core
{
    class FDInStream_O : virtual public FDStream_O
    {
	LISP_BASE1(FDStream_O);
	LISP_CLASS(core,CorePkg,FDInStream_O,"fd-in-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // ctor/dtor for classes with shared virtual base
	explicit FDInStream_O() : FDStream_O() {};
	virtual ~FDInStream_O() {};
    public:
	
    protected: // instance variables here
#ifndef MERGE_FDSTREAM
	SourceFileInfo_sp 		_SourceFileInfo;
	/*! Keep track of the line pos in the file */
	StreamCursor		_Cursor;
	LongLongInt		_gcount;
	UnputChar		_Unput;
#endif
    public:
	static FDInStream_sp create(Pathname_sp fileSpec);
	static FDInStream_sp create(FILE* fout, string const& name, bool closeable=true);
	static FDInStream_sp make(T_sp file_desig);
    public: // Functions here
	virtual BrclStreamModeEnum getStreamMode() {return brcl_stream_mode_input;};
	virtual bool inputStreamP() const {return true;};
	virtual string readEntireFile();
	LongLongInt tell();
	void seek(LongLongInt);
	LongLongInt fileSize();
	SourceFileInfo_sp sourceFileInfo() const { return this->_SourceFileInfo;};
//	int read(unsigned char* buffer, int num );
	LongLongInt gcount();
	int _get();
	int listen();
	/*! Low level putback character */
	virtual void putback(char c);
	void clearInput();
	int peek_char();
//	void readLine(string& buf, bool& hitEof);
	bool good() const;
	bool eof() const;

	// Line counting stuff for input streams
	CURSOR_HANDLING_FUNCTIONS();

    }; // FDInStream class
    
}; // core namespace
TRANSLATE(core::FDInStream_O);




namespace core
{
    class FDOutStream_O : virtual public FDStream_O
    {
	LISP_BASE1(FDStream_O);
	LISP_CLASS(core,CorePkg,FDOutStream_O,"fd-out-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // ctor/dtor for classes with shared virtual base
	explicit FDOutStream_O() : FDStream_O()
#ifndef MERGE_FDSTREAM
                                 , _RenameToOriginalOnClose(false)
                                 , _OriginalPathname(_Nil<Pathname_O>())
#endif
        {};
	virtual ~FDOutStream_O() {};
    public:

	
    protected: // instance variables here
#ifndef MERGE_FDSTREAM
	Pathname_sp	_ActivePathname;
	bool		_RenameToOriginalOnClose;
	Pathname_sp	_OriginalPathname;
	StreamCursor	_OutputCursor;
#endif
    public:
	static FDOutStream_sp create(FILE* fout, string const& name, bool closeable=true);
 	static FDOutStream_sp create(Pathname_sp currentPath, std::ios_base::openmode mode);
	static FDOutStream_sp createTemporary(Pathname_sp temporaryPath, Pathname_sp originalPath, std::ios_base::openmode mode );
	static FDOutStream_sp make(T_sp file_desig);

    public:
	void do_open(Pathname_sp currentPath, std::ios_base::openmode mode);
    public: // Functions here
	virtual BrclStreamModeEnum getStreamMode() {return brcl_stream_mode_output;};
	virtual bool outputStreamP() const {return true;};

	LongLongInt tellp();
	void seekp(LongLongInt pos);
	void flush();
	virtual void writeChar(brclChar c);
	virtual void writeln(const string& str);
	virtual uint outputColumn() const;
	virtual bool atStartOfLine() const;
	virtual T_sp close(bool abort=false);




    }; // FDOutStream class
    
}; // core namespace
TRANSLATE(core::FDOutStream_O);





namespace core {
    FORWARD(FDIOStream);
    class FDIOStream_O : public FDInStream_O, public FDOutStream_O
    {
	LISP_VIRTUAL_BASE2(FDStream_O,FDInStream_O,FDOutStream_O);
	LISP_CLASS(core,CorePkg,FDIOStream_O,"fd-io-stream");
//    DECLARE_ARCHIVE();
    public: // ctor/dtor for classes with shared virtual base
	explicit FDIOStream_O() : FDStream_O(), FDInStream_O(), FDOutStream_O() {};
	virtual ~FDIOStream_O() {};
    public:
	static FDIOStream_sp create(Pathname_sp fileSpec);
	static FDIOStream_sp create(FILE* fio, string const& name, bool closeable=true);
	static FDIOStream_sp make(T_sp file_desig);
    public: // Functions here
	virtual BrclStreamModeEnum getStreamMode() {return brcl_stream_mode_io;};
	virtual bool inputStreamP() const {return true;};
	virtual bool outputStreamP() const {return true;};

    }; // FDIOStream class
    
}; // core namespace
TRANSLATE(core::FDIOStream_O);











namespace core
{
    class SynonymStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,SynonymStream_O,"synonym-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	SynonymStream_O();
	virtual ~SynonymStream_O();
	
    protected: // instance variables here
	Symbol_sp 	_SynonymSymbol;

    public:
	static SynonymStream_sp make(Symbol_sp symbol);
    public: // Functions here


	T_sp close(bool abort=false);

	Stream_sp stream();
	Stream_sp stream() const;

	virtual bool inputStreamP() const;
	virtual string readEntireFile();
	LongLongInt tell();
	void seek(LongLongInt);
	LongLongInt fileSize();
	SourceFileInfo_sp sourceFileInfo() const;
//	int read(unsigned char* buffer, int num );
	LongLongInt gcount();
	string __repr__() const;
	int _get();
	int listen();

	/*! Low level putback character */
	virtual void putback(char c);
	int peek_char();
//	void readLine(string& buf, bool& hitEof);
	bool good() const;
	bool eof() const;

	void flush();

	void clearInput();

	virtual bool outputStreamP() const;

	virtual void writeChar(brclChar c);
	virtual void writeln(const string& str);
	virtual bool atStartOfLine() const;
	// Line counting stuff for input streams

	virtual uint lineNumber() const;
	virtual uint column() const;
	virtual uint outputColumn() const;
	virtual void invalidateCursor();
	virtual void advanceLineNumber(int num=1);
	virtual void advanceColumn(int num=1);


    }; // SynonymStream class
    
}; // core namespace
TRANSLATE(core::SynonymStream_O);









namespace core
{
    class TwoWayStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,TwoWayStream_O,"two-way-stream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	TwoWayStream_O();
	virtual ~TwoWayStream_O();
	
    protected: // instance variables here
	Stream_sp 	_in_stream;
	Stream_sp 	_out_stream;
        bool            interactive;
    public: // Functions here
	void throw_if_no_file_descriptor() const;
	T_sp close(bool abort=false);


    public:
	static TwoWayStream_sp make(Stream_sp in_stream, Stream_sp out_stream);
    public: // Functions here
	virtual bool inputStreamP() const {return true;};
	virtual string readEntireFile();
        virtual bool interactiveStreamP() const { return this->interactive;};
        virtual void setInteractive(bool i) { this->interactive = i;};
	LongLongInt tell();
	void seek(LongLongInt);
	LongLongInt fileSize();
	void clearInput();
//	int read(unsigned char* buffer, int num );
	LongLongInt gcount();
	int _get();
	int listen();

	/*! Low level putback character */
	virtual void putback(char c);
	int peek_char();
//	void readLine(string& buf, bool& hitEof);
	bool good() const;
	bool eof() const;

	virtual bool outputStreamP() const {return true;};

	string __repr__() const;

	void flush();
	virtual void writeChar(brclChar c);
	virtual void writeln(const string& str);
	virtual bool atStartOfLine() const;

	Stream_sp in_stream();
	Stream_sp in_stream() const;
	Stream_sp out_stream();
	Stream_sp out_stream() const;

	Stream_sp input_stream() { return this->in_stream();};
	Stream_sp output_stream() { return this->out_stream();};
	Stream_sp input_stream_const() const { return this->in_stream();};
	Stream_sp output_stream_const() const { return this->out_stream();};

	void advanceColumn(int num=1) {this->input_stream()->advanceColumn(num);};
	void advanceLineNumber(int num=1) {this->input_stream()->advanceLineNumber(num);};
	virtual uint lineNumber() const {return this->input_stream_const()->lineNumber();};
	virtual uint column() const {return this->input_stream_const()->column();}
	virtual SourceFileInfo_sp sourceFileInfo() const {return this->input_stream_const()->sourceFileInfo();};

	virtual uint outputColumn() const { return this->out_stream()->outputColumn();};

    }; // TwoWayStream class
    
}; // core namespace
TRANSLATE(core::TwoWayStream_O);


namespace core
{
    class BroadcastStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,BroadcastStream_O,"BroadcastStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(BroadcastStream_O);
	
    private: // instance variables here
	
	
    public: // Functions here
    }; // BroadcastStream class
    
}; // core namespace
TRANSLATE(core::BroadcastStream_O);




namespace core
{
    class ConcatenatedStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,ConcatenatedStream_O,"ConcatenatedStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(ConcatenatedStream_O);
	
    private: // instance variables here
	
	
    public: // Functions here
	int listen();

    }; // ConcatenatedStream class
    
}; // core namespace
TRANSLATE(core::ConcatenatedStream_O);



namespace core
{
    class EchoStream_O : public AnsiStream_O
    {
	LISP_BASE1(AnsiStream_O);
	LISP_CLASS(core,ClPkg,EchoStream_O,"EchoStream");
	DECLARE_INIT();
//    DECLARE_ARCHIVE();
    public: // Simple default ctor/dtor
	DEFAULT_CTOR_DTOR(EchoStream_O);
	
    private: // instance variables here
	
	
    public: // Functions here
	int listen();

    }; // EchoStream class


    T_sp af_peekChar(T_sp peek_type, T_sp strm, T_sp eof_errorp, T_sp eof_value, T_sp recursivep);
    T_sp af_readChar(T_sp ostrm, T_sp eof_error_p, T_sp eof_value, T_sp recursive_p);


    void initialize_lispStream();

}; // core namespace
TRANSLATE(core::EchoStream_O);



namespace core {

    Sequence_sp af_writeSequence(Sequence_sp seq, Stream_sp stream, Fixnum_sp start, Fixnum_sp end);

    Stream_sp brcl_makeStreamFromFD(const string& name, int fd,
				    BrclStreamModeEnum mode,
				    int byteSize,
				    int flags,
				    T_sp externalFormat );

    Stream_sp brcl_makeStreamFromFILE(const string& name,
				      FILE* file,
				      BrclStreamModeEnum mode,
				      int byteSize,
				      int flags,
				      T_sp externalFormat);


    bool cl_streamp(T_sp strm);

};






#endif //]
