       


using namespace core;

class	FFFile {
public:
	string		line;
	ifstream	fin;

	void	open(string fname) { fin.open(fname,ios::read); };
	void	close()		   { fin.close(); };
	void	seek(std::ifstream::pos_type pos)	{
			fin.seek(pos,std::ios_base::beg);
	};
	std::ifstream::pos_type tell() {
			return fin.tellp();
	};
	string	nextWord() {
	    ;
	string	nextLine() {string l = line; this.getLine(); return l; };
	string	peakLine() {return line;};
};
	

