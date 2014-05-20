#ifndef CommandLineOptions_H
#define CommandLineOptions_H

namespace core {


struct CommandLineOptions {
    CommandLineOptions(int argc, char* argv[]);

    bool 		_DontLoadInit;
    bool 		_IgnoreInitImage;
    std::vector<std::string>	_Features;
    std::string 		_ExecCode;
    std::string 		_LoadFile;
    bool 		_GotRandomNumberSeed;
    long		_RandomNumberSeed;
    bool		_Interactive;
    bool 		_Version;
    bool 		_SilentStartup;
    std::vector<std::string>	_Args;
};



};
#endif
