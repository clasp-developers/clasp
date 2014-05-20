// start
#ifdef	DECLARE_SIGNAL_CONSTANTS
#endif



#ifdef	DEFINE_SIGNAL_CONSTANTS
#endif



#ifdef	SIGNAL_EXPOSE_TO_BOOST_PYTHON
#endif



#ifdef	SIGNAL_EXPOSE_CONVERSION_FUNCTIONS

string signalIdToName(uint sig)
{
    switch ( sig )
    {
        default: { return "undefinedSignal"; break; }
    }
};


uint signalNameToId(const string& name )
{
    return -1;

}
#endif
