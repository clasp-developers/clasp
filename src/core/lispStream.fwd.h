#ifndef lispStream_fwd_H
#define lispStream_fwd_H
namespace core
{
    FORWARD(Stream);
    FORWARD(AnsiStream);
    FORWARD(StringStream);
    FORWARD(StringInputStream);
    FORWARD(StringOutputStream);
    FORWARD(SynonymStream);
    FORWARD(TwoWayStream);
    FORWARD(BroadcastStream);
    FORWARD(EchoStream);
    FORWARD(FileStream);
    FORWARD(IOFileStream);
    FORWARD(IOStreamStream);
    FORWARD(ConcatenatedStream);


    void clasp_write(const boost::format& fmt, T_sp strm);


}
#endif
