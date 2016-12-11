#ifndef _syslog_generator_h_
#define _syslog_generator_h_

#include "udp_generator.hpp"

class SyslogGenerator : public UDPGenerator {
public:
    SyslogGenerator();
    virtual int start();
    virtual void stop();
    virtual const char* getPacketDescription();
    virtual void sendPacket(int threadid, unsigned long long seq);
private:
    sockaddr_storage m_dest;

    static const int BUFFER_SIZE = 512;

    std::unique_ptr<char*[]> m_buffers;
    std::unique_ptr<int[]> m_sockets;
};

#endif