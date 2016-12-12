#ifndef _syslog_generator_h_
#define _syslog_generator_h_

#include "udp_generator.hpp"

class SyslogGenerator : public UDPGenerator {
public:
    SyslogGenerator();
    virtual int start();
    virtual void stop();
    virtual const char* getPacketDescription();
    virtual void sendPackets(int threadid, unsigned long long seq);
private:
    sockaddr_storage m_dest;
    unsigned int m_num_packets_per_send = 1;

    static const int SEQ_BUFFER_SIZE = 64;

    int* m_sockets = NULL;
    struct msghdr* m_message_headers = NULL;
    struct iovec** m_message_iovs = NULL;
    char** m_seq_buffers = NULL;
};

#endif