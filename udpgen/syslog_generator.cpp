#include "syslog_generator.hpp"
#include <sys/socket.h>
#include <netdb.h>
#include <iostream>
#include <cstring>
#include <unistd.h>

SyslogGenerator::SyslogGenerator() {
    setPort(1514);
}

int SyslogGenerator::start() {
    char service[16];
    snprintf(service, 16, "%d", getPort());

    if (resolvehelper(getHost(), AF_INET, service, &m_dest)) {
        std::cout << "error: " << errno << std::endl;
        return -1;
    }

    m_sockets = std::unique_ptr<int[]>(new int[getNumThreads()]);
    for (int i = 0; i < getNumThreads(); ++i) {
        m_sockets[i] = socket(AF_INET, SOCK_DGRAM, 0);
        sockaddr_in addrListen = {}; // zero-int, sin_port is 0, which picks a random port for bind.
        addrListen.sin_family = AF_INET;
        if (bind(m_sockets[i], (sockaddr *) &addrListen, sizeof(addrListen))) {
            std::cout << "error: " << errno << std::endl;
            return -1;
        }
    }

    m_buffers = std::unique_ptr<char*[]>(new char*[getNumThreads()]);
    for (int i = 0; i < getNumThreads(); ++i) {
        m_buffers[i] = new char[BUFFER_SIZE];
    }

    return UDPGenerator::start();
}

void SyslogGenerator::stop() {
    UDPGenerator::stop();
    for (int i = 0; i < getNumThreads(); ++i) {
        close(m_sockets[i]);
        m_sockets[i] = -1;
    }

    for (int i = 0; i < getNumThreads(); ++i) {
        delete m_buffers[i];
        m_buffers[i] = NULL;
    }
}

const char* SyslogGenerator::getPacketDescription() {
    return "Syslog Messages";
}

void SyslogGenerator::sendPacket(int threadid, unsigned long long seq) {
    char* buffer = m_buffers[threadid];
    snprintf(buffer, (size_t)BUFFER_SIZE, "<190>Mar 11 08:35:17 fw01 %llu: Mar 11 08:35:16.844 CST: %%SEC-6-IPACCESSLOGP: list in110 denied tcp 10.99.99.1(63923) -> 10.98.98.1(1521), 1 packet", seq);
    size_t num_bytes_to_send = strlen(buffer);
    ssize_t num_bytes_sent = sendto(m_sockets[threadid], buffer, num_bytes_to_send, 0, (sockaddr *)&m_dest, sizeof(m_dest));
    if (num_bytes_sent != num_bytes_to_send) {
        std::cout << "Send failed. Only sent " << num_bytes_sent << " out of " <<  num_bytes_to_send << " bytes!" << std::endl;
    }
}
