#include <iostream>
#include <sys/socket.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <memory.h>

#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>
#include <net-snmp/agent/net-snmp-agent-includes.h>
#include "rate_limiter.hpp"

int resolvehelper(const char *hostname, int family, const char *service, sockaddr_storage *pAddr) {
    int result;
    addrinfo *result_list = NULL;
    addrinfo hints = {};
    hints.ai_family = family;
    hints.ai_socktype = SOCK_DGRAM; // without this flag, getaddrinfo will return 3x the number of addresses (one for each socket type).
    result = getaddrinfo(hostname, service, &hints, &result_list);
    if (result == 0) {
        memcpy(pAddr, result_list->ai_addr, result_list->ai_addrlen);
        freeaddrinfo(result_list);
    }
    return result;
}

void sendSyslogMessages(int rate) {
    ssize_t result = 0;
    int sock = socket(AF_INET, SOCK_DGRAM, 0);

    char szIP[100];

    sockaddr_in addrListen = {}; // zero-int, sin_port is 0, which picks a random port for bind.
    addrListen.sin_family = AF_INET;
    result = bind(sock, (sockaddr *) &addrListen, sizeof(addrListen));
    if (result == -1) {
        int lasterror = errno;
        std::cout << "error: " << lasterror;
        std::exit(1);
    }

    sockaddr_storage addrDest = {};
    result = resolvehelper("127.0.0.1", AF_INET, "1514", &addrDest);
    if (result != 0) {
        int lasterror = errno;
        std::cout << "error: " << lasterror;
        std::exit(1);
    }

    const char *msg = "<190>Mar 11 08:35:17 fw01 30228451: Mar 11 08:35:16.844 CST: %SEC-6-IPACCESSLOGP: list in110 denied tcp 10.99.99.1(63923) -> 10.98.98.1(1521), 1 packet";
    size_t msg_length = strlen(msg);

    RateLimiterInterface* limiter = new RateLimiter();
    limiter->set_rate(rate);
    while(true) {
        result = sendto(sock, msg, msg_length, 0, (sockaddr *) &addrDest, sizeof(addrDest));
        limiter->aquire();
    }
    free(limiter);
}

oid             objid_sysuptime[] = { 1, 3, 6, 1, 2, 1, 1, 3, 0 };
oid              trap_oid[] =       { 1, 3, 6, 1, 6, 3, 1, 1, 4, 1, 0 };
oid             objid_id[] = { 1, 3, 6, 1, 6, 3, 1, 1, 5, 1 };


void sendSnmpTraps(int rate) {
    netsnmp_session session, *ss;
    netsnmp_pdu    *pdu, *response;
    char *trap = NULL;

    char comm[] = "public";
    snmp_sess_init( &session );
    session.version = SNMP_VERSION_2c;
    session.community = (u_char*)comm;
    session.community_len = strlen((const char*)session.community);
    session.peername = (char*)("127.0.0.1:1262");
    ss = snmp_open(&session);
    if (!ss) {
        snmp_sess_perror("ack", &session);
        std::exit(1);
    }

    pdu = snmp_pdu_create(SNMP_MSG_TRAP2);
    pdu->community = (u_char*)comm;
    pdu->community_len = strlen(comm);
    pdu->trap_type = SNMP_TRAP_ENTERPRISESPECIFIC;

    long sysuptime;
    char csysuptime [20];
    sysuptime = get_uptime ();
    sprintf (csysuptime, "%ld", sysuptime);
    trap = csysuptime;
    snmp_add_var (pdu, objid_sysuptime, sizeof (objid_sysuptime)/sizeof(oid),'t', trap);
    snmp_add_var(pdu, trap_oid, OID_LENGTH(trap_oid), 'o', ".1.3.6.1.1.6.3.1.1.5.1");
    snmp_add_var(pdu, objid_id, OID_LENGTH(objid_id) , 's', "ABC");

    RateLimiterInterface* limiter = new RateLimiter();
    limiter->set_rate(rate);
    while(true) {
        send_trap_to_sess(ss, pdu);
        limiter->aquire();
    }

    free(limiter);
    snmp_close(ss);
}

int main(int argc, char **argv) {
    int rate = 40000;
    char traps = 0;
    int c;
    while ((c = getopt (argc, argv, "r:t")) != -1) {
        switch (c)
        {
            case 'r':
                rate = atoi(optarg);
                break;
            case 't':
                traps = 1;
                break;
            default:
                printf("Invalid option.");
                exit(1);
        }
    }

    if (traps) {
        printf("Sending SNMPv2 traps at target rate of %d traps per second\n", rate);
        sendSnmpTraps(rate);
    } else {
        printf("Sending syslog messages at target rate of %d message per seconds\n", rate);
        sendSyslogMessages(rate);
    }


    return 0;
}