#include <iostream>
#include <sys/socket.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <memory.h>
#include <thread>
#include <atomic>

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


std::atomic_ullong counter(0);

void sendSyslogMessages(const char* host, const int port, const int rate, RateLimiterInterface* limiter) {
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

    char service[128];
    snprintf(service, 128, "%d", port);

    sockaddr_storage addrDest = {};
    result = resolvehelper(host, AF_INET, service, &addrDest);
    if (result != 0) {
        int lasterror = errno;
        std::cout << "error: " << lasterror;
        std::exit(1);
    }

    const int N = 512;
    char msg[N];
    const int printEvery = 10 * rate;

    char buff[100];
    time_t now;
    unsigned long long val;


    while(true) {
        limiter->aquire();
        val = counter.fetch_add(1);
        snprintf(msg, N, "<190>Mar 11 08:35:17 fw01 %llu: Mar 11 08:35:16.844 CST: %%SEC-6-IPACCESSLOGP: list in110 denied tcp 10.99.99.1(63923) -> 10.98.98.1(1521), 1 packet", val);
        result = sendto(sock, msg, strlen(msg), 0, (sockaddr *) &addrDest, sizeof(addrDest));
        if (val % printEvery == 0) {
            now = time(0);
            strftime (buff, 100, "%Y-%m-%d %H:%M:%S.000", localtime (&now));
            printf ("%s: Sent %llu Syslog messages\n", buff, val);
        }
    }
}

oid             objid_sysuptime[] = { 1, 3, 6, 1, 2, 1, 1, 3, 0 };
oid              trap_oid[] =       { 1, 3, 6, 1, 6, 3, 1, 1, 4, 1, 0 };
oid             objid_id[] = { 1, 3, 6, 1, 6, 3, 1, 1, 5, 1 };

void sendSnmpTraps(const char* host, const int port, const int rate) {
    netsnmp_session session, *ss;
    netsnmp_pdu    *pdu, *response;
    char *trap = NULL;

    char peername[512];
    snprintf(peername, 512, "%s:%d", host, port);
    char comm[] = "public";
    snmp_sess_init( &session );
    session.version = SNMP_VERSION_2c;
    session.community = (u_char*)comm;
    session.community_len = strlen((const char*)session.community);
    session.peername = (char*)(peername);
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


void daemonize() {
    FILE *fp= NULL;
    pid_t process_id = 0;
    pid_t sid = 0;
    // Create child process
    process_id = fork();
    // Indication of fork() failure
    if (process_id < 0)
    {
        printf("fork failed!\n");
        // Return failure in exit status
        exit(1);
    }
    // PARENT PROCESS. Need to kill it.
    if (process_id > 0)
    {
        printf("process_id of child process %d \n", process_id);
        // return success in exit status
        exit(0);
    }
    //unmask the file mode
    //umask(0);
    //set new session
    sid = setsid();
    if(sid < 0)
    {
        // Return failure
        exit(1);
    }
    // Change the current working directory to tmp.
    chdir("/tmp");
    // Close stdin. stdout and stderr
    close(STDIN_FILENO);
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
}

int main(int argc, char **argv) {
    const char* DEFAULT_HOST = "127.0.0.1";
    const int DEFAULT_TRAP_PORT = 1162;
    const int DEFAULT_SYSLOG_PORT = 1514;
    const int DEFAULT_RATE = 10000;

    char host[512];
    int port = 0;
    int rate = DEFAULT_RATE;
    char traps = 0;
    char daemon = 0;
    int num_threads = 1;

    strcpy(host, DEFAULT_HOST);
    int c;
    while ((c = getopt (argc, argv, "dh:p:r:tx:")) != -1) {
        switch (c)
        {
            case 'd':
                daemon = 1;
                break;
            case 'h':
                strncpy(host, optarg, sizeof(host));
                break;
            case 'p':
                port = atoi(optarg);
                break;
            case 'r':
                rate = atoi(optarg);
                break;
            case 'x':
                num_threads = atoi(optarg);
                break;
            case 't':
                traps = 1;
                break;
            default:
                printf("\nUsage: udpgen [-d] [-h host] [-p port] [-r rate] [-x threads] [-t]\n\n");
                exit(1);
        }
    }

    if (daemon) {
        daemonize();
    }

    if (traps) {
        if (port < 1) {
            port = DEFAULT_TRAP_PORT;
        }
        printf("Sending SNMPv2 traps to %s:%d at target rate of %d traps per second\n", host, port, rate);
        sendSnmpTraps(host, port, rate);
    } else {
        if (port < 1) {
            port = DEFAULT_SYSLOG_PORT;
        }
        printf("Sending syslog messages to %s:%d at target rate of %d message per seconds across %d threads \n", host, port, rate, num_threads);

        RateLimiterInterface* limiter = new RateLimiter();
        limiter->set_rate(rate);

        std::thread t[num_threads];
        for (int i = 0; i < num_threads; ++i) {
            t[i] = std::thread(sendSyslogMessages, host, port, rate, limiter);
        }

        for (int i = 0; i < num_threads; ++i) {
            t[i].join();
        }

        free(limiter);
    }

    return 0;
}
