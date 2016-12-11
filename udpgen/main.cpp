#include <iostream>
#include <cstdlib>
#include <thread>
#include <atomic>
#include <cstring>
#include <sys/socket.h>

#include "syslog_generator.hpp"
#include "trap_generator.hpp"

void daemonize();

int main(int argc, char **argv) {
    const int HOST_LEN = 512;
    char host[HOST_LEN];
    memset(host, 0, HOST_LEN);
    int port = -1;
    int rate = -1;
    char traps = 0;
    char daemon = 0;
    int num_threads = -1;

    int c;
    while ((c = getopt (argc, argv, "dh:p:r:t:x")) != -1) {
        switch (c)
        {
            case 'd':
                daemon = 1;
                break;
            case 'h':
                strncpy(host, optarg, HOST_LEN);
                break;
            case 'p':
                port = atoi(optarg);
                break;
            case 'r':
                rate = atoi(optarg);
                break;
            case 't':
                num_threads = atoi(optarg);
                break;
            case 'x':
                traps = 1;
                break;
            default:
                printf("\nUsage: udpgen [-x] [-d] [-h host] [-p port] [-r rate] [-t threads]\n\n");
                printf("  -x: Generate SNMP Traps instead of Syslog Messages\n");
                printf("  -d: Daemonize (default: false)\n");
                printf("  -h: Target host / IP address (default: 127.0.0.1)\n");
                printf("  -p: Target port (default: depends on mode)\n");
                printf("  -r: Rate - number of packets per second to generate (default: 10000)\n");
                printf("  -t: Number of threads used to generate packets (default: 1)\n\n");
                return 1;
        }
    }

    if (daemon) {
        daemonize();
    }

    std::unique_ptr<UDPGenerator> generator;
    if (traps) {
        generator = std::unique_ptr<UDPGenerator>(new TrapGenerator());
    } else {
        generator = std::unique_ptr<UDPGenerator>(new SyslogGenerator());
    }

    if (strlen(host) > 0) {
        generator->setHost(host);
    }
    if (port > 0) {
        generator->setPort(port);
    }
    if (rate > 0) {
        generator->setPacketsPerSecond(rate);
    }
    if (num_threads > 0) {
        generator->setNumThreads(num_threads);
    }

    printf("Sending %s to %s:%d at target rate of %.2f packets per seconds across %d thread(s).\n",
           generator->getPacketDescription(), generator->getHost(), generator->getPort(), generator->getPacketsPerSecond(),
           generator->getNumThreads());

    printf("\nThe number of packets sent should be printed every %d seconds.\n"
           "If the more than %d seconds elapses between the reports, the program is unable to generate packets at the requested rate.\n"
           "You can try consider increasing the number of threads.\n\n",
            generator->getReportInterval(), generator->getReportInterval());

    generator->start();
    char waitForKey;
    std::cout << "Type q + enter to exit..." << std::endl;
    std::cin >> waitForKey;
    generator->stop();
    return 0;
}

void daemonize() {
    FILE *fp= NULL;
    pid_t process_id = 0;
    pid_t sid = 0;
    // Create child process
    process_id = fork();
    // Indication of fork() failure
    if (process_id < 0) {
        printf("fork failed!\n");
        // Return failure in exit status
        std::exit(1);
    }
    // PARENT PROCESS. Need to kill it.
    if (process_id > 0) {
        printf("process_id of child process %d \n", process_id);
        // return success in exit status
        std::exit(0);
    }
    //unmask the file mode
    //umask(0);
    //set new session
    sid = setsid();
    if (sid < 0) {
        // Return failure
        std::exit(1);
    }
    // Change the current working directory to tmp.
    chdir("/tmp");
    // Close stdin. stdout and stderr
    close(STDIN_FILENO);
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
}
