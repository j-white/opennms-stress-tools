# udpgen

## Overview

Yet another tool used to generate and send traps and syslogs messages.

## Syslogs

Syslog message:

```
<190>Mar 11 08:35:17 fw01 30228451: Mar 11 08:35:16.844 CST: %SEC-6-IPACCESSLOGP: list in110 denied tcp 10.99.99.1(63923) -> 10.98.98.1(1521), 1 packet
```

On OpenNMS:

```
ssh -p 8101 admin@localhost
opennms> features:install opennms-syslogd-handler-default
```

## Traps

```
ssh -p 8101 admin@localhost
opennms> features:install opennms-trapd-handler-default
```

