# udpgen

## Overview

Yet another tool used to generate and send traps and syslogs messages.

## Requirements

* cmake
* netsnmp-devel

## Building

```sh
mkdir build
cd build
cmake ..
make
```

## Running

```sh
./udpgen -r 1000
```

## OpenNMS and Minion

When pointing to a Minion, make sure that the following handler features are installed on OpenNMS (these are not currently installed by default)

```
ssh -p 8101 admin@localhost
opennms> features:install opennms-syslogd-handler-default opennms-trapd-handler-default
```
