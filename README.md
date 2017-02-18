# OpenNMS Events Stress Tools

This repository contains a collection of utility and scripts for load testing OpenNMS' event bus and helping to isolate event persistence bottlenecks.

## pgbench

The scripts provided in the `pgbench` directory will help in using `pgbench`, which should be provided by your local `PostgreSQL` install, to establish a baseline of raw database insert performance.

## jdbc-events

The tool provided in the `jdbc-events` directory will help establish a baseline of raw database insert performance from a minimal Java application, using the same libraries as we use in `OpenNMS`.

## udpgen

The tool provided in the `udpgen` directory will generate a stream of either Syslog or SNMPv2 Trap messages via UDP.

These can be sent directly to OpenNMS, or to a Minion to test the performance of the full stack.

## udplistener

A series of fat .jars used to evaluate the rate at which different strategies can handle processing UDP packets.

