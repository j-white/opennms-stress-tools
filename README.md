# OpenNMS Stress Tools

This repository contains a collection of utilities and scripts for load testing OpenNMS' and associated components.

## pgbench

The scripts provided in the `pgbench` directory will help in using `pgbench`, which should be provided by your local `PostgreSQL` install, to establish a baseline of raw database (event) insert performance.

## jdbc-events

The tool provided in the `jdbc-events` directory will help establish a baseline of raw database insert performance from a minimal Java application, using the same libraries as we use in `OpenNMS`.

## udpgen

The tool provided in the `udpgen` directory will generate a stream of Syslog, SNMPv2 Traps or Netflow 5 messages via UDP.

These can be sent directly to OpenNMS, or to a Minion to test the performance of the full stack.

## udplistener

A series of fat .jars used to evaluate the rate at which different strategies can handle processing UDP packets.

## esstress

Indexes flows into Elasticsearch.

