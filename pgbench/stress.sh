#!/bin/sh
echo "Creating the database and schema..."
dropdb -U postgres opennms_stress > /dev/null 2>&1
createdb -U postgres opennms_stress
# Dump was created with 'pg_dump -U opennms -C opennms'
# and extended with:
#  ALTER TABLE events ALTER COLUMN eventid SET DEFAULT nextval('eventsnxtid');
psql -U postgres -d opennms_stress -f init-opennms.sql > /dev/null 2>&1
echo "Done!"

# -s: scale factor in output
# -f: transaction script
# -n: no vacuum before tests
# -c: number of database clients
# -t: number of transactions each client runs
# -l: enable per transaction logging
# -r: display report of average per statement latencies
echo "Running pgbench..."
pgbench -U postgres -s 1000 -f insert-event.sql -n -c 5 -t 25000 -l -r opennms_stress

# Format of the log output is:
#   client_id transaction_no time file_no time_epoch time_us
# where:
#   time: transaction time in microseconds
#   time_epoch: unix timestamp
#   time_us: offset in microseconds
