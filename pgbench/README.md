# pgbench

## Requirements

* A running PostgreSQL Server

## Usage

```sh
./stress.sh
```

## Database schema

The database schema in `init-opennms.sql` was dumped from Horizon 19.0.0-SNAPSHOT installation using the following command:

```sh
pg_dump -U opennms -C opennms
```

We've also added the following SQL statement to the schema in order to help simplify the `insert-event.sql` script:

```sql
ALTER TABLE events ALTER COLUMN eventid SET DEFAULT nextval('eventsnxtid');
```
