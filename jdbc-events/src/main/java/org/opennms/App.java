package org.opennms;

import java.io.IOException;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import javax.sql.DataSource;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import com.codahale.metrics.ConsoleReporter;
import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.Timer;
import com.codahale.metrics.Timer.Context;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

public class App {
    
    private static final MetricRegistry metrics = new MetricRegistry();

    private static final Timer eventsTimer = metrics.timer("events");
    
    @Option(name="-t",usage="use <t> threads")
    private int numThreads = 1;

    public static class EventWriter implements Runnable {
        private final Date then = new Date(0);
        private final DataSource ds;

        public EventWriter(DataSource ds) {
            this.ds = Objects.requireNonNull(ds);
        }

        @Override
        public void run() {
            while(true) {
                try(Context ctx = eventsTimer.time()) {
                    try (Connection c = ds.getConnection()) {
                        try (PreparedStatement insStmt = c.prepareStatement(EventdConstants.SQL_DB_INS_EVENT)) {
                            insStmt.setString(1, "uei.opennms.org/alarms/trigger");
                            insStmt.setNull(2, Types.INTEGER);
                            insStmt.setDate(3, then);
                            insStmt.setNull(4, Types.INTEGER);
                            insStmt.setNull(5, Types.INTEGER);
                            insStmt.setString(6, "00000000-0000-0000-0000-000000000000");
                            insStmt.setNull(7, Types.INTEGER);
                            insStmt.setNull(8, Types.INTEGER);
                            insStmt.setNull(9, Types.INTEGER);
                            insStmt.setNull(10, Types.INTEGER);
                            insStmt.setDate(11, then);
                            insStmt.setString(12, "A problem has been triggered.");
                            insStmt.setNull(13, Types.INTEGER);
                            insStmt.setString(14, "A problem has been triggered on //.");
                            insStmt.setString(15, "Y");
                            insStmt.setString(16, "Y");
                            insStmt.setInt(17, 4);
                            insStmt.setNull(18, Types.INTEGER);
                            insStmt.setNull(19, Types.INTEGER);
                            insStmt.setNull(20, Types.INTEGER);
                            insStmt.setNull(21, Types.INTEGER);
                            insStmt.setNull(22, Types.INTEGER);
                            insStmt.setNull(23, Types.INTEGER);
                            insStmt.setNull(24, Types.INTEGER);
                            insStmt.setNull(25, Types.INTEGER);
                            insStmt.setNull(26, Types.INTEGER);
                            insStmt.setNull(27, Types.INTEGER);
                            insStmt.setNull(28, Types.INTEGER);
                            insStmt.setNull(29, Types.INTEGER);
                            insStmt.setNull(30, Types.INTEGER);
                            insStmt.setNull(31, Types.DATE);
                            insStmt.setString(32, "stress");
                            insStmt.setNull(33, Types.INTEGER);

                            insStmt.executeUpdate();
                            insStmt.getGeneratedKeys();
                        }
                        
                    } catch (SQLException e) {
                        e.printStackTrace();
                        break;
                    }
                }
            }
        }
    }

    public static void main(String[] args) throws IOException {
        new App().doMain(args);
    }
    
    public void doMain(String[] args) throws IOException {
        CmdLineParser parser = new CmdLineParser(this);
        try {
            parser.parseArgument(args);
        } catch( CmdLineException e ) {
            System.err.println(e.getMessage());
            // print the list of available options
            parser.printUsage(System.err);
            System.err.println();
        }

        // Setup the reporter
        ConsoleReporter reporter = ConsoleReporter.forRegistry(metrics)
                .convertRatesTo(TimeUnit.SECONDS)
                .convertDurationsTo(TimeUnit.MILLISECONDS)
                .build();
        reporter.start(15, TimeUnit.SECONDS);

        HikariConfig config = new HikariConfig();
        config.setJdbcUrl("jdbc:postgresql://localhost:5432/opennms");
        config.setUsername("opennms");
        config.setPassword("opennms");
        config.addDataSourceProperty("cachePrepStmts", "true");
        config.addDataSourceProperty("prepStmtCacheSize", "250");
        config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

        try(HikariDataSource ds = new HikariDataSource(config)) {
            ExecutorService executor = Executors.newFixedThreadPool(numThreads);
            for (int k = 0; k < numThreads; k++) {
                executor.submit(new EventWriter(ds));
            }

            while (true) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    break;
                }
            }
            
            executor.shutdownNow();
            try {
                executor.awaitTermination(1, TimeUnit.MINUTES);
            } catch (InterruptedException e) {
                // pass
            }
        }
    }
}
