package org.opennms.udplistener;

import java.util.Map;
import java.util.ServiceLoader;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;

import com.codahale.metrics.ConsoleReporter;
import com.codahale.metrics.MetricRegistry;

public class App {

    private static final ServiceLoader<UDPListener> s_udpListeners = ServiceLoader.load(UDPListener.class);

    @Option(name="--port",usage="Port")
    public int port = 1514;

    @Option(name="--listener",usage="Listener name")
    public String listener = null;

    public void run() throws Exception {
        MetricRegistry metrics = new MetricRegistry();

        ConsoleReporter reporter = ConsoleReporter.forRegistry(metrics)
            .convertRatesTo(TimeUnit.SECONDS)
            .convertDurationsTo(TimeUnit.MICROSECONDS)
            .build();
        reporter.start(5, TimeUnit.SECONDS);

        Map<String, UDPListener> listenersByName = new TreeMap<>();
        s_udpListeners.forEach(l -> listenersByName.put(l.getShortName(), l));
        if (listenersByName.size() < 1) {
            throw new RuntimeException("No listeners available.");
        }
        String listenerNames = listenersByName.keySet().stream()
                .collect(Collectors.joining(", "));

        UDPListener udpListener;
        if (listener == null) {
            if (listenersByName.size() > 1) {
                throw new RuntimeException(String.format("Many listeners available in this "
                        + "runtime. Choose one of '%s' using the --listener option.", listenerNames));
            } else {
                // Use the first
                udpListener = listenersByName.values().iterator().next();
            }
        } else {
            udpListener = listenersByName.get(listener);
            if (udpListener == null) {
                throw new RuntimeException(String.format("No listener named %s is available in this "
                        + "runtime. Choose one of '%s' using the --listener option.",
                        listener, listenerNames));
            }
        }

        System.err.printf("Starting %s listener on port %d\n", udpListener.getShortName(), port);
        udpListener.setMetricRegistry(metrics);
        udpListener.setPort(port);
        udpListener.start();

        // Wait until we're interrupted
        while(true) {
            try {
                Thread.sleep(200);
            } catch (InterruptedException e) {
                return;
            }
        }
    }

    public static void main( String[] args ) throws Exception {
        App app = new App();
        CmdLineParser parser = new CmdLineParser(app);
        try {
            parser.parseArgument(args);
            app.run();
        } catch (CmdLineException e) {
            // handling of wrong arguments
            System.err.println(e.getMessage());
            parser.printUsage(System.err);
        }
    }
}
