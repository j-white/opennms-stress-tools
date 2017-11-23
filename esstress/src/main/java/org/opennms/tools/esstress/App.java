package org.opennms.tools.esstress;

import com.codahale.metrics.ConsoleReporter;
import com.codahale.metrics.Counter;
import com.codahale.metrics.Meter;
import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.Timer;
import io.searchbox.client.JestClient;
import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;
import org.kohsuke.args4j.Option;
import org.opennms.netmgt.flows.api.FlowException;
import org.opennms.netmgt.flows.api.FlowRepository;
import org.opennms.netmgt.flows.api.IndexStrategy;
import org.opennms.netmgt.flows.api.NetflowDocument;
import org.opennms.plugins.elasticsearch.rest.RestClientFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

public class App {
    private static final Logger LOG = LoggerFactory.getLogger(App.class);

    @Option(name="-url", usage="Set the URL")
    public String url;

    @Option(name="-username", usage="Username")
    public String username;

    @Option(name="-password", usage="Password")
    public String password;

    @Option(name="-batch", usage="Batch size")
    public Integer batchSize = 300;

    @Option(name="-threads", usage="Thread count")
    public Integer threadCount = Runtime.getRuntime().availableProcessors() *2;

    final MetricRegistry metrics = new MetricRegistry();
    final Meter insertMeter = metrics.meter("inserts");
    final Timer insertTimer = metrics.timer("timer");

    public static void main(String[] args) {
        final App app = new App();
        final CmdLineParser parser = new CmdLineParser(app);
        try {
            parser.parseArgument(args);
            app.run();
        } catch (CmdLineException e) {
            // handling of wrong arguments
            System.err.println(e.getMessage());
            parser.printUsage(System.err);
        } catch (MalformedURLException e) {
            System.out.printf("Invalid URL: {}", app.url);
        }
    }

    private void run() throws MalformedURLException {
        // Ready!
        final RestClientFactory clientFactory = new RestClientFactory(url, username, password);
        clientFactory.setConnTimeout(10*1000);
        clientFactory.setDefaultMaxTotalConnectionPerRoute(threadCount);
        clientFactory.setMaxTotalConnection(threadCount*2);

        final JestClient client = clientFactory.createClient();
        final ElasticFlowRepository flowRepository = new ElasticFlowRepository(client, IndexStrategy.HOURLY);
        final InitializingFlowRepository initializingFlowRepository = new InitializingFlowRepository(flowRepository, client);

        // Set!
        final ConsoleReporter reporter = ConsoleReporter.forRegistry(metrics)
                .convertRatesTo(TimeUnit.SECONDS)
                .convertDurationsTo(TimeUnit.MILLISECONDS)
                .build();
        reporter.start(10, TimeUnit.SECONDS);

        // Start!
        final ExecutorService executor = Executors.newFixedThreadPool(threadCount);
        final AtomicBoolean stop = new AtomicBoolean();
        for (int i = 0; i < threadCount; i++) {
            executor.submit(new Worker(initializingFlowRepository, stop));
        }

        // Wait until interuppted
        while(true) {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                stop.set(true);
                break;
            }
        }

        // Shutdown
        try {
            executor.awaitTermination(2, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // One last report
        reporter.report();
        reporter.stop();
    }

    private class Worker implements Runnable {
        private AtomicBoolean stop;
        private FlowRepository flowRepository;

        public Worker(FlowRepository flowRepository, AtomicBoolean stop) {
            this.flowRepository = Objects.requireNonNull(flowRepository);
            this.stop = Objects.requireNonNull(stop);
        }

        @Override
        public void run() {
            while(!stop.get()) {
                try {
                    final List<NetflowDocument> docs = createNextBatch();
                    try (final Timer.Context ctx = insertTimer.time()) {
                        flowRepository.save(docs);
                    }
                    insertMeter.mark(docs.size());
                } catch (FlowException e) {
                    LOG.error("Insert failed.", e);
                }
            }
        }
    }

    private List<NetflowDocument> createNextBatch() {
        final Long now = System.currentTimeMillis();
        final List<NetflowDocument> docs = new ArrayList<>(batchSize);
        for (int i = 0; i < batchSize; i++) {
            final NetflowDocument doc = new NetflowDocument();
            doc.setVersion(5);
            doc.setFlowSequenceNumber(1);
            doc.setEngineType(1);
            doc.setSamplingInterval(1);
            doc.setFlowRecords(1);
            doc.setSysUptime(now);
            doc.setTimestamp(now);
            doc.setIpv4SourceAddress("1.1.1.1");
            doc.setIpv4DestAddress("2.2.2.2");
            doc.setIpv4NextHopAddress("3.3.3.3");
            doc.setInputSnmpInterfaceIndex(1);
            doc.setOutputSnmpInterfaceIndex(1);
            doc.setInPackets(1);
            doc.setInBytes(1);
            doc.setFirst(1);
            doc.setLast(1);
            doc.setSourcePort(1);
            doc.setDestPort(1);
            doc.setTcpFlags(1);
            doc.setIpProtocol(1);
            doc.setTos(1);
            doc.setSourceAutonomousSystemNumber(1);
            doc.setDestAutonomousSystemNumber(1);
            doc.setSourceMask(1);
            doc.setDestMask(1);
            docs.add(doc);
        }
        return docs;
    }
}
