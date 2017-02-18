package org.opennms.udplistener.camelnetty39;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.concurrent.TimeUnit;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.camel.builder.RouteBuilder;
import org.apache.camel.component.netty.NettyComponent;
import org.apache.camel.component.netty.NettyConstants;
import org.apache.camel.impl.DefaultCamelContext;
import org.apache.camel.impl.DefaultManagementNameStrategy;
import org.apache.camel.impl.SimpleRegistry;
import org.jboss.netty.buffer.ChannelBuffer;
import org.opennms.udplistener.AbstractUDPListener;
import org.opennms.udplistener.UDPConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CamelNetty39Listener extends AbstractUDPListener {

    private static final Logger LOG = LoggerFactory.getLogger(CamelNetty39Listener.class);

    private static final int SOCKET_TIMEOUT = 500;

    private DefaultCamelContext m_camel;

    @Override
    public String getShortName() {
        return "camel-netty39";
    }

    @Override
    public void start() {
        SimpleRegistry registry = new SimpleRegistry();
        registry.put("dispatcher", this);

        //Adding netty component to camel inorder to resolve OSGi loading issues
        NettyComponent nettyComponent = new NettyComponent();
        m_camel = new DefaultCamelContext(registry);

        // Set the context name so that it shows up nicely in JMX
        //
        // @see org.apache.camel.management.DefaultManagementNamingStrategy
        //
        //m_camel.setManagementName("org.opennms.features.events.syslog.listener");
        m_camel.setName("syslogdListenerCamelNettyContext");
        m_camel.setManagementNameStrategy(new DefaultManagementNameStrategy(m_camel, "#name#", null));

        m_camel.addComponent("netty", nettyComponent);

        m_camel.getShutdownStrategy().setShutdownNowOnTimeout(true);
        m_camel.getShutdownStrategy().setTimeout(15);
        m_camel.getShutdownStrategy().setTimeUnit(TimeUnit.SECONDS);
        
        
        try {
            m_camel.addRoutes(new RouteBuilder() {
                @Override
                public void configure() throws Exception {
                    String from = String.format("netty:udp://0.0.0.0:%d?sync=false&allowDefaultCodec=false&receiveBufferSize=%d&connectTimeout=%d&synchronous=true&orderedThreadPoolExecutor=false",
                            getPort(),
                            Integer.MAX_VALUE,
                            SOCKET_TIMEOUT
                    );
                    from(from)
                    // Polled via JMX
                    .routeId("syslogListen")
                    .process(new Processor() {
                        public void process(Exchange exchange) throws Exception {
                            ChannelBuffer buffer = exchange.getIn().getBody(ChannelBuffer.class);
                            // NettyConstants.NETTY_REMOTE_ADDRESS is a SocketAddress type but because 
                            // we are listening on an InetAddress, it will always be of type InetAddressSocket
                            InetSocketAddress source = (InetSocketAddress)exchange.getIn().getHeader(NettyConstants.NETTY_REMOTE_ADDRESS); 
                            // Syslog Handler Implementation to receive message from syslog port and pass it on to handler
                            ByteBuffer byteBuffer = buffer.toByteBuffer();
                            UDPConnection connection = new UDPConnection(source, byteBuffer);
                            exchange.getIn().setBody(connection, UDPConnection.class);
                        }
                    })
                    .to("bean:dispatcher?method=dispatch");
                }
            });
            m_camel.start();
        } catch (Throwable e) {
            LOG.error("Could not configure Camel routes for syslog receiver", e);
        }
    }

}
