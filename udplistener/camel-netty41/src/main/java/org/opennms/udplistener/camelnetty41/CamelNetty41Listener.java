package org.opennms.udplistener.camelnetty41;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.util.concurrent.TimeUnit;

import org.apache.camel.AsyncCallback;
import org.apache.camel.AsyncProcessor;
import org.apache.camel.Exchange;
import org.apache.camel.builder.RouteBuilder;
import org.apache.camel.component.netty4.NettyComponent;
import org.apache.camel.component.netty4.NettyConstants;
import org.apache.camel.impl.DefaultCamelContext;
import org.apache.camel.impl.DefaultManagementNameStrategy;
import org.apache.camel.impl.SimpleRegistry;
import org.opennms.udplistener.AbstractUDPListener;
import org.opennms.udplistener.UDPConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.netty.buffer.ByteBuf;

public class CamelNetty41Listener extends AbstractUDPListener {

    private static final Logger LOG = LoggerFactory.getLogger(CamelNetty41Listener.class);

    private static final int SOCKET_TIMEOUT = 500;

    private DefaultCamelContext m_camel;

    @Override
    public String getShortName() {
        return "camel-netty41";
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
                    String from = String.format("netty4:udp://0.0.0.0:%d?sync=false&allowDefaultCodec=false&receiveBufferSize=%d&connectTimeout=%d",
                            getPort(),
                        Integer.MAX_VALUE,
                        SOCKET_TIMEOUT
                    );
                    from(from)
                    // Polled via JMX
                    .routeId("syslogListen")
                    .process(new AsyncProcessor() {

                        private UDPConnection createSyslogConnection(Exchange exchange) {
                            ByteBuf buffer = exchange.getIn().getBody(ByteBuf.class);
                            // NettyConstants.NETTY_REMOTE_ADDRESS is a SocketAddress type but because 
                            // we are listening on an InetAddress, it will always be of type InetAddressSocket
                            InetSocketAddress source = (InetSocketAddress)exchange.getIn().getHeader(NettyConstants.NETTY_REMOTE_ADDRESS); 
                            ByteBuffer byteBuffer = buffer.copy().nioBuffer();
                            return new UDPConnection(source, byteBuffer);
                        }

                        @Override
                        public void process(Exchange exchange) throws Exception {
                            // Synchronously invoke the dispatcher
                            dispatch(createSyslogConnection(exchange));
                        }

                        @Override
                        public boolean process(Exchange exchange, AsyncCallback callback) {
                            dispatch(createSyslogConnection(exchange));
                            return false;
                        }
                    });
                }
            });
            m_camel.start();
        } catch (Throwable e) {
            LOG.error("Could not configure Camel routes for syslog receiver", e);
        }
    }

}
