package org.opennms.udplistener.camelnetty39;

import java.net.InetSocketAddress;
import java.nio.ByteBuffer;

import org.jboss.netty.bootstrap.ConnectionlessBootstrap;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.jboss.netty.channel.socket.nio.NioDatagramChannelFactory;
import org.opennms.udplistener.AbstractUDPListener;
import org.opennms.udplistener.UDPConnection;

public class Netty39Listener extends AbstractUDPListener {

    @Override
    public String getShortName() {
        return "netty39";
    }

    private class DataServerHandler extends SimpleChannelHandler {
        @Override
        public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) {
            ChannelBuffer buffer = (ChannelBuffer) e.getMessage();
            InetSocketAddress source = (InetSocketAddress) e.getRemoteAddress();
            ByteBuffer byteBuffer = cloneByteBuffer(buffer.toByteBuffer());
            UDPConnection connection = new UDPConnection(source, byteBuffer);
            dispatch(connection);
        }
    }

    @Override
    public void start() {
        final DataServerHandler handler = new DataServerHandler();
        NioDatagramChannelFactory factory = new NioDatagramChannelFactory();
        ConnectionlessBootstrap bootstrap = new ConnectionlessBootstrap(factory);
        bootstrap.setPipelineFactory(new ChannelPipelineFactory() {
            @Override
            public ChannelPipeline getPipeline() throws Exception {
                return Channels.pipeline(handler);
            }
        });
        bootstrap.setOption("receiveBufferSize", Integer.MAX_VALUE);
        bootstrap.bind(new InetSocketAddress(getPort()));
    }

}
