package org.opennms.udplistener.netty40;

import java.util.ArrayList;
import java.util.List;

import org.opennms.udplistener.AbstractUDPListener;
import org.opennms.udplistener.UDPConnection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.netty.bootstrap.Bootstrap;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelHandler.Sharable;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.epoll.EpollChannelOption;
import io.netty.channel.epoll.EpollDatagramChannel;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.socket.DatagramPacket;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.internal.logging.InternalLoggerFactory;
import io.netty.util.internal.logging.Slf4JLoggerFactory;

public class Netty40Listener extends AbstractUDPListener {

    private static final Logger LOG = LoggerFactory.getLogger(Netty40Listener.class);

    @Override
    public String getShortName() {
        return "netty40";
    }

    @Sharable
    private class DataServerHandler extends ChannelInboundHandlerAdapter {
        @Override
        public void channelRead(ChannelHandlerContext ctx, Object msg) {
            try {
                DatagramPacket packet = ((DatagramPacket) msg);
                ByteBuf byteBuf = packet.content().copy();
                UDPConnection connection = new UDPConnection(packet.sender(), byteBuf.nioBuffer());
                byteBuf.release();
                dispatch(connection);
            } finally {
                ReferenceCountUtil.release(msg);
            }
        }
    }

    @Override
    public void start() throws Exception {
        InternalLoggerFactory.setDefaultFactory(Slf4JLoggerFactory.INSTANCE);
        
        final int workerThreads = Math.max(1, Runtime.getRuntime().availableProcessors() - 1);
        final DataServerHandler handler = new DataServerHandler();

        EventLoopGroup group = new EpollEventLoopGroup(workerThreads);
        Bootstrap b = new Bootstrap()
                .group(group)
                .channel(EpollDatagramChannel.class)
                .option(EpollChannelOption.SO_REUSEPORT, true)
                .option(EpollChannelOption.SO_RCVBUF, Integer.MAX_VALUE)
                .handler(handler);

        List<ChannelFuture> futures = new ArrayList<>(workerThreads);
        for (int i = 0; i < workerThreads; ++i) {
            LOG.info("Binding thread {}.", i);
            futures.add(b.bind(getPort()).await());
            LOG.info("Done binding thread {}.", i);
        }
    }

}
