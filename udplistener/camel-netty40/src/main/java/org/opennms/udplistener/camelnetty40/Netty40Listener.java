package org.opennms.udplistener.camelnetty40;

import java.net.InetSocketAddress;

import org.opennms.udplistener.AbstractUDPListener;
import org.opennms.udplistener.UDPConnection;

import io.netty.bootstrap.Bootstrap;
import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.DatagramPacket;
import io.netty.channel.socket.nio.NioDatagramChannel;
import io.netty.util.ReferenceCountUtil;
import io.netty.util.internal.logging.InternalLoggerFactory;
import io.netty.util.internal.logging.Slf4JLoggerFactory;

public class Netty40Listener extends AbstractUDPListener {

    @Override
    public String getShortName() {
        return "netty40";
    }

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
    public void start() {
        InternalLoggerFactory.setDefaultFactory(new Slf4JLoggerFactory());

        final DataServerHandler handler = new DataServerHandler();
        EventLoopGroup bossGroup = new NioEventLoopGroup();

        Bootstrap b = new Bootstrap();
        b.group(bossGroup);
        b.channel(NioDatagramChannel.class);
        b.handler(new ChannelInitializer<NioDatagramChannel>() {
            @Override
            protected void initChannel(NioDatagramChannel ch) throws Exception {
                ch.pipeline().addLast(handler);
            }
        })
        .option(ChannelOption.SO_RCVBUF, Integer.MAX_VALUE)
        .bind(new InetSocketAddress(getPort()));
    }

}
