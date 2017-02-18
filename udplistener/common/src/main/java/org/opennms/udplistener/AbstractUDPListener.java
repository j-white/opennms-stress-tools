package org.opennms.udplistener;

import java.nio.ByteBuffer;

import com.codahale.metrics.Meter;
import com.codahale.metrics.MetricRegistry;

public abstract class AbstractUDPListener implements UDPListener {

    private Meter packets;

    private int port;

    public void setMetricRegistry(MetricRegistry metrics) {
        packets = metrics.meter("packets");
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public void dispatch(UDPConnection connection) {
        packets.mark();
    }

    public static ByteBuffer cloneByteBuffer(final ByteBuffer original) {
        // Create clone with same capacity as original.
        final ByteBuffer clone = (original.isDirect()) ?
            ByteBuffer.allocateDirect(original.capacity()) :
            ByteBuffer.allocate(original.capacity());

        // Create a read-only copy of the original.
        // This allows reading from the original without modifying it.
        final ByteBuffer readOnlyCopy = original.asReadOnlyBuffer();

        // Flip and read from the original.
        readOnlyCopy.flip();
        clone.put(readOnlyCopy);

        return clone;
    }
}
