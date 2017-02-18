package org.opennms.udplistener;

import com.codahale.metrics.MetricRegistry;

public interface UDPListener {

    String getShortName();

    void setMetricRegistry(MetricRegistry metrics);

    void setPort(int port);

    void start() throws Exception;

}
