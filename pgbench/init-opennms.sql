--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.4
-- Dumped by pg_dump version 9.5.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: daily_series; Type: TYPE; Schema: public; Owner: opennms
--

CREATE TYPE daily_series AS (
	ds timestamp without time zone,
	de timestamp without time zone,
	dow integer
);


ALTER TYPE daily_series OWNER TO opennms;

--
-- Name: check_hex_range(integer, text); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION check_hex_range(i_octet integer, i_rule text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
    c_temp text;
    c_r1 integer;
    c_r2 integer;
begin

    c_temp := split_part(i_rule, '-', 1);
    while length(c_temp) < 4 loop
        c_temp := '0' || c_temp;
    end loop;
    c_r1 := cast(cast('x' || cast(c_temp as text) as bit(16)) as integer);
    c_temp := split_part(i_rule, '-', 2);
    while length(c_temp) < 4 loop
        c_temp := '0' || c_temp;
    end loop;
    c_r2 := cast(cast('x' || cast(c_temp as text) as bit(16)) as integer);
    if i_octet between c_r1 and c_r2 then
        return 't';
    end if;
    return 'f';
end;
$$;


ALTER FUNCTION public.check_hex_range(i_octet integer, i_rule text) OWNER TO opennms;

--
-- Name: check_hex_rule(integer, text); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION check_hex_rule(i_octet integer, i_rule text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
    c_element text;
    c_work  text;
begin
    if i_rule = '*' then   -- * matches anything!
        return 't';
    end if;

    c_work := i_rule;
    while c_work <> '' loop
        -- raise notice 'c_work = %',c_work;
        if c_work ~ ',' then
            c_element := substr(c_work, 0, strpos(c_work, ','));
            c_work := substr(c_work, strpos(c_work, ',')+1);
        else
            c_element := c_work;
            c_work := '';
        end if;

        if c_element ~ '-' then
            if check_hex_range(i_octet, c_element) then
                return 't';
            end if;
        else
            while length(c_element) < 4 loop
                c_element := '0' || c_element;
            end loop;
            if i_octet = cast(cast('x' || cast(c_element as text) as bit(16)) as integer) then
                return 't';
            end if;
        end if;
    end loop;
    return 'f';
end;
$$;


ALTER FUNCTION public.check_hex_rule(i_octet integer, i_rule text) OWNER TO opennms;

--
-- Name: check_range(integer, text); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION check_range(i_octet integer, i_rule text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
    c_r1 integer;
    c_r2 integer;
begin

    c_r1 := to_number(split_part(i_rule, '-', 1), '999');
    c_r2 := to_number(split_part(i_rule, '-', 2), '999');
    if i_octet between c_r1 and c_r2 then
        return 't';
    end if;
    return 'f';
end;
$$;


ALTER FUNCTION public.check_range(i_octet integer, i_rule text) OWNER TO opennms;

--
-- Name: check_rule(integer, text); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION check_rule(i_octet integer, i_rule text) RETURNS boolean
    LANGUAGE plpgsql
    AS $$
declare
    c_element text;
    c_work	text;
begin
    if i_rule = '*' then   -- * matches anything!
        return 't';
    end if;

    c_work := i_rule;
    while c_work <> '' loop
        -- raise notice 'c_work = %',c_work;
        if c_work ~ ',' then
            c_element := substr(c_work, 0, strpos(c_work, ','));
            c_work := substr(c_work, strpos(c_work, ',')+1);
        else
            c_element := c_work;
            c_work := '';
        end if;

        if c_element ~ '-' then
            if check_range(i_octet, c_element) then
                return 't';
            end if;
        else
            if i_octet = to_number(c_element, '99999') then
                return 't';
            end if;
        end if;
    end loop;
    return 'f';
end;
$$;


ALTER FUNCTION public.check_rule(i_octet integer, i_rule text) OWNER TO opennms;

--
-- Name: drop_trigger_if_exists(character varying, character varying); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION drop_trigger_if_exists(character varying, character varying) RETURNS void
    LANGUAGE plpgsql
    AS $_$
        DECLARE
                in_triggername ALIAS FOR $1;
                in_tablename ALIAS FOR $2;

        BEGIN
                PERFORM tgname FROM pg_catalog.pg_trigger, pg_catalog.pg_class WHERE pg_catalog.pg_class.oid = pg_catalog.pg_trigger.tgrelid AND tgname = in_triggername AND pg_catalog.pg_class.relname = in_tablename;
                IF FOUND THEN
                        EXECUTE 'DROP TRIGGER ' || in_triggername || ' ON ' || in_tablename;
                END IF;
                RETURN;
        END;
$_$;


ALTER FUNCTION public.drop_trigger_if_exists(character varying, character varying) OWNER TO opennms;

--
-- Name: generate_daily_series(timestamp without time zone, integer); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION generate_daily_series(start timestamp without time zone, days integer) RETURNS SETOF daily_series
    LANGUAGE sql
    AS $_$ select $1 + CAST(n || ' days' as interval) as ds, $1 + CAST((n+1)||' days' as interval) as de, n as dow from generate_series(0,$2) as n $_$;


ALTER FUNCTION public.generate_daily_series(start timestamp without time zone, days integer) OWNER TO opennms;

--
-- Name: getmanagedoutageforintfinwindow(integer, character varying, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getmanagedoutageforintfinwindow(integer, character varying, timestamp without time zone, timestamp without time zone) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        nid ALIAS FOR $1;
        ipid ALIAS FOR $2;
        xtime ALIAS FOR $3;
        ytime ALIAS FOR $4;
        downtime float8 := 0.0;
        orec RECORD;
   BEGIN
        FOR orec IN SELECT DISTINCT ifservices.id AS ifServiceId 
                FROM ifservices, ipinterface, node 
                WHERE ifservices.ipInterfaceId = ipInterface.id 
                        AND ipinterface.nodeid = node.nodeid 
                        AND ifservices.status = 'A' 
                        AND ipinterface.ismanaged = 'M' 
                        AND ipinterface.ipaddr = ipid 
                        AND node.nodeid = nid 
                        AND node.nodetype = 'A'
        LOOP
                BEGIN
                        downtime := downtime + getOutageTimeInWindow( orec.ifServiceId, xtime, ytime);
                END;
        END LOOP;
        RETURN downtime;
   END;
$_$;


ALTER FUNCTION public.getmanagedoutageforintfinwindow(integer, character varying, timestamp without time zone, timestamp without time zone) OWNER TO opennms;

--
-- Name: getmanagedoutagefornodeinwindow(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getmanagedoutagefornodeinwindow(integer, timestamp without time zone, timestamp without time zone) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        nid ALIAS FOR $1;
        xtime ALIAS FOR $2;
        ytime ALIAS FOR $3;
        downtime float8 := 0.0;
        orec RECORD;
   BEGIN
        FOR orec IN SELECT DISTINCT ifservices.id AS ifServiceId 
                FROM ifservices, ipinterface, node 
                WHERE ifservices.ipInterfaceId = ipInterface.id 
                        AND ipinterface.nodeid = node.nodeid 
                        AND ifservices.status = 'A' 
                        AND ipinterface.ismanaged = 'M' 
                        AND node.nodeid = nid 
                        AND node.nodetype = 'A'
        LOOP
                BEGIN
                        downtime := downtime + getOutageTimeInWindow( orec.ifServiceId, xtime, ytime);
                END;
        END LOOP;
        RETURN downtime;
   END;
$_$;


ALTER FUNCTION public.getmanagedoutagefornodeinwindow(integer, timestamp without time zone, timestamp without time zone) OWNER TO opennms;

--
-- Name: getmanagedservicecountforintf(integer, character varying); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getmanagedservicecountforintf(integer, character varying) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        nid ALIAS FOR $1;
        ipid ALIAS FOR $2;
        orec RECORD;
        counter float8;
   BEGIN
        counter = 0;
        FOR orec IN SELECT DISTINCT ifservices.ipInterfaceId, ifservices.serviceid
                FROM ifservices, ipinterface, node 
                WHERE ifservices.ipInterfaceId = ipInterface.id 
                        AND ipinterface.nodeid = node.nodeid 
                        AND ifservices.status = 'A' 
                        AND ipinterface.ismanaged = 'M' 
                        AND ipinterface.ipaddr = ipid 
                        AND node.nodeid = nid 
                        AND node.nodetype = 'A'
        LOOP
                BEGIN
                        counter := counter + 1;
                END;
        END LOOP;
        RETURN counter;
   END;
$_$;


ALTER FUNCTION public.getmanagedservicecountforintf(integer, character varying) OWNER TO opennms;

--
-- Name: getmanagedservicecountfornode(integer); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getmanagedservicecountfornode(integer) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        nid ALIAS FOR $1;
        orec RECORD;
        counter float8;
   BEGIN
        counter = 0;
        FOR orec IN SELECT DISTINCT ifservices.ipInterfaceId, ifservices.serviceid
                FROM ifservices, ipinterface, node 
                WHERE ifservices.ipInterfaceId = ipInterface.id 
                        AND ipinterface.nodeid = node.nodeid 
                        AND ifservices.status = 'A' 
                        AND ipinterface.ismanaged = 'M' 
                        AND node.nodeid = nid 
                        AND node.nodetype = 'A'
        LOOP
                BEGIN
                         counter := counter + 1;
                END;
        END LOOP;
        RETURN counter;
   END;
$_$;


ALTER FUNCTION public.getmanagedservicecountfornode(integer) OWNER TO opennms;

--
-- Name: getmanagepercentavailintfwindow(integer, character varying, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getmanagepercentavailintfwindow(integer, character varying, timestamp without time zone, timestamp without time zone) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        nid ALIAS FOR $1;
        ipid ALIAS FOR $2;
        xtime ALIAS FOR $3;
        ytime ALIAS FOR $4;
        downtime float8 := 0.0;
        count integer := 0;
        rollingWindow float := 0;
        totalServiceTime float := 0;
   BEGIN
        IF xtime < ytime THEN
                rollingWindow := EXTRACT (EPOCH FROM (ytime - xtime));
                downtime := getManagedOutageForIntfInWindow(nid, ipid, ytime, xtime)/1000;
        ELSE
                rollingWindow := EXTRACT (EPOCH FROM (xtime - ytime));
                downtime := getManagedOutageForIntfInWindow(nid, ipid, xtime, ytime)/1000;
        END IF;
        count := getManagedServiceCountForIntf(nid, ipid);
        totalServiceTime := count * rollingWindow;

        IF totalServiceTime > 0 THEN
                RETURN  100 * (1 - (downtime / totalServiceTime));
        ELSE
                IF totalServiceTime = 0 THEN
                        RETURN 100;
                ELSE
                        RETURN -1;
                END IF;
        END IF;    
   END;
$_$;


ALTER FUNCTION public.getmanagepercentavailintfwindow(integer, character varying, timestamp without time zone, timestamp without time zone) OWNER TO opennms;

--
-- Name: getmanagepercentavailnodewindow(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getmanagepercentavailnodewindow(integer, timestamp without time zone, timestamp without time zone) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        nid ALIAS FOR $1;
        xtime ALIAS FOR $2;
        ytime ALIAS FOR $3;
        downtime float8 := 0.0;
        count integer := 0;
        rollingWindow float := 0;
        totalServiceTime float := 0;
   BEGIN
        IF xtime < ytime THEN
                rollingWindow := EXTRACT (EPOCH FROM (ytime - xtime));
                downtime := getManagedOutageForNodeInWindow(nid, ytime, xtime)/1000;
        ELSE
                rollingWindow := EXTRACT (EPOCH FROM (xtime - ytime));
                downtime := getManagedOutageForNodeInWindow(nid, xtime, ytime)/1000;
        END IF;
        count := getManagedServiceCountForNode(nid);
        totalServiceTime := count * rollingWindow;

        IF totalServiceTime > 0 THEN
                RETURN 100 * (1 - (downtime / totalServiceTime));
        ELSE
                IF totalServiceTime = 0 THEN
                        RETURN 100;
                ELSE
                        RETURN -1;
                END IF;
        END IF;
   END;
$_$;


ALTER FUNCTION public.getmanagepercentavailnodewindow(integer, timestamp without time zone, timestamp without time zone) OWNER TO opennms;

--
-- Name: getoutagetimeinwindow(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getoutagetimeinwindow(integer, timestamp without time zone, timestamp without time zone) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        ifsrvid ALIAS FOR $1;
        xtime ALIAS FOR $2;
        ytime ALIAS FOR $3;
        orec RECORD;
        lostTime timestamp without time zone;
        gainTime timestamp without time zone;
        downtime float8;
        zero CONSTANT float8 := 0.0;
        epochTime CONSTANT timestamp without time zone := to_timestamp('01 Jan 1970 00:00:00', 'DD Mon YYYY HH24:MI:SS');
   BEGIN
        downtime = zero;
        FOR orec IN SELECT ifLostService,ifRegainedService
                FROM outages 
                WHERE ifServiceId = ifsrvid 
                        AND (
                                (ifRegainedService IS NULL AND ifLostService <= xtime)
                                OR (ifRegainedService > ytime)
                        )
        LOOP
         BEGIN
                gainTime := epochTime;
                lostTime := orec.ifLostService;
                IF orec.ifRegainedService IS NOT NULL THEN
                        gainTime := orec.ifRegainedService;
                END IF;
                --
                -- Find the appropriate records
                --
                IF xtime > lostTime THEN
                 --
                 -- for any outage to be in window of 
                 -- opportunity the lost time must ALWAYS be
                 -- less that the x time.
                 --
                 IF gainTime = epochTime THEN
                  --
                  -- if the gain time is epochTime then the outage
                  -- does not have an uptime.
                  --
                   IF ytime > lostTime THEN
                    downtime := downtime + EXTRACT(EPOCH FROM (xtime - ytime));
                   ELSE
                    downtime := downtime + EXTRACT(EPOCH FROM (xtime - lostTime));
                   END IF;
                 ELSE
                  IF xtime > gainTime AND gainTime > ytime THEN
                   --
                   -- regain time between x and y
                   --
                    IF ytime > lostTime THEN
                     downtime := downtime + EXTRACT (EPOCH FROM (gainTime - ytime));
                    ELSE
                     downtime := downtime + EXTRACT (EPOCH FROM (gainTime - lostTime));
                    END IF; 
                  ELSE
                   IF gainTime > xtime THEN
                   --
                   -- regain time greater than x, lost less that x
                   --
                    IF ytime > lostTime THEN
                     downtime := downtime + EXTRACT (EPOCH FROM (xtime - ytime));
                    ELSE
                     downtime := downtime + EXTRACT (EPOCH FROM (xtime - lostTime));
                    END IF;
                   -- end gainTime > xtime
                   END IF;
                  -- end xtime > gainTime AND gainTime > ytime
                  END IF;
                 -- end gaintime == epochTime
                 END IF;
                -- end xtime > lostTime
                END IF;
         END;
        END LOOP;
        RETURN downtime*1000.0;
   END;
$_$;


ALTER FUNCTION public.getoutagetimeinwindow(integer, timestamp without time zone, timestamp without time zone) OWNER TO opennms;

--
-- Name: getpercentavailabilityinwindow(integer, timestamp without time zone, timestamp without time zone); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION getpercentavailabilityinwindow(integer, timestamp without time zone, timestamp without time zone) RETURNS double precision
    LANGUAGE plpgsql
    AS $_$
   DECLARE
        ipifid ALIAS FOR $1;
        xtime ALIAS FOR $2;
        ytime ALIAS FOR $3;
        downtime float8;
   BEGIN
        downtime := getOutageTimeInWindow(ipifid, xtime, ytime);
        IF xtime > ytime THEN
                RETURN 100 * (1 - (downtime / (EXTRACT(EPOCH FROM (xtime - ytime))* 1000)));
        ELSE
                RETURN 100 * (1 - (downtime / (EXTRACT(EPOCH FROM (ytime - xtime))* 1000)));
        END IF;
   END;
$_$;


ALTER FUNCTION public.getpercentavailabilityinwindow(integer, timestamp without time zone, timestamp without time zone) OWNER TO opennms;

--
-- Name: iplike(text, text); Type: FUNCTION; Schema: public; Owner: opennms
--

CREATE FUNCTION iplike(i_ipaddress text, i_rule text) RETURNS boolean
    LANGUAGE plpgsql
    AS $_$
  declare
    c_i integer;
    c_r text;

    c_addrwork text;
    c_addrtemp text;
    c_rulework text;
    c_ruletemp text;
    c_scopeid text;

    i integer;

  begin
    if i_ipaddress is NULL or i_ipaddress is null then
        return 'f';
    end if;

    if i_rule = '*.*.*.*' or i_rule = '*:*:*:*:*:*:*:*' then
        return 't';
    end if;

    -- First, strip apart the IP address into octets, and
    -- verify that they are legitimate (0-255)
    --
    -- IPv4
    if i_ipaddress ~ E'^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' and i_rule ~ E'^[0-9*,-]+\.[0-9*,-]+\.[0-9*,-]+\.[0-9*,-]+$' then
        c_addrwork := i_ipaddress;
        c_rulework := i_rule;

        i := 0;
        while i < 4 loop
            if (strpos(c_addrwork, '.') > 0) then
                c_i := to_number(substr(c_addrwork, 0, strpos(c_addrwork, '.')), '999');
            else 
                c_i := to_number(c_addrwork, '999');
            end if;

            if c_i > 255 then
                return 'f';
            end if;
            c_addrwork := ltrim(ltrim(c_addrwork, '0123456789'), '.');

            if (strpos(c_rulework, '.') > 0) then
                c_r := substr(c_rulework, 0, strpos(c_rulework, '.'));
            else
                c_r := c_rulework;
            end if;

            if not check_rule(c_i, c_r) then
                return 'f';
            end if;

            c_rulework := ltrim(ltrim(c_rulework, '0123456789,-*'), '.');

            i := i + 1;
        end loop;
    -- IPv6
    elsif i_ipaddress ~ E'^[0-9a-f]+:[0-9a-f]+:[0-9a-f]+:[0-9a-f]+:[0-9a-f]+:[0-9a-f]+:[0-9a-f]+:[0-9a-f]+(%[0-9]+)?$' and i_rule ~ E'^[0-9a-f*,-]+:[0-9a-f*,-]+:[0-9a-f*,-]+:[0-9a-f*,-]+:[0-9a-f*,-]+:[0-9a-f*,-]+:[0-9a-f*,-]+:[0-9a-f*,-]+(%[0-9*,-]+)?$' then
        c_addrwork := i_ipaddress;
        c_rulework := i_rule;

        -- TODO Add support for scope identifiers

        i := 0;
        while i < 8 loop
            if (strpos(c_addrwork, ':') > 0) then
                c_addrtemp = substr(c_addrwork, 0, strpos(c_addrwork, ':'));
            else 
                c_addrtemp = c_addrwork;
            end if;
            if (strpos(c_addrtemp, '%') > 0) then
                -- Strip off the scope ID for now
                c_scopeid = substr(c_addrtemp, strpos(c_addrtemp, '%') + 1);
                c_addrtemp = substr(c_addrtemp, 0, strpos(c_addrtemp, '%'));
            end if;
            while length(c_addrtemp) < 4 loop
                c_addrtemp := '0' || c_addrtemp;
            end loop;
            c_i := cast(cast('x' || cast(c_addrtemp as text) as bit(16)) as integer);

            -- Max 16-bit integer value
            if c_i > 65535 then
                return 'f';
            end if;
            c_addrwork := ltrim(ltrim(c_addrwork, '0123456789abcdef'), ':');

            if (strpos(c_rulework, ':') > 0) then
                c_r := substr(c_rulework, 0, strpos(c_rulework, ':'));
            else
                c_r := c_rulework;
            end if;

            if not check_hex_rule(c_i, c_r) then
                return 'f';
            end if;

            c_rulework := ltrim(ltrim(c_rulework, '0123456789abcdef,-*'), ':');

            i := i + 1;
        end loop;
    else
        return 'f';
    end if;	

  return 't';
end;
$_$;


ALTER FUNCTION public.iplike(i_ipaddress text, i_rule text) OWNER TO opennms;

--
-- Name: plpgsql_call_handler(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION plpgsql_call_handler() RETURNS opaque
    LANGUAGE c
    AS '$libdir/plpgsql', 'plpgsql_call_handler';


ALTER FUNCTION public.plpgsql_call_handler() OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: accesslocks; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE accesslocks (
    lockname character varying(40) NOT NULL
);


ALTER TABLE accesslocks OWNER TO opennms;

--
-- Name: accesspoints; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE accesspoints (
    physaddr character varying(32) NOT NULL,
    nodeid integer,
    pollingpackage character varying(256) NOT NULL,
    status integer,
    controlleripaddr character varying(40)
);


ALTER TABLE accesspoints OWNER TO opennms;

--
-- Name: opennmsnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE opennmsnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE opennmsnxtid OWNER TO opennms;

--
-- Name: acks; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE acks (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    acktime timestamp with time zone DEFAULT now() NOT NULL,
    ackuser character varying(64) DEFAULT 'admin'::character varying NOT NULL,
    acktype integer DEFAULT 1 NOT NULL,
    ackaction integer DEFAULT 1 NOT NULL,
    log character varying(128),
    refid integer
);


ALTER TABLE acks OWNER TO opennms;

--
-- Name: alarm_attributes; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE alarm_attributes (
    alarmid integer NOT NULL,
    attributename character varying(63),
    attributevalue character varying(255)
);


ALTER TABLE alarm_attributes OWNER TO opennms;

--
-- Name: alarms; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE alarms (
    alarmid integer NOT NULL,
    eventuei character varying(256) NOT NULL,
    nodeid integer,
    ipaddr text,
    serviceid integer,
    reductionkey text,
    alarmtype integer,
    counter integer NOT NULL,
    severity integer NOT NULL,
    lasteventid integer,
    firsteventtime timestamp with time zone,
    lasteventtime timestamp with time zone,
    firstautomationtime timestamp with time zone,
    lastautomationtime timestamp with time zone,
    description text,
    logmsg text,
    operinstruct text,
    tticketid character varying(128),
    tticketstate integer,
    mouseovertext character varying(64),
    suppresseduntil timestamp with time zone,
    suppresseduser character varying(256),
    suppressedtime timestamp with time zone,
    alarmackuser character varying(256),
    alarmacktime timestamp with time zone,
    managedobjectinstance character varying(512),
    managedobjecttype character varying(512),
    applicationdn character varying(512),
    ossprimarykey character varying(512),
    x733alarmtype character varying(31),
    x733probablecause integer DEFAULT 0 NOT NULL,
    qosalarmstate character varying(31),
    clearkey text,
    ifindex integer,
    eventparms text,
    stickymemo integer,
    systemid text NOT NULL
);


ALTER TABLE alarms OWNER TO opennms;

--
-- Name: alarmsnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE alarmsnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE alarmsnxtid OWNER TO opennms;

--
-- Name: application_service_map; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE application_service_map (
    appid integer NOT NULL,
    ifserviceid integer NOT NULL
);


ALTER TABLE application_service_map OWNER TO opennms;

--
-- Name: applications; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE applications (
    id integer NOT NULL,
    name character varying(32) NOT NULL
);


ALTER TABLE applications OWNER TO opennms;

--
-- Name: assets; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE assets (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer,
    category text NOT NULL,
    manufacturer text,
    vendor text,
    modelnumber text,
    serialnumber text,
    description text,
    circuitid text,
    assetnumber text,
    operatingsystem text,
    rack text,
    slot text,
    port text,
    region text,
    division text,
    department text,
    address1 text,
    address2 text,
    city text,
    state text,
    zip text,
    building text,
    floor text,
    room text,
    vendorphone text,
    vendorfax text,
    vendorassetnumber text,
    userlastmodified character varying(20) NOT NULL,
    lastmodifieddate timestamp with time zone NOT NULL,
    dateinstalled character varying(64),
    lease text,
    leaseexpires character varying(64),
    supportphone text,
    maintcontract text,
    maintcontractexpires character varying(64),
    displaycategory text,
    notifycategory text,
    pollercategory text,
    thresholdcategory text,
    comment text,
    managedobjectinstance text,
    managedobjecttype text,
    username text,
    password text,
    enable text,
    autoenable character(1),
    connection character varying(32),
    cpu text,
    ram text,
    storagectrl text,
    hdd1 text,
    hdd2 text,
    hdd3 text,
    hdd4 text,
    hdd5 text,
    hdd6 text,
    numpowersupplies character varying(1),
    inputpower character varying(11),
    additionalhardware text,
    admin text,
    snmpcommunity character varying(32),
    rackunitheight character varying(2),
    vmwaremanagedobjectid text,
    vmwaremanagedentitytype text,
    vmwaremanagementserver text,
    vmwaretopologyinfo text,
    vmwarestate text,
    country text,
    longitude double precision,
    latitude double precision
);


ALTER TABLE assets OWNER TO opennms;

--
-- Name: bridgebridgelink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bridgebridgelink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    bridgeport integer,
    bridgeportifindex integer,
    bridgeportifname text,
    vlan integer,
    designatednodeid integer NOT NULL,
    designatedbridgeport integer,
    designatedbridgeportifindex integer,
    designatedbridgeportifname text,
    designatedvlan integer,
    bridgebridgelinkcreatetime timestamp with time zone NOT NULL,
    bridgebridgelinklastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE bridgebridgelink OWNER TO opennms;

--
-- Name: bridgeelement; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bridgeelement (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    basebridgeaddress character varying(12) NOT NULL,
    basenumports integer NOT NULL,
    basetype integer NOT NULL,
    vlan integer,
    vlanname text,
    stpprotocolspecification integer,
    stppriority integer,
    stpdesignatedroot character varying(16),
    stprootcost integer,
    stprootport integer,
    bridgenodecreatetime timestamp with time zone NOT NULL,
    bridgenodelastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE bridgeelement OWNER TO opennms;

--
-- Name: bridgemaclink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bridgemaclink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    bridgeport integer NOT NULL,
    bridgeportifindex integer,
    bridgeportifname text,
    vlan integer,
    macaddress character varying(12) NOT NULL,
    bridgemaclinkcreatetime timestamp with time zone NOT NULL,
    bridgemaclinklastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE bridgemaclink OWNER TO opennms;

--
-- Name: bridgestplink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bridgestplink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    stpport integer NOT NULL,
    stpportpriority integer NOT NULL,
    stpportstate integer NOT NULL,
    stpportenable integer NOT NULL,
    stpportpathcost integer NOT NULL,
    stpportifindex integer,
    stpportifname text,
    vlan integer,
    designatedroot character varying(16) NOT NULL,
    designatedcost integer NOT NULL,
    designatedbridge character varying(16) NOT NULL,
    designatedport character varying(4) NOT NULL,
    bridgestplinkcreatetime timestamp with time zone NOT NULL,
    bridgestplinklastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE bridgestplink OWNER TO opennms;

--
-- Name: bsm_map; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_map (
    id integer NOT NULL,
    type character varying(32) NOT NULL,
    severity integer
);


ALTER TABLE bsm_map OWNER TO opennms;

--
-- Name: bsm_reduce; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_reduce (
    id integer NOT NULL,
    type character varying(32) NOT NULL,
    threshold double precision,
    threshold_severity integer,
    base double precision
);


ALTER TABLE bsm_reduce OWNER TO opennms;

--
-- Name: bsm_service; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_service (
    id integer NOT NULL,
    bsm_reduce_id integer NOT NULL,
    name character varying(255)
);


ALTER TABLE bsm_service OWNER TO opennms;

--
-- Name: bsm_service_attributes; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_service_attributes (
    bsm_service_id integer NOT NULL,
    key character varying(255) NOT NULL,
    value text NOT NULL
);


ALTER TABLE bsm_service_attributes OWNER TO opennms;

--
-- Name: bsm_service_children; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_service_children (
    id integer NOT NULL,
    bsm_service_child_id integer NOT NULL
);


ALTER TABLE bsm_service_children OWNER TO opennms;

--
-- Name: bsm_service_edge; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_service_edge (
    id integer NOT NULL,
    enabled boolean NOT NULL,
    weight integer NOT NULL,
    bsm_map_id integer NOT NULL,
    bsm_service_id integer NOT NULL
);


ALTER TABLE bsm_service_edge OWNER TO opennms;

--
-- Name: bsm_service_ifservices; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_service_ifservices (
    id integer NOT NULL,
    ifserviceid integer NOT NULL,
    friendlyname character varying(255)
);


ALTER TABLE bsm_service_ifservices OWNER TO opennms;

--
-- Name: bsm_service_reductionkeys; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE bsm_service_reductionkeys (
    id integer NOT NULL,
    reductionkey text NOT NULL,
    friendlyname character varying(255)
);


ALTER TABLE bsm_service_reductionkeys OWNER TO opennms;

--
-- Name: catnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE catnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE catnxtid OWNER TO opennms;

--
-- Name: categories; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE categories (
    categoryid integer DEFAULT nextval('catnxtid'::regclass) NOT NULL,
    categoryname text NOT NULL,
    categorydescription character varying(256)
);


ALTER TABLE categories OWNER TO opennms;

--
-- Name: category_group; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE category_group (
    categoryid integer NOT NULL,
    groupid character varying(64) NOT NULL
);


ALTER TABLE category_group OWNER TO opennms;

--
-- Name: category_node; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE category_node (
    categoryid integer NOT NULL,
    nodeid integer NOT NULL
);


ALTER TABLE category_node OWNER TO opennms;

--
-- Name: cdpelement; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE cdpelement (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    cdpglobalrun integer NOT NULL,
    cdpglobaldeviceid text NOT NULL,
    cdpnodecreatetime timestamp with time zone NOT NULL,
    cdpnodelastpolltime timestamp with time zone NOT NULL,
    cdpglobaldeviceidformat integer
);


ALTER TABLE cdpelement OWNER TO opennms;

--
-- Name: cdplink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE cdplink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    cdpcacheifindex integer NOT NULL,
    cdpinterfacename text,
    cdpcacheaddresstype integer NOT NULL,
    cdpcacheaddress text NOT NULL,
    cdpcacheversion text NOT NULL,
    cdpcachedeviceid text NOT NULL,
    cdpcachedeviceport text NOT NULL,
    cdpcachedeviceplatform text NOT NULL,
    cdplinkcreatetime timestamp with time zone NOT NULL,
    cdplinklastpolltime timestamp with time zone NOT NULL,
    cdpcachedeviceindex integer NOT NULL
);


ALTER TABLE cdplink OWNER TO opennms;

--
-- Name: databasechangelog; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE databasechangelog (
    id character varying(63) NOT NULL,
    author character varying(63) NOT NULL,
    filename character varying(200) NOT NULL,
    dateexecuted timestamp with time zone NOT NULL,
    orderexecuted integer NOT NULL,
    exectype character varying(10) NOT NULL,
    md5sum character varying(35),
    description character varying(255),
    comments character varying(255),
    tag character varying(255),
    liquibase character varying(20)
);


ALTER TABLE databasechangelog OWNER TO opennms;

--
-- Name: databasechangeloglock; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE databasechangeloglock (
    id integer NOT NULL,
    locked boolean NOT NULL,
    lockgranted timestamp with time zone,
    lockedby character varying(255)
);


ALTER TABLE databasechangeloglock OWNER TO opennms;

--
-- Name: demandpollnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE demandpollnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE demandpollnxtid OWNER TO opennms;

--
-- Name: demandpolls; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE demandpolls (
    id integer NOT NULL,
    requesttime timestamp with time zone,
    username character varying(32),
    description character varying(128)
);


ALTER TABLE demandpolls OWNER TO opennms;

--
-- Name: eventsnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE eventsnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE eventsnxtid OWNER TO opennms;

--
-- Name: events; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE events (
    eventid integer DEFAULT nextval('eventsnxtid'::regclass) NOT NULL,
    eventuei character varying(256) NOT NULL,
    nodeid integer,
    eventtime timestamp with time zone NOT NULL,
    eventhost character varying(256),
    eventsource character varying(128) NOT NULL,
    ipaddr text,
    eventsnmphost character varying(256),
    serviceid integer,
    eventsnmp character varying(256),
    eventparms text,
    eventcreatetime timestamp with time zone NOT NULL,
    eventdescr text,
    eventloggroup character varying(32),
    eventlogmsg text,
    eventseverity integer NOT NULL,
    eventpathoutage character varying(1024),
    eventcorrelation character varying(1024),
    eventsuppressedcount integer,
    eventoperinstruct text,
    eventautoaction character varying(256),
    eventoperaction character varying(256),
    eventoperactionmenutext character varying(64),
    eventnotification character varying(128),
    eventtticket character varying(128),
    eventtticketstate integer,
    eventforward character varying(256),
    eventmouseovertext character varying(64),
    eventlog character(1) NOT NULL,
    eventdisplay character(1) NOT NULL,
    eventackuser character varying(256),
    eventacktime timestamp with time zone,
    alarmid integer,
    ifindex integer,
    systemid text NOT NULL
);


ALTER TABLE events OWNER TO opennms;

--
-- Name: filterfavorites; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE filterfavorites (
    filterid integer NOT NULL,
    username character varying(50) NOT NULL,
    filtername text NOT NULL,
    page character varying(25) NOT NULL,
    filter text NOT NULL
);


ALTER TABLE filterfavorites OWNER TO opennms;

--
-- Name: filternextid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE filternextid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE filternextid OWNER TO opennms;

--
-- Name: hwentity; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE hwentity (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    parentid integer,
    nodeid integer,
    entphysicalindex integer NOT NULL,
    entphysicalparentrelpos integer,
    entphysicalname character varying(128),
    entphysicaldescr character varying(128),
    entphysicalalias character varying(128),
    entphysicalvendortype character varying(128),
    entphysicalclass character varying(128),
    entphysicalmfgname character varying(128),
    entphysicalmodelname character varying(128),
    entphysicalhardwarerev character varying(128),
    entphysicalfirmwarerev character varying(128),
    entphysicalsoftwarerev character varying(128),
    entphysicalserialnum character varying(128),
    entphysicalassetid character varying(128),
    entphysicalisfru boolean,
    entphysicalmfgdate timestamp with time zone,
    entphysicaluris character varying(256)
);


ALTER TABLE hwentity OWNER TO opennms;

--
-- Name: hwentityattribute; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE hwentityattribute (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    hwentityid integer NOT NULL,
    hwattribtypeid integer NOT NULL,
    attribvalue character varying(256) NOT NULL
);


ALTER TABLE hwentityattribute OWNER TO opennms;

--
-- Name: hwentityattributetype; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE hwentityattributetype (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    attribname character varying(128) NOT NULL,
    attriboid character varying(128) NOT NULL,
    attribclass character varying(32) NOT NULL
);


ALTER TABLE hwentityattributetype OWNER TO opennms;

--
-- Name: ifservices; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ifservices (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    ifindex integer,
    serviceid integer NOT NULL,
    lastgood timestamp with time zone,
    lastfail timestamp with time zone,
    qualifier character(16),
    status character(1),
    source character(1),
    notify character(1),
    ipinterfaceid integer NOT NULL
);


ALTER TABLE ifservices OWNER TO opennms;

--
-- Name: inventory; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE inventory (
    nodeid integer NOT NULL,
    name character varying(30) NOT NULL,
    createtime timestamp with time zone NOT NULL,
    lastpolltime timestamp with time zone NOT NULL,
    pathtofile character varying(256) NOT NULL,
    status character(1) NOT NULL
);


ALTER TABLE inventory OWNER TO opennms;

--
-- Name: ipinterface; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ipinterface (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    ipaddr text NOT NULL,
    iphostname character varying(256),
    ismanaged character(1),
    ipstatus integer,
    iplastcapsdpoll timestamp with time zone,
    issnmpprimary character(1),
    snmpinterfaceid integer
);


ALTER TABLE ipinterface OWNER TO opennms;

--
-- Name: ipnettomedia; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ipnettomedia (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    netaddress text NOT NULL,
    physaddress character varying(32) NOT NULL,
    sourcenodeid integer NOT NULL,
    sourceifindex integer NOT NULL,
    createtime timestamp with time zone NOT NULL,
    lastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE ipnettomedia OWNER TO opennms;

--
-- Name: isiselement; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE isiselement (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    isissysid character varying(32) NOT NULL,
    isissysadminstate integer NOT NULL,
    isisnodecreatetime timestamp with time zone NOT NULL,
    isisnodelastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE isiselement OWNER TO opennms;

--
-- Name: isislink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE isislink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    isiscircindex integer NOT NULL,
    isisisadjindex integer NOT NULL,
    isiscircifindex integer,
    isiscircadminstate integer,
    isisisadjstate integer NOT NULL,
    isisisadjneighsnpaaddress character varying(80) NOT NULL,
    isisisadjneighsystype integer NOT NULL,
    isisisadjneighsysid character varying(32) NOT NULL,
    isisisadjnbrextendedcircid integer NOT NULL,
    isislinkcreatetime timestamp with time zone NOT NULL,
    isislinklastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE isislink OWNER TO opennms;

--
-- Name: lldpelement; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE lldpelement (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    lldpchassisid text NOT NULL,
    lldpchassisidsubtype integer NOT NULL,
    lldpsysname text NOT NULL,
    lldpnodecreatetime timestamp with time zone NOT NULL,
    lldpnodelastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE lldpelement OWNER TO opennms;

--
-- Name: lldplink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE lldplink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    lldplocalportnum integer NOT NULL,
    lldpportid text NOT NULL,
    lldpportidsubtype integer NOT NULL,
    lldpportdescr text NOT NULL,
    lldpportifindex integer,
    lldpremchassisid text NOT NULL,
    lldpremchassisidsubtype integer NOT NULL,
    lldpremsysname text NOT NULL,
    lldpremportid text NOT NULL,
    lldpremportidsubtype integer NOT NULL,
    lldpremportdescr text NOT NULL,
    lldplinkcreatetime timestamp with time zone NOT NULL,
    lldplinklastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE lldplink OWNER TO opennms;

--
-- Name: location_specific_status_changes; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE location_specific_status_changes (
    id integer NOT NULL,
    ifserviceid integer NOT NULL,
    statuscode integer NOT NULL,
    statustime timestamp with time zone NOT NULL,
    statusreason character varying(255),
    responsetime double precision,
    systemid text NOT NULL
);


ALTER TABLE location_specific_status_changes OWNER TO opennms;

--
-- Name: memonxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE memonxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE memonxtid OWNER TO opennms;

--
-- Name: memos; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE memos (
    id integer NOT NULL,
    created timestamp with time zone,
    updated timestamp with time zone,
    author character varying(256),
    body text,
    reductionkey character varying(256),
    type character varying(64)
);


ALTER TABLE memos OWNER TO opennms;

--
-- Name: monitoringlocations; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE monitoringlocations (
    id text NOT NULL,
    monitoringarea text NOT NULL,
    geolocation text,
    latitude double precision,
    longitude double precision,
    priority integer
);


ALTER TABLE monitoringlocations OWNER TO opennms;

--
-- Name: monitoringlocationscollectionpackages; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE monitoringlocationscollectionpackages (
    monitoringlocationid text NOT NULL,
    packagename text NOT NULL
);


ALTER TABLE monitoringlocationscollectionpackages OWNER TO opennms;

--
-- Name: monitoringlocationspollingpackages; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE monitoringlocationspollingpackages (
    monitoringlocationid text NOT NULL,
    packagename text NOT NULL
);


ALTER TABLE monitoringlocationspollingpackages OWNER TO opennms;

--
-- Name: monitoringlocationstags; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE monitoringlocationstags (
    monitoringlocationid text NOT NULL,
    tag text NOT NULL
);


ALTER TABLE monitoringlocationstags OWNER TO opennms;

--
-- Name: monitoringsystems; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE monitoringsystems (
    id text NOT NULL,
    label text,
    location text NOT NULL,
    type text NOT NULL,
    status text,
    last_updated timestamp with time zone
);


ALTER TABLE monitoringsystems OWNER TO opennms;

--
-- Name: monitoringsystemsproperties; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE monitoringsystemsproperties (
    monitoringsystemid text NOT NULL,
    property text NOT NULL,
    propertyvalue text
);


ALTER TABLE monitoringsystemsproperties OWNER TO opennms;

--
-- Name: ncs_attributes; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ncs_attributes (
    ncscomponent_id integer NOT NULL,
    key character varying(255) NOT NULL,
    value character varying(255) NOT NULL
);


ALTER TABLE ncs_attributes OWNER TO opennms;

--
-- Name: ncscomponent; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ncscomponent (
    id integer NOT NULL,
    version integer,
    name character varying(255),
    type character varying(255),
    foreignsource character varying(255),
    foreignid character varying(255),
    depsrequired character varying(12),
    nodeforeignsource character varying(64),
    nodeforeignid character varying(64),
    upeventuei character varying(255),
    downeventuei character varying(255)
);


ALTER TABLE ncscomponent OWNER TO opennms;

--
-- Name: node; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE node (
    nodeid integer NOT NULL,
    nodecreatetime timestamp with time zone NOT NULL,
    nodeparentid integer,
    nodetype character(1),
    nodesysoid character varying(256),
    nodesysname character varying(256),
    nodesysdescription character varying(256),
    nodesyslocation character varying(256),
    nodesyscontact character varying(256),
    nodelabel character varying(256) NOT NULL,
    nodelabelsource character(1),
    nodenetbiosname character varying(16),
    nodedomainname character varying(16),
    operatingsystem character varying(64),
    lastcapsdpoll timestamp with time zone,
    foreignsource character varying(64),
    foreignid character varying(64),
    location text NOT NULL
);


ALTER TABLE node OWNER TO opennms;

--
-- Name: nodenxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE nodenxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE nodenxtid OWNER TO opennms;

--
-- Name: notifications; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE notifications (
    notifyid integer NOT NULL,
    textmsg text NOT NULL,
    subject text,
    numericmsg character varying(256),
    pagetime timestamp with time zone,
    respondtime timestamp with time zone,
    answeredby character varying(256),
    nodeid integer,
    interfaceid text,
    serviceid integer,
    queueid character varying(256),
    eventid integer,
    eventuei character varying(256) NOT NULL,
    notifconfigname text
);


ALTER TABLE notifications OWNER TO opennms;

--
-- Name: notifynxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE notifynxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE notifynxtid OWNER TO opennms;

--
-- Name: ospfelement; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ospfelement (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    ospfrouterid character varying(16) NOT NULL,
    ospfadminstat integer NOT NULL,
    ospfversionnumber integer NOT NULL,
    ospfbdrrtrstatus integer NOT NULL,
    ospfasbdrrtrstatus integer NOT NULL,
    ospfrouteridnetmask character varying(16) NOT NULL,
    ospfrouteridifindex integer NOT NULL,
    ospfnodecreatetime timestamp with time zone NOT NULL,
    ospfnodelastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE ospfelement OWNER TO opennms;

--
-- Name: ospflink; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE ospflink (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    ospfipaddr character varying(16),
    ospfipmask character varying(16),
    ospfaddresslessindex integer,
    ospfifindex integer,
    ospfremrouterid character varying(16) NOT NULL,
    ospfremipaddr character varying(16) NOT NULL,
    ospfremaddresslessindex integer NOT NULL,
    ospflinkcreatetime timestamp with time zone NOT NULL,
    ospflinklastpolltime timestamp with time zone NOT NULL
);


ALTER TABLE ospflink OWNER TO opennms;

--
-- Name: outagenxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE outagenxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE outagenxtid OWNER TO opennms;

--
-- Name: outages; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE outages (
    outageid integer NOT NULL,
    svclosteventid integer,
    svcregainedeventid integer,
    iflostservice timestamp with time zone NOT NULL,
    ifregainedservice timestamp with time zone,
    suppresstime timestamp with time zone,
    suppressedby character varying(256),
    ifserviceid integer NOT NULL
);


ALTER TABLE outages OWNER TO opennms;

--
-- Name: pathoutage; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE pathoutage (
    nodeid integer NOT NULL,
    criticalpathip text NOT NULL,
    criticalpathservicename character varying(255)
);


ALTER TABLE pathoutage OWNER TO opennms;

--
-- Name: pollresultnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE pollresultnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE pollresultnxtid OWNER TO opennms;

--
-- Name: pollresults; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE pollresults (
    id integer NOT NULL,
    pollid integer,
    nodeid integer,
    ipaddr text,
    ifindex integer,
    serviceid integer,
    statuscode integer,
    statusname character varying(32),
    reason character varying(128)
);


ALTER TABLE pollresults OWNER TO opennms;

--
-- Name: qrtz_blob_triggers; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_blob_triggers (
    trigger_name character varying(80) NOT NULL,
    trigger_group character varying(80) NOT NULL,
    blob_data bytea
);


ALTER TABLE qrtz_blob_triggers OWNER TO opennms;

--
-- Name: qrtz_calendars; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_calendars (
    calendar_name character varying(80) NOT NULL,
    calendar bytea NOT NULL
);


ALTER TABLE qrtz_calendars OWNER TO opennms;

--
-- Name: qrtz_cron_triggers; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_cron_triggers (
    trigger_name character varying(80) NOT NULL,
    trigger_group character varying(80) NOT NULL,
    cron_expression character varying(80) NOT NULL,
    time_zone_id character varying(80)
);


ALTER TABLE qrtz_cron_triggers OWNER TO opennms;

--
-- Name: qrtz_fired_triggers; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_fired_triggers (
    entry_id character varying(95) NOT NULL,
    trigger_name character varying(80) NOT NULL,
    trigger_group character varying(80) NOT NULL,
    is_volatile boolean NOT NULL,
    instance_name character varying(80) NOT NULL,
    fired_time bigint NOT NULL,
    state character varying(16) NOT NULL,
    job_name character varying(80),
    job_group character varying(80),
    is_stateful boolean,
    requests_recovery boolean,
    priority integer
);


ALTER TABLE qrtz_fired_triggers OWNER TO opennms;

--
-- Name: qrtz_job_details; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_job_details (
    job_name character varying(80) NOT NULL,
    job_group character varying(80) NOT NULL,
    description character varying(120),
    job_class_name character varying(128) NOT NULL,
    is_durable boolean NOT NULL,
    is_volatile boolean NOT NULL,
    is_stateful boolean NOT NULL,
    requests_recovery boolean NOT NULL,
    job_data bytea NOT NULL
);


ALTER TABLE qrtz_job_details OWNER TO opennms;

--
-- Name: qrtz_job_listeners; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_job_listeners (
    job_name character varying(80) NOT NULL,
    job_group character varying(80) NOT NULL,
    job_listener character varying(80) NOT NULL
);


ALTER TABLE qrtz_job_listeners OWNER TO opennms;

--
-- Name: qrtz_locks; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_locks (
    lock_name character varying(40) NOT NULL
);


ALTER TABLE qrtz_locks OWNER TO opennms;

--
-- Name: qrtz_paused_trigger_grps; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_paused_trigger_grps (
    trigger_group character varying(80) NOT NULL
);


ALTER TABLE qrtz_paused_trigger_grps OWNER TO opennms;

--
-- Name: qrtz_scheduler_state; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_scheduler_state (
    instance_name character varying(80) NOT NULL,
    last_checkin_time bigint NOT NULL,
    checkin_interval bigint NOT NULL,
    recoverer character varying(80)
);


ALTER TABLE qrtz_scheduler_state OWNER TO opennms;

--
-- Name: qrtz_simple_triggers; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_simple_triggers (
    trigger_name character varying(80) NOT NULL,
    trigger_group character varying(80) NOT NULL,
    repeat_count bigint NOT NULL,
    repeat_interval bigint NOT NULL,
    times_triggered bigint NOT NULL
);


ALTER TABLE qrtz_simple_triggers OWNER TO opennms;

--
-- Name: qrtz_trigger_listeners; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_trigger_listeners (
    trigger_name character varying(80) NOT NULL,
    trigger_group character varying(80) NOT NULL,
    trigger_listener character varying(80) NOT NULL
);


ALTER TABLE qrtz_trigger_listeners OWNER TO opennms;

--
-- Name: qrtz_triggers; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE qrtz_triggers (
    trigger_name character varying(80) NOT NULL,
    trigger_group character varying(80) NOT NULL,
    job_name character varying(80) NOT NULL,
    job_group character varying(80) NOT NULL,
    is_volatile boolean NOT NULL,
    description character varying(120),
    next_fire_time bigint,
    prev_fire_time bigint,
    trigger_state character varying(16) NOT NULL,
    trigger_type character varying(8) NOT NULL,
    start_time bigint NOT NULL,
    end_time bigint,
    calendar_name character varying(80),
    misfire_instr smallint,
    job_data bytea,
    priority integer
);


ALTER TABLE qrtz_triggers OWNER TO opennms;

--
-- Name: reportcatalog; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE reportcatalog (
    id integer NOT NULL,
    reportid character varying(256) NOT NULL,
    title character varying(256) NOT NULL,
    date timestamp with time zone NOT NULL,
    location character varying(256) NOT NULL
);


ALTER TABLE reportcatalog OWNER TO opennms;

--
-- Name: reportcatalognxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE reportcatalognxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE reportcatalognxtid OWNER TO opennms;

--
-- Name: requisitioned_categories; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE requisitioned_categories (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    categoryid integer NOT NULL
);


ALTER TABLE requisitioned_categories OWNER TO opennms;

--
-- Name: resourcereference; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE resourcereference (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    resourceid character varying(255) NOT NULL
);


ALTER TABLE resourcereference OWNER TO opennms;

--
-- Name: scanreportlogs; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE scanreportlogs (
    scanreportid text NOT NULL,
    logtext text
);


ALTER TABLE scanreportlogs OWNER TO opennms;

--
-- Name: scanreportpollresults; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE scanreportpollresults (
    id text NOT NULL,
    scanreportid text NOT NULL,
    servicename text NOT NULL,
    serviceid integer NOT NULL,
    nodelabel text NOT NULL,
    nodeid integer NOT NULL,
    ipaddress text,
    statusreason text,
    responsetime double precision,
    statuscode integer NOT NULL,
    statustime timestamp with time zone
);


ALTER TABLE scanreportpollresults OWNER TO opennms;

--
-- Name: scanreportproperties; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE scanreportproperties (
    scanreportid text NOT NULL,
    property text NOT NULL,
    propertyvalue text
);


ALTER TABLE scanreportproperties OWNER TO opennms;

--
-- Name: scanreports; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE scanreports (
    id text NOT NULL,
    location text NOT NULL,
    locale text,
    "timestamp" timestamp with time zone
);


ALTER TABLE scanreports OWNER TO opennms;

--
-- Name: servicenxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE servicenxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE servicenxtid OWNER TO opennms;

--
-- Name: service; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE service (
    serviceid integer DEFAULT nextval('servicenxtid'::regclass) NOT NULL,
    servicename character varying(255) NOT NULL
);


ALTER TABLE service OWNER TO opennms;

--
-- Name: servicemap; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE servicemap (
    ipaddr text NOT NULL,
    servicemapname character varying(255) NOT NULL
);


ALTER TABLE servicemap OWNER TO opennms;

--
-- Name: snmpinterface; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE snmpinterface (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    nodeid integer NOT NULL,
    snmpipadentnetmask character varying(45),
    snmpphysaddr character varying(64),
    snmpifindex integer NOT NULL,
    snmpifdescr character varying(256),
    snmpiftype integer,
    snmpifname character varying(96),
    snmpifspeed bigint,
    snmpifadminstatus integer,
    snmpifoperstatus integer,
    snmpifalias character varying(256),
    snmpcollect character varying(2) DEFAULT 'N'::character varying,
    snmplastcapsdpoll timestamp with time zone,
    snmppoll character varying(1) DEFAULT 'N'::character varying,
    snmplastsnmppoll timestamp with time zone
);


ALTER TABLE snmpinterface OWNER TO opennms;

--
-- Name: statisticsreport; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE statisticsreport (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    startdate timestamp with time zone NOT NULL,
    enddate timestamp with time zone NOT NULL,
    name character varying(63) NOT NULL,
    description character varying(256) NOT NULL,
    jobstarteddate timestamp with time zone NOT NULL,
    jobcompleteddate timestamp with time zone NOT NULL,
    purgedate timestamp with time zone NOT NULL
);


ALTER TABLE statisticsreport OWNER TO opennms;

--
-- Name: statisticsreportdata; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE statisticsreportdata (
    id integer DEFAULT nextval('opennmsnxtid'::regclass) NOT NULL,
    reportid integer NOT NULL,
    resourceid integer NOT NULL,
    value double precision NOT NULL
);


ALTER TABLE statisticsreportdata OWNER TO opennms;

--
-- Name: subcomponents; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE subcomponents (
    component_id integer NOT NULL,
    subcomponent_id integer NOT NULL
);


ALTER TABLE subcomponents OWNER TO opennms;

--
-- Name: usernotifnxtid; Type: SEQUENCE; Schema: public; Owner: opennms
--

CREATE SEQUENCE usernotifnxtid
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE usernotifnxtid OWNER TO opennms;

--
-- Name: usersnotified; Type: TABLE; Schema: public; Owner: opennms
--

CREATE TABLE usersnotified (
    id integer NOT NULL,
    userid character varying(256) NOT NULL,
    notifyid integer,
    notifytime timestamp with time zone,
    media character varying(32),
    contactinfo character varying(64),
    autonotify character(1)
);


ALTER TABLE usersnotified OWNER TO opennms;

--
-- Data for Name: accesslocks; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY accesslocks (lockname) FROM stdin;
\.


--
-- Data for Name: accesspoints; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY accesspoints (physaddr, nodeid, pollingpackage, status, controlleripaddr) FROM stdin;
\.


--
-- Data for Name: acks; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY acks (id, acktime, ackuser, acktype, ackaction, log, refid) FROM stdin;
\.


--
-- Data for Name: alarm_attributes; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY alarm_attributes (alarmid, attributename, attributevalue) FROM stdin;
\.


--
-- Data for Name: alarms; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY alarms (alarmid, eventuei, nodeid, ipaddr, serviceid, reductionkey, alarmtype, counter, severity, lasteventid, firsteventtime, lasteventtime, firstautomationtime, lastautomationtime, description, logmsg, operinstruct, tticketid, tticketstate, mouseovertext, suppresseduntil, suppresseduser, suppressedtime, alarmackuser, alarmacktime, managedobjectinstance, managedobjecttype, applicationdn, ossprimarykey, x733alarmtype, x733probablecause, qosalarmstate, clearkey, ifindex, eventparms, stickymemo, systemid) FROM stdin;
\.


--
-- Name: alarmsnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('alarmsnxtid', 1, false);


--
-- Data for Name: application_service_map; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY application_service_map (appid, ifserviceid) FROM stdin;
\.


--
-- Data for Name: applications; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY applications (id, name) FROM stdin;
\.


--
-- Data for Name: assets; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY assets (id, nodeid, category, manufacturer, vendor, modelnumber, serialnumber, description, circuitid, assetnumber, operatingsystem, rack, slot, port, region, division, department, address1, address2, city, state, zip, building, floor, room, vendorphone, vendorfax, vendorassetnumber, userlastmodified, lastmodifieddate, dateinstalled, lease, leaseexpires, supportphone, maintcontract, maintcontractexpires, displaycategory, notifycategory, pollercategory, thresholdcategory, comment, managedobjectinstance, managedobjecttype, username, password, enable, autoenable, connection, cpu, ram, storagectrl, hdd1, hdd2, hdd3, hdd4, hdd5, hdd6, numpowersupplies, inputpower, additionalhardware, admin, snmpcommunity, rackunitheight, vmwaremanagedobjectid, vmwaremanagedentitytype, vmwaremanagementserver, vmwaretopologyinfo, vmwarestate, country, longitude, latitude) FROM stdin;
\.


--
-- Data for Name: bridgebridgelink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bridgebridgelink (id, nodeid, bridgeport, bridgeportifindex, bridgeportifname, vlan, designatednodeid, designatedbridgeport, designatedbridgeportifindex, designatedbridgeportifname, designatedvlan, bridgebridgelinkcreatetime, bridgebridgelinklastpolltime) FROM stdin;
\.


--
-- Data for Name: bridgeelement; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bridgeelement (id, nodeid, basebridgeaddress, basenumports, basetype, vlan, vlanname, stpprotocolspecification, stppriority, stpdesignatedroot, stprootcost, stprootport, bridgenodecreatetime, bridgenodelastpolltime) FROM stdin;
\.


--
-- Data for Name: bridgemaclink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bridgemaclink (id, nodeid, bridgeport, bridgeportifindex, bridgeportifname, vlan, macaddress, bridgemaclinkcreatetime, bridgemaclinklastpolltime) FROM stdin;
\.


--
-- Data for Name: bridgestplink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bridgestplink (id, nodeid, stpport, stpportpriority, stpportstate, stpportenable, stpportpathcost, stpportifindex, stpportifname, vlan, designatedroot, designatedcost, designatedbridge, designatedport, bridgestplinkcreatetime, bridgestplinklastpolltime) FROM stdin;
\.


--
-- Data for Name: bsm_map; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_map (id, type, severity) FROM stdin;
\.


--
-- Data for Name: bsm_reduce; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_reduce (id, type, threshold, threshold_severity, base) FROM stdin;
\.


--
-- Data for Name: bsm_service; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_service (id, bsm_reduce_id, name) FROM stdin;
\.


--
-- Data for Name: bsm_service_attributes; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_service_attributes (bsm_service_id, key, value) FROM stdin;
\.


--
-- Data for Name: bsm_service_children; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_service_children (id, bsm_service_child_id) FROM stdin;
\.


--
-- Data for Name: bsm_service_edge; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_service_edge (id, enabled, weight, bsm_map_id, bsm_service_id) FROM stdin;
\.


--
-- Data for Name: bsm_service_ifservices; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_service_ifservices (id, ifserviceid, friendlyname) FROM stdin;
\.


--
-- Data for Name: bsm_service_reductionkeys; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY bsm_service_reductionkeys (id, reductionkey, friendlyname) FROM stdin;
\.


--
-- Data for Name: categories; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY categories (categoryid, categoryname, categorydescription) FROM stdin;
1	Routers	\N
2	Switches	\N
3	Servers	\N
4	Production	\N
5	Test	\N
6	Development	\N
\.


--
-- Data for Name: category_group; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY category_group (categoryid, groupid) FROM stdin;
\.


--
-- Data for Name: category_node; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY category_node (categoryid, nodeid) FROM stdin;
\.


--
-- Name: catnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('catnxtid', 6, true);


--
-- Data for Name: cdpelement; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY cdpelement (id, nodeid, cdpglobalrun, cdpglobaldeviceid, cdpnodecreatetime, cdpnodelastpolltime, cdpglobaldeviceidformat) FROM stdin;
\.


--
-- Data for Name: cdplink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY cdplink (id, nodeid, cdpcacheifindex, cdpinterfacename, cdpcacheaddresstype, cdpcacheaddress, cdpcacheversion, cdpcachedeviceid, cdpcachedeviceport, cdpcachedeviceplatform, cdplinkcreatetime, cdplinklastpolltime, cdpcachedeviceindex) FROM stdin;
\.


--
-- Data for Name: databasechangelog; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY databasechangelog (id, author, filename, dateexecuted, orderexecuted, exectype, md5sum, description, comments, tag, liquibase) FROM stdin;
opennmsnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.020818-04	1	EXECUTED	3:95d7f1911278dc603bd6ad1af45ffa08	Create Sequence		\N	2.0.5-ONMS20160524b
nodenxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.030479-04	2	EXECUTED	3:9d6573fa6ce094d2e353d75476ea90bc	Create Sequence		\N	2.0.5-ONMS20160524b
servicenxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.044767-04	3	EXECUTED	3:8e32ff93e8b35b244a2d9ef0c119e01c	Create Sequence		\N	2.0.5-ONMS20160524b
eventsnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.060671-04	4	EXECUTED	3:caa50cdddc75832c6fb23e538da5bb24	Create Sequence		\N	2.0.5-ONMS20160524b
alarmsnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.06868-04	5	EXECUTED	3:34e4412336c86f8f26b09dc0a117c11c	Create Sequence		\N	2.0.5-ONMS20160524b
outagenxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.090003-04	6	EXECUTED	3:6d3721e0663b6622ad8e21b5b5c99964	Create Sequence		\N	2.0.5-ONMS20160524b
notifynxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.098327-04	7	EXECUTED	3:ec551776d0e02b3717947ee6f601efb0	Create Sequence		\N	2.0.5-ONMS20160524b
catnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.105906-04	8	EXECUTED	3:c3c6433f8b264fe9572e2c8304f90cde	Create Sequence		\N	2.0.5-ONMS20160524b
usernotifnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.113292-04	9	EXECUTED	3:8c31c2273aa70e01533e6ca873dcdb76	Create Sequence		\N	2.0.5-ONMS20160524b
demandpollnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.120844-04	10	EXECUTED	3:7c125fa55d01dd3967027c81871fc12e	Create Sequence		\N	2.0.5-ONMS20160524b
pollresultnxtid	rangerrick	create-sequences.xml	2016-10-20 10:28:42.128564-04	11	EXECUTED	3:95fbc40e227525988f43c6afd7d4a763	Create Sequence		\N	2.0.5-ONMS20160524b
1.6.0-serverMap	rangerrick	1.6.0/tables/serverMap.xml	2016-10-20 10:28:42.150906-04	12	EXECUTED	3:5e525e820142d333e8333d3e6d37dac6	Create Table, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-serviceMap	rangerrick	1.6.0/tables/serviceMap.xml	2016-10-20 10:28:42.177254-04	13	EXECUTED	3:999a107c5aba0b96dceb7de6cad6aa97	Create Table, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.6.0-distPoller	rangerrick	1.6.0/tables/distPoller.xml	2016-10-20 10:28:42.193812-04	14	EXECUTED	3:caf30968b035f0091f4feb7e3af973f1	Create Table, Insert Row		\N	2.0.5-ONMS20160524b
1.6.0-node	rangerrick	1.6.0/tables/node.xml	2016-10-20 10:28:42.25592-04	15	EXECUTED	3:8345c601959735c36d93e090acf19a39	Create Table, Add Foreign Key Constraint, Create Index (x4)		\N	2.0.5-ONMS20160524b
1.6.0-snmpInterface	rangerrick	1.6.0/tables/snmpInterface.xml	2016-10-20 10:28:42.326804-04	16	EXECUTED	3:881789cf6ed87406c071f5d71164dcb1	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-ipInterface	rangerrick	1.6.0/tables/ipInterface.xml	2016-10-20 10:28:42.403918-04	17	EXECUTED	3:2975a1965990c11927144ef4d0eea327	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x2), Create Index (x7)		\N	2.0.5-ONMS20160524b
1.6.0-service	rangerrick	1.6.0/tables/service.xml	2016-10-20 10:28:42.430359-04	18	EXECUTED	3:caa6640954859a10465c234e89c117b6	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-ifServices	rangerrick	1.6.0/tables/ifservices.xml	2016-10-20 10:28:42.514336-04	19	EXECUTED	3:bcebcffc7621fd938946ea977edd9b8e	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x3), Create Index (x7)		\N	2.0.5-ONMS20160524b
1.6.0-events	rangerrick	1.6.0/tables/events.xml	2016-10-20 10:28:42.657588-04	20	EXECUTED	3:11ae23b05437ecf37519aefba2bda9db	Create Table, Create Index (x12)		\N	2.0.5-ONMS20160524b
1.6.0-outages	rangerrick	1.6.0/tables/outages.xml	2016-10-20 10:28:42.761885-04	21	EXECUTED	3:57e081e259975ca8003eb6b9a180a77e	Create Table, Add Foreign Key Constraint (x6), Create Index (x8)		\N	2.0.5-ONMS20160524b
1.6.0-vulnerabilities	rangerrick	1.6.0/tables/vulnerabilities.xml	2016-10-20 10:28:42.815995-04	22	EXECUTED	3:eac8b2d70f70cadc76fc475d54bfc010	Create Table, Create Index (x5)		\N	2.0.5-ONMS20160524b
1.6.0-vulnplugins	rangerrick	1.6.0/tables/vulnplugins.xml	2016-10-20 10:28:42.842894-04	23	EXECUTED	3:2170294e171ab8b934f2780bb299d14a	Create Table, Add Primary Key		\N	2.0.5-ONMS20160524b
1.6.0-notifications	rangerrick	1.6.0/tables/notifications.xml	2016-10-20 10:28:42.949206-04	24	EXECUTED	3:0dbe9e5129b15fd099c8e866827a8154	Create Table, Add Foreign Key Constraint (x2), Create Index (x7)		\N	2.0.5-ONMS20160524b
1.6.0-usersnotified	rangerrick	1.6.0/tables/usersnotified.xml	2016-10-20 10:28:42.98137-04	25	EXECUTED	3:9758b84f83242eb088bf33746fb6eb60	Create Table, Add Foreign Key Constraint, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-alarms	rangerrick	1.6.0/tables/alarms.xml	2016-10-20 10:28:43.086796-04	26	EXECUTED	3:bf2fe2bd84eacb76952e7fb6c7f18f57	Create Table, Add Foreign Key Constraint, Create Index (x8)		\N	2.0.5-ONMS20160524b
1.6.0-alarm_attributes	rangerrick	1.6.0/tables/alarm_attributes.xml	2016-10-20 10:28:43.105239-04	27	EXECUTED	3:8c2117c376136c00b912eebd2ecc0880	Create Table, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.6.0-assets	rangerrick	1.6.0/tables/assets.xml	2016-10-20 10:28:43.143403-04	28	EXECUTED	3:7d403bf4c78c4e333dc9baa08340574b	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.6.0-categories	rangerrick	1.6.0/tables/categories.xml	2016-10-20 10:28:43.178444-04	29	EXECUTED	3:beebae13879f04517a8aa0491213d919	Create Table, Set Column as Auto-Increment, Create Index, Insert Row (x6)		\N	2.0.5-ONMS20160524b
1.6.0-category_node	rangerrick	1.6.0/tables/category_node.xml	2016-10-20 10:28:43.223237-04	30	EXECUTED	3:597ce8c6272a1d57d5443b56dd0eeee0	Create Table, Add Foreign Key Constraint (x2), Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-pathoutage	rangerrick	1.6.0/tables/pathoutage.xml	2016-10-20 10:28:43.271703-04	31	EXECUTED	3:243f3432a1b325d7a79e804601cec001	Create Table, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-demandpolls	rangerrick	1.6.0/tables/demandpolls.xml	2016-10-20 10:28:43.308199-04	32	EXECUTED	3:5ebe0c975c57e37234dc0cbd7b552890	Create Table, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-pollresults	rangerrick	1.6.0/tables/pollresults.xml	2016-10-20 10:28:43.34369-04	33	EXECUTED	3:650f88935740d0f7392e72e18266740b	Create Table, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.6.0-location_monitors	rangerrick	1.6.0/tables/location_monitors.xml	2016-10-20 10:28:43.356286-04	34	EXECUTED	3:0757a5f38706c1edd1b60696da487df4	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-location_monitor_details	rangerrick	1.6.0/tables/location_monitor_details.xml	2016-10-20 10:28:43.402573-04	35	EXECUTED	3:4e7a88dea24b9d29dfde89e746ad59de	Create Table, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.6.0-location_specific_status_changes	rangerrick	1.6.0/tables/location_specific_status_changes.xml	2016-10-20 10:28:43.501561-04	36	EXECUTED	3:26255be9ee237edd03afb09149a44a5a	Create Table, Add Foreign Key Constraint (x2), Create Index (x5)		\N	2.0.5-ONMS20160524b
1.6.0-applications	rangerrick	1.6.0/tables/applications.xml	2016-10-20 10:28:43.553901-04	37	EXECUTED	3:28cdcac53d7d06e01e82a01cc0dc0f01	Create Table, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-application_service_map	rangerrick	1.6.0/tables/application_service_map.xml	2016-10-20 10:28:43.620837-04	38	EXECUTED	3:3b05eee564ab100cf3c107a9d3ef6861	Create Table, Add Foreign Key Constraint (x2), Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-atinterface	rangerrick	1.6.0/tables/atinterface.xml	2016-10-20 10:28:43.714748-04	39	EXECUTED	3:e329018e82673c5757a4dd562a7331bb	Create Table, Set Column as Auto-Increment, Add Primary Key, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-recreate-hash-index	rangerrick	1.6.0/tables/atinterface.xml	2016-10-20 10:28:44.302378-04	40	EXECUTED	3:5f0f6ff4d636cc72e7fb1175493a9728	Drop Index, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-vlan	rangerrick	1.6.0/tables/vlan.xml	2016-10-20 10:28:44.365285-04	41	EXECUTED	3:9d7f14265d0bd4fc397529a7beaa8434	Create Table, Add Primary Key, Add Foreign Key Constraint, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-stpnode	rangerrick	1.6.0/tables/stpnode.xml	2016-10-20 10:28:44.456055-04	42	EXECUTED	3:92b87ac356e54712f063d5d890ed8c7d	Create Table, Add Primary Key, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-stpinterface	rangerrick	1.6.0/tables/stpinterface.xml	2016-10-20 10:28:44.555139-04	43	EXECUTED	3:399e9c23546977506fbb92a3bb30290e	Create Table, Add Primary Key, Add Foreign Key Constraint, Create Index (x4)		\N	2.0.5-ONMS20160524b
1.6.0-iprouteinterface	rangerrick	1.6.0/tables/iprouteinterface.xml	2016-10-20 10:28:44.660003-04	44	EXECUTED	3:be8dc0414e6151bf2834de37aa16d50f	Create Table, Add Primary Key, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-datalinkinterface	rangerrick	1.6.0/tables/datalinkinterface.xml	2016-10-20 10:28:44.779747-04	45	EXECUTED	3:8728d8aecacaf03b86c76a0b15ec05a4	Create Table, Add Primary Key, Add Foreign Key Constraint (x2), Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-inventory	rangerrick	1.6.0/tables/inventory.xml	2016-10-20 10:28:44.886027-04	46	EXECUTED	3:b6820c052a3951d49f18198e2b7f7ed6	Create Table, Add Foreign Key Constraint, Create Index (x4)		\N	2.0.5-ONMS20160524b
1.6.0-map	rangerrick	1.6.0/tables/map.xml	2016-10-20 10:28:44.942429-04	47	EXECUTED	3:4217cf900b2c6752b1a739d8c529d7d1	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-element	rangerrick	1.6.0/tables/element.xml	2016-10-20 10:28:45.04631-04	48	EXECUTED	3:cb390a6858a669c686f7c19a5d9563fb	Create Table, Add Primary Key, Add Foreign Key Constraint, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-reportlocator	rangerrick	1.6.0/tables/reportlocator.xml	2016-10-20 10:28:45.102156-04	49	EXECUTED	3:792df87ffb0e4a929da7074bcc176554	Create Table, Create Sequence		\N	2.0.5-ONMS20160524b
1.6.0-statisticsreport	rangerrick	1.6.0/tables/statisticsreport.xml	2016-10-20 10:28:45.236658-04	50	EXECUTED	3:0deafb6ff85f4fa7843adc728b94ec8f	Create Table, Set Column as Auto-Increment, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.6.0-resourcereference	rangerrick	1.6.0/tables/resourcereference.xml	2016-10-20 10:28:45.300877-04	51	EXECUTED	3:e0e7b1113ae3f23cd0f6f9dcd2413489	Create Table, Set Column as Auto-Increment, Create Index		\N	2.0.5-ONMS20160524b
1.6.0-statisticsreportdata	rangerrick	1.6.0/tables/statisticsreportdata.xml	2016-10-20 10:28:45.374101-04	52	EXECUTED	3:2f64069c427ea0f2be0f39284c5fb3cf	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x2), Create Index		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_job_details	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.456331-04	53	EXECUTED	3:76c15082e1b872df333ead122b6d58d8	Create Table, Add Primary Key		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_job_listeners	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.500334-04	54	EXECUTED	3:f6c48a43ad1448ad09dea1b99838f548	Create Table, Add Primary Key, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_triggers	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.570461-04	55	EXECUTED	3:040046f347f4d93b5f496dbf47db64ef	Create Table, Add Primary Key, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_simple_triggers	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.622819-04	56	EXECUTED	3:d26b92424ada455709e41365919b724f	Create Table, Add Primary Key, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_cron_triggers	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.67151-04	57	EXECUTED	3:b4aec4cdd3e0797b97befc1cb50811f4	Create Table, Add Primary Key, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_blob_triggers	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.744411-04	58	EXECUTED	3:dc9d228305de541207f6dab3a7f32f30	Create Table, Add Primary Key, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_trigger_listeners	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.79242-04	59	EXECUTED	3:ef270d8cad765fc106ab1c33132f9e06	Create Table, Add Primary Key, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_calendars	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.859395-04	60	EXECUTED	3:a7c49a9ca65e69c9d99c221d9948ebcf	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_paused_trigger_grps	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.906763-04	61	EXECUTED	3:b74d1abce56cefa196897acfb49d2dc1	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_fired_triggers	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:45.979635-04	62	EXECUTED	3:c16f6d4880e9042dc3665e3eeb144092	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_scheduler_state	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:46.017968-04	63	EXECUTED	3:f6ac29a288115210ce23942d23837067	Create Table		\N	2.0.5-ONMS20160524b
1.6.0-qrtz_locks	rangerrick	1.6.0/tables/quartz.xml	2016-10-20 10:28:46.068284-04	64	EXECUTED	3:f94acbb9f1d7e8d6cbdf163707ff8485	Create Table, Insert Row (x5)		\N	2.0.5-ONMS20160524b
1.7.0-ifindex-persistence	rssntn67	1.7.0/changelog.xml	2016-10-20 10:28:46.109525-04	65	EXECUTED	3:3b290f3b2fe547a5ae38192db2b92296	Add Column (x2), Create Index		\N	2.0.5-ONMS20160524b
1.7.1-ackd	dhustace	1.7.1/ackd.xml	2016-10-20 10:28:46.204112-04	66	EXECUTED	3:2e973f05e0c80c1f4dd954355c6681d7	Create Table, Set Column as Auto-Increment, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.7.1-assets	rssntn67	1.7.1/assets.xml	2016-10-20 10:28:46.231299-04	67	EXECUTED	3:5548f61017b23bac1df735550670bf6a	Add Column		\N	2.0.5-ONMS20160524b
1.7.1-datalinkinterface-index	mattraykow	1.7.1/datalinkinterface.xml	2016-10-20 10:28:46.282562-04	68	EXECUTED	3:862813172ea33cffdd7a4c50513b5677	Drop Primary Key, Add Column, Set Column as Auto-Increment		\N	2.0.5-ONMS20160524b
1.7.1-add-element-id	rssntn67	1.7.1/elements.xml	2016-10-20 10:28:46.34093-04	69	EXECUTED	3:2827faa37713d85ec4eb85b227b5b61d	Drop Primary Key, Add Column, Set Column as Auto-Increment		\N	2.0.5-ONMS20160524b
1.7.1-add-map-id	mattraykow	1.7.1/maps.xml	2016-10-20 10:28:46.395547-04	70	EXECUTED	3:cab72a06d1955f899030b117804abd4b	Drop Foreign Key Constraint, Add Primary Key, Set Column as Auto-Increment		\N	2.0.5-ONMS20160524b
1.7.1-add-map-group	rssntn67	1.7.1/maps.xml	2016-10-20 10:28:46.435051-04	71	EXECUTED	3:cac80936134cf15c06d9ee9fc009a5af	Add Column		\N	2.0.5-ONMS20160524b
1.7.1-move-snmpcollect	brozow	1.7.1/snmpcollect.xml	2016-10-20 10:28:46.602787-04	72	EXECUTED	3:4934f8677499916adc9a6a59c4bb9dd9	Add Column, Update Data, Custom SQL (x2), Update Data		\N	2.0.5-ONMS20160524b
1.7.1-expand-snmpPhysAddr	rangerrick	1.7.1/changelog.xml	2016-10-20 10:28:46.624907-04	73	EXECUTED	3:fcf974d77f234c961628c97f29e5841e	Modify data type		\N	2.0.5-ONMS20160524b
1.7.1-add-last-capsd-poll	brozow	1.7.1/changelog.xml	2016-10-20 10:28:46.643327-04	74	EXECUTED	3:4a08fa054ca528a4acc63ecbffa7e7cd	Add Column		\N	2.0.5-ONMS20160524b
1.7.3-expand-mapname	rssntn67	1.7.3/changelog.xml	2016-10-20 10:28:46.67954-04	75	EXECUTED	3:83880e3cbccf2657a52fd2d5bddf97e9	Modify data type		\N	2.0.5-ONMS20160524b
1.7.3-category-group	brozow	1.7.3/changelog.xml	2016-10-20 10:28:46.795511-04	76	EXECUTED	3:8576a7a7296acb2cf719b5fc820b09b4	Create Table, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.7.4-fix-broken-autoincrement	rangerrick	1.7.4/changelog.xml	2016-10-20 10:28:46.83473-04	77	EXECUTED	3:71be532c9752e8be2af496774a4eec4d	Set Column as Auto-Increment (x12)		\N	2.0.5-ONMS20160524b
1.7.5-addAlarmFkConstraint	dhustace	1.7.5/changelog.xml	2016-10-20 10:28:46.859924-04	78	EXECUTED	3:b900252a80c2c51236ab35f66468613a	Delete Data, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.7.5-fixElementFkConstraint	rssntn67	1.7.5/changelog.xml	2016-10-20 10:28:47.687711-04	79	EXECUTED	3:a443722d11d90729978939651d4f3897	Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.7.6-changePhysAddrColumns	rangerrick	1.7.6/changelog.xml	2016-10-20 10:28:47.708842-04	80	EXECUTED	3:29563e9f9cb6336a1661b68de2337158	Modify data type (x2)		\N	2.0.5-ONMS20160524b
1.7.7-add-linkTypeId	rssntn67	1.7.7/changelog.xml	2016-10-20 10:28:47.731187-04	81	EXECUTED	3:9c951644b369f77f9f6291ab2492c901	Add Column		\N	2.0.5-ONMS20160524b
1.7.8-add-linkState	rangerrick	1.7.8/changelog.xml	2016-10-20 10:28:47.782946-04	82	EXECUTED	3:e3a164c41f762370e47d3c220e06c270	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.7.8-update-linkstate	thedesloge	1.7.8/changelog.xml	2016-10-20 10:28:47.800224-04	83	EXECUTED	3:9b51bc44cbc14e61ab8e46ac5b303277	Modify data type		\N	2.0.5-ONMS20160524b
1.7.10-reportcatalog	jsartin	1.7.10/reportcatalog.xml	2016-10-20 10:28:47.858092-04	84	EXECUTED	3:4af78bb208e4e8aa26ca513cb66d137b	Create Table, Create Sequence		\N	2.0.5-ONMS20160524b
1.7.10-quartz-priority	jsartin	1.7.10/changelog.xml	2016-10-20 10:28:47.87714-04	85	EXECUTED	3:2c220ac1bee2f771f55bec7412acfd79	Add Column (x2)		\N	2.0.5-ONMS20160524b
1.7.10-drop-reportlocator	jsartin	1.7.10/changelog.xml	2016-10-20 10:28:47.900171-04	86	EXECUTED	3:ca46ade0e42f2e93cc7f1d3faf3e8882	Drop Table		\N	2.0.5-ONMS20160524b
1.7.10-snmp-poll	rssntn67	1.7.10/changelog.xml	2016-10-20 10:28:48.025673-04	87	EXECUTED	3:7d048835d0907ab78a3be74f06da211b	Add Column (x2)		\N	2.0.5-ONMS20160524b
1.8.5-repair-linkd-data	rangerrick	1.8.5/changelog.xml	2016-10-20 10:28:48.048548-04	88	EXECUTED	3:c62197268e863d6c2aebd6b230cfcd5f	Update Data (x8)		\N	2.0.5-ONMS20160524b
1.8.6-notifconfigname-size	rangerrick	1.8.6/changelog.xml	2016-10-20 10:28:48.075573-04	89	EXECUTED	3:facd59ecc3c46ed054846f8392bf2515	Modify data type		\N	2.0.5-ONMS20160524b
1.8.6-notifications-index	rangerrick	1.8.6/changelog.xml	2016-10-20 10:28:48.1155-04	90	EXECUTED	3:61f332fdfcc8bb778aec210c2d9fa6b4	Create Index		\N	2.0.5-ONMS20160524b
1.8.8-alarms-text-columns	rangerrick	1.8.8/changelog.xml	2016-10-20 10:28:48.143045-04	91	EXECUTED	3:aa4ccfe64d39600a55adefbca94f6aae	Modify data type (x2)		\N	2.0.5-ONMS20160524b
1.8.8-events-text-columns	rangerrick	1.8.8/changelog.xml	2016-10-20 10:28:48.179719-04	92	EXECUTED	3:2a5e5839bb9cefd1845bd69d578713cd	Modify data type (x2)		\N	2.0.5-ONMS20160524b
1.8.8-notifications-text-columns	rangerrick	1.8.8/changelog.xml	2016-10-20 10:28:48.194583-04	93	EXECUTED	3:1e79570e22f13bc44d29066c5efdc009	Modify data type		\N	2.0.5-ONMS20160524b
1.8.8-vulnerabilities-text-columns	rangerrick	1.8.8/changelog.xml	2016-10-20 10:28:48.220902-04	94	EXECUTED	3:ba940eb76c0a38f98b53c27fe7a5569a	Modify data type		\N	2.0.5-ONMS20160524b
1.8.11-service-servicename-length	David Schlenk <david.schlenk@spanlink.com>	1.8.11/changelog.xml	2016-10-20 10:28:48.245298-04	95	EXECUTED	3:ec185b079e389dab77d1ca3ea79a1871	Modify data type (x3)		\N	2.0.5-ONMS20160524b
1.9.4-remove-snmpinterface-ipaddr	seth	1.9.4/changelog.xml	2016-10-20 10:28:48.271021-04	96	EXECUTED	3:3b3245185eded3c5a00dd050412c7527	Drop Column		\N	2.0.5-ONMS20160524b
1.9.4-changeIpAddrTablesCountForIPV6	thedesloge	1.9.4/changelog.xml	2016-10-20 10:28:48.533662-04	97	EXECUTED	3:abd67639f7439d14bb53da52c52f2f2f	Modify data type (x11)		\N	2.0.5-ONMS20160524b
1.9.4-remove-alarms-clearuei	seth	1.9.4/changelog.xml	2016-10-20 10:28:48.558262-04	98	EXECUTED	3:d6ed53e9b4b3eada4cf4d57f9a67a8e7	Drop Column		\N	2.0.5-ONMS20160524b
1.9.7-expand-netmask-field	rangerrick	1.9.7/changelog.xml	2016-10-20 10:28:48.582755-04	99	EXECUTED	3:636ea9f3ccc33a06815fad0dfa143b8d	Modify data type		\N	2.0.5-ONMS20160524b
1.9.7-add-hardware-assets	kleind	1.9.7/changelog.xml	2016-10-20 10:28:48.618646-04	100	EXECUTED	3:723a24b3f6a27ec3bfa32c6cf501031a	Add Column		\N	2.0.5-ONMS20160524b
1.9.8-NMS-4597-add-not-null-application-service-map	rangerrick	1.9.8/changelog.xml	2016-10-20 10:28:48.647365-04	101	EXECUTED	3:3f26aa815b03e050fb03923e14932921	Delete Data, Add Not-Null Constraint (x2)		\N	2.0.5-ONMS20160524b
1.9.8-NMS-4597-add-not-null-category-node	rangerrick	1.9.8/changelog.xml	2016-10-20 10:28:48.67312-04	102	EXECUTED	3:a1d03bff71bdba9ed4f5a7ca6911d8dd	Delete Data, Add Not-Null Constraint (x2)		\N	2.0.5-ONMS20160524b
1.9.8-NMS-4597-add-not-null-pathoutage	rangerrick	1.9.8/changelog.xml	2016-10-20 10:28:48.700754-04	103	EXECUTED	3:13bf6b169dc6379ab62fb52ad317bdf8	Delete Data, Add Not-Null Constraint		\N	2.0.5-ONMS20160524b
1.9.9-add-not-null-constraint-nodelabel	seth	1.9.9/changelog.xml	2016-10-20 10:28:48.734457-04	104	EXECUTED	3:659cda4d96bb23e044fdb3959f1c4106	Add Not-Null Constraint		\N	2.0.5-ONMS20160524b
1.9.9-add-rackunitheight-hardware-assets	kleind	1.9.9/changelog.xml	2016-10-20 10:28:48.752073-04	105	EXECUTED	3:44507433aea5daad1fb1a95063ffb118	Add Column		\N	2.0.5-ONMS20160524b
1.9.9-more-space-for-hardware-assets	kleind	1.9.9/changelog.xml	2016-10-20 10:28:48.782796-04	106	EXECUTED	3:238ee27970be1d7ff184ec09d69a450c	Modify data type (x9)		\N	2.0.5-ONMS20160524b
1.9.90-add-accessLocks-table	brozow	1.9.90/changelog.xml	2016-10-20 10:28:48.851145-04	107	EXECUTED	3:d5ce222c4118f78881cc743fb37809fe	Create Table		\N	2.0.5-ONMS20160524b
1.9.90-make-accessLocks-case-insensitive	brozow	1.9.90/changelog.xml	2016-10-20 10:28:48.877576-04	108	EXECUTED	3:89b8b91a2b103a47559bc413296ab706	Rename Table, Rename Column		\N	2.0.5-ONMS20160524b
1.9.90-add-ids-to-linkd-vlan	rangerrick	1.9.90/changelog.xml	2016-10-20 10:28:48.984199-04	109	EXECUTED	3:42465e2035664c4615da77339f338f01	Add Column		\N	2.0.5-ONMS20160524b
1.9.90-add-ids-to-linkd-stpNode	rangerrick	1.9.90/changelog.xml	2016-10-20 10:28:49.120253-04	110	EXECUTED	3:6260172cbe417c1f4204b5521f12abfe	Add Column		\N	2.0.5-ONMS20160524b
1.9.90-add-ids-to-linkd-stpInterface	rangerrick	1.9.90/changelog.xml	2016-10-20 10:28:49.29418-04	111	EXECUTED	3:23683450ddae7dabbdc7fbc1006857f9	Add Column		\N	2.0.5-ONMS20160524b
1.9.90-add-ids-to-linkd-ipRouteInterface	rangerrick	1.9.90/changelog.xml	2016-10-20 10:28:49.45346-04	112	EXECUTED	3:700e80e2f9bc65483c1b99e0bc682cfc	Add Column		\N	2.0.5-ONMS20160524b
1.9.91-make-vlan-id-unique	rangerrick	1.9.91/changelog.xml	2016-10-20 10:28:50.306803-04	113	EXECUTED	3:7092b4dfe0e13d5e16d5766dfdda7d09	Create Index		\N	2.0.5-ONMS20160524b
1.9.91-make-stpnode-id-unique	rangerrick	1.9.91/changelog.xml	2016-10-20 10:28:51.148242-04	114	EXECUTED	3:ffa36f474cedb57bdade14df80d3a962	Create Index		\N	2.0.5-ONMS20160524b
1.9.91-make-stpinterface-id-unique	rangerrick	1.9.91/changelog.xml	2016-10-20 10:28:51.967964-04	115	EXECUTED	3:5be3e6218543a5f069147db157e6f47a	Create Index		\N	2.0.5-ONMS20160524b
1.9.91-make-iprouteinterface-id-unique	rangerrick	1.9.91/changelog.xml	2016-10-20 10:28:52.733183-04	116	EXECUTED	3:94fcfe7a93f6cd36b1f0218cd7bd3282	Create Index		\N	2.0.5-ONMS20160524b
1.9.92-changeInterfaeIDForIPV6	brozow	1.9.92/changelog.xml	2016-10-20 10:28:52.762839-04	117	EXECUTED	3:4a4c4bcffbb6be6943f429c71c4c76f6	Modify data type		\N	2.0.5-ONMS20160524b
1.9.94-addAssetFieldsForVMwareRequisition	ronny	1.9.94/changelog.xml	2016-10-20 10:28:52.792525-04	118	EXECUTED	3:8f355362db73bf8c64e7abf2c55a10b6	Add Column		\N	2.0.5-ONMS20160524b
1.10.1-increase-group-size	rangerrick	1.10.1/changelog.xml	2016-10-20 10:28:52.817219-04	119	EXECUTED	3:91f686d8937dd4b85c2ad5df51c629bf	Modify data type		\N	2.0.5-ONMS20160524b
1.10.4-subject-type	agalue	1.10.4/changelog.xml	2016-10-20 10:28:52.836399-04	120	EXECUTED	3:21ddb813de4e4792f5b50dad583fa14b	Modify data type		\N	2.0.5-ONMS20160524b
1.10.13-unique-service-type-alarms	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.850345-04	121	EXECUTED	3:d5987a75440ed2d3b406c2e990487087	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.13-unique-service-type-events	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.860474-04	122	EXECUTED	3:2994ccb6420c7684cd9445859e077aee	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.13-unique-service-type-ifservices	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.872783-04	123	EXECUTED	3:358b64d972c35834de52d6d9cb9976f4	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.13-unique-service-type-notifications	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.891338-04	124	EXECUTED	3:69a8880cfc0b56028c4178528e8d3d4d	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.13-unique-service-type-outages	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.90546-04	125	EXECUTED	3:2cbfcd6e308312897aef855f732b6dff	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.13-unique-service-type-vulnerabilities	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.915014-04	126	EXECUTED	3:b955b1934a51ae2e7cf2fbfa2e2e9644	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.13-delete-duplicate-services	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.962461-04	127	EXECUTED	3:08418302786c93514a508772c14acc6e	Delete Data, Add Unique Constraint		\N	2.0.5-ONMS20160524b
1.10.13-set-service-sequence	rangerrick	1.10.13/changelog.xml	2016-10-20 10:28:52.995759-04	128	EXECUTED	3:102f8e40dc09540d9d41049a72062c88	Custom SQL		\N	2.0.5-ONMS20160524b
1.10.3-alarm-note-feature	derTak	1.11.1/AlarmNotes.xml	2016-10-20 10:28:53.064692-04	129	EXECUTED	3:52c60bdb7502ab02f74d96e1709cbe27	Create Sequence, Create Table, Add Primary Key, Add Column, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
1.11.3-increase-size-of-physaddr	brozow	1.11.3/changelog.xml	2016-10-20 10:28:53.087847-04	130	EXECUTED	3:153f52b4177e74c8b4892777f99f1273	Modify data type		\N	2.0.5-ONMS20160524b
1.11.3-SNAPSHOT-accesspoints	jwhite	1.11.3/tables/accesspoints.xml	2016-10-20 10:28:53.155359-04	131	EXECUTED	3:49e8617495c188a37541d5fa27a62085	Create Table, Create Index		\N	2.0.5-ONMS20160524b
1.11.4-add-source-to-datalinkinterface	rangerrick	1.11.4/changelog.xml	2016-10-20 10:28:53.309475-04	132	EXECUTED	3:90956e9084acaf6691dc4b6619febac7	Add Column, Add Not-Null Constraint		\N	2.0.5-ONMS20160524b
1.11.4-add-geolocation-to-assets	agalue	1.11.4/changelog.xml	2016-10-20 10:28:53.339365-04	133	EXECUTED	3:e0e6db2b50a2ae499e9cd38ec90cdf4c	Add Column		\N	2.0.5-ONMS20160524b
1.11.4-add-country-to-assets	agalue	1.11.4/changelog.xml	2016-10-20 10:28:53.361742-04	134	EXECUTED	3:e14cf443b13f840c1677bbe275916bf4	Add Column		\N	2.0.5-ONMS20160524b
1.11.90-change-geolocation-to-floats	rangerrick	1.11.90/changelog.xml	2016-10-20 10:28:53.386425-04	135	EXECUTED	3:3cc360207da6e58f724a67bb39b25eae	Drop Column, Add Column (x2)		\N	2.0.5-ONMS20160524b
1.11.95-increase-reduction-key-columns	rangerrick	1.11.95/changelog.xml	2016-10-20 10:28:53.440582-04	136	EXECUTED	3:8a321d1eefae95e4ed743ab9e916f9f9	Modify data type (x2)		\N	2.0.5-ONMS20160524b
1.12.2-changeTypeOfVmwareTopologyInfo	agalue	1.12.2/changelog.xml	2016-10-20 10:28:53.480562-04	137	EXECUTED	3:e133d3e608d270859f4176cd9848c279	Modify data type		\N	2.0.5-ONMS20160524b
1.12.5-CategoryNames-to-text	dertak	1.12.5/changelog.xml	2016-10-20 10:28:53.548739-04	138	EXECUTED	3:c644cbe2fee6cbce06873a004fe69926	Modify data type		\N	2.0.5-ONMS20160524b
1.12.6-fix-null-dpname	rangerrick	1.12.6/changelog.xml	2016-10-20 10:28:53.56283-04	139	EXECUTED	3:bede467602ae04f4c1be8a8fe9d208b5	Update Data (x2)		\N	2.0.5-ONMS20160524b
1.13.0-introduce-filters-for-alarms-and-events	MVR	1.13.0/changelog.xml	2016-10-20 10:28:53.611094-04	140	EXECUTED	3:14b3ae19a478c0be38a599402b2ad563	Create Table, Create Sequence		\N	2.0.5-ONMS20160524b
1.13.0-createSequences-for-filters-table	MVR	1.13.0/changelog.xml	2016-10-20 10:28:53.632877-04	141	EXECUTED	3:86d0b5ec8da16b25ed26c4fc234c0af0	Set Sequence		\N	2.0.5-ONMS20160524b
1.13.0-addIndex-for-filters	MVR	1.13.0/changelog.xml	2016-10-20 10:28:53.680236-04	142	EXECUTED	3:b2ab06691e0f3361434a8817751458d1	Create Index		\N	2.0.5-ONMS20160524b
1.13.0-add-protocol-to-datalinkinterface	rssntn67	1.13.0/changelog.xml	2016-10-20 10:28:53.70743-04	143	EXECUTED	3:22794f014f18f8dea4b13802a18345e3	Add Column		\N	2.0.5-ONMS20160524b
1.13.1-newLinkdSearchProviderIndexes	david@opennms.org	1.13.1/changelog.xml	2016-10-20 10:28:53.789437-04	144	EXECUTED	3:5a761b828339a30216cc41400933bd0b	Create Index (x2)		\N	2.0.5-ONMS20160524b
1.13.1-lldpelement	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:53.922096-04	145	EXECUTED	3:2fbef27da3e4abc343ba16534b885646	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-lldplink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.071909-04	146	EXECUTED	3:0290a5d326305cd038452341453b5542	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-ospfelement	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.179312-04	147	EXECUTED	3:d947c2aca7e7a4b371bfe5ddfffd63de	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.13.1-ospflink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.30067-04	148	EXECUTED	3:3ff4df8a0ccd6cc917057dcf7f4d14fa	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-isiselement	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.389556-04	149	EXECUTED	3:8a1cf505adf3f64c636a5ed704916809	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.13.1-isislink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.49745-04	150	EXECUTED	3:a5bce6d95d7ae18b7b51132a3695bc8f	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-ipnettomedia	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.64068-04	151	EXECUTED	3:a3fa9576f6482f550f0ae623d2982161	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-bridgeelement	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.745116-04	152	EXECUTED	3:398332b3378bf46905288fe4d6e6f2a1	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-bridgemaclink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:54.890878-04	153	EXECUTED	3:5d784d7cd0c1182def7f07076b339b11	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x4)		\N	2.0.5-ONMS20160524b
1.13.1-bridgestplink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:55.051731-04	154	EXECUTED	3:622413e697e2696373fbc141e38a5854	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x4)		\N	2.0.5-ONMS20160524b
1.13.1-bridgebridgelink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:55.152426-04	155	EXECUTED	3:e32e8df5eca2525ae24f71c6dc1c49db	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x2), Create Index (x3)		\N	2.0.5-ONMS20160524b
1.13.1-cdpelement	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:55.260689-04	156	EXECUTED	3:678a2c6c98829a7f22c701a07b484dcb	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.13.1-cdplink	rssntn67	1.13.1/changelog.xml	2016-10-20 10:28:55.430027-04	157	EXECUTED	3:19a1ed61d590d9db80a91766a519ad6e	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint, Create Index (x4)		\N	2.0.5-ONMS20160524b
fix-typo-in-isis-table	ranger@opennms.org	1.13.1/changelog.xml	2016-10-20 10:28:55.475397-04	158	EXECUTED	3:80245c6f636be881b4ee3d22e8c502cb	Rename Column		\N	2.0.5-ONMS20160524b
1.13.3-pathOutage-ipv6	ranger@opennms.org	1.13.3/changelog.xml	2016-10-20 10:28:55.54719-04	159	EXECUTED	3:2c183004dc9706656d1efc9786998812	Modify data type		\N	2.0.5-ONMS20160524b
1.13.4-minion-base	ranger@opennms.org	1.13.4/changelog.xml	2016-10-20 10:28:55.648723-04	160	EXECUTED	3:fa3cb3da56b2c345440316ad72f3c212	Create Table (x2), Add Foreign Key Constraint, Create Index		\N	2.0.5-ONMS20160524b
1.13.4-add-primary-key-to-minions-properties	ranger@opennms.org	1.13.4/changelog.xml	2016-10-20 10:28:55.725352-04	161	EXECUTED	3:26fa1320d2df423d055332b2fc6cb5f3	Rename Column, Add Column, Add Primary Key		\N	2.0.5-ONMS20160524b
1.13.5-hwEntity	agalue	1.13.5/changelog.xml	2016-10-20 10:28:55.815308-04	162	EXECUTED	3:78148d5e8dcc7f3bf1f83630a9ed4d44	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x2), Create Index (x2)		\N	2.0.5-ONMS20160524b
1.13.5-hwEntityAttributeType	agalue	1.13.5/changelog.xml	2016-10-20 10:28:55.882353-04	163	EXECUTED	3:d2cd1afe262c73dc14716f40d1be0169	Create Table, Set Column as Auto-Increment, Create Index (x2)		\N	2.0.5-ONMS20160524b
1.13.5-hwEntityAttribute	agalue	1.13.5/changelog.xml	2016-10-20 10:28:55.937665-04	164	EXECUTED	3:47b04441db90d3a7fd5e0c999271ebce	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x2), Create Index		\N	2.0.5-ONMS20160524b
1.13.5-requisitioned-categories	ranger@opennms.org	1.13.5/changelog.xml	2016-10-20 10:28:55.981417-04	165	EXECUTED	3:e6d94685acf569b3eb2bff2f2de8e4f0	Create Table, Set Column as Auto-Increment, Add Foreign Key Constraint (x2), Create Index		\N	2.0.5-ONMS20160524b
1.40.0-changeDataTypeOfAssetFieldsToText	cpape	1.14.0/changelog.xml	2016-10-20 10:28:56.024204-04	166	EXECUTED	3:543d037439993ea44123f678fb8eab86	Modify data type (x55)		\N	2.0.5-ONMS20160524b
1.14.0-changeDataTypeOfcdpLinkCdpVersion	rssntn67	1.14.0/changelog.xml	2016-10-20 10:28:56.04534-04	167	EXECUTED	3:ba3f6176f6d0d0a7997af38d8d0b55ee	Modify data type		\N	2.0.5-ONMS20160524b
14.0.3-changeDataTypeOfFilterFavoritesToText	thedesloge	14.0.3/changelog.xml	2016-10-20 10:28:56.077193-04	168	EXECUTED	3:9883d26625868accc7d3646545e0caee	Modify data type (x2)		\N	2.0.5-ONMS20160524b
14.0.4-changeDataTypeOfOperInstructToText	agalue	14.0.4/changelog.xml	2016-10-20 10:28:56.096986-04	169	EXECUTED	3:ff69e8a4242be74e49b79158d7db4b0d	Modify data type (x2)		\N	2.0.5-ONMS20160524b
1.15.1-removeDuplicateOutages	jwhite	1.15.1/changelog.xml	2016-10-20 10:28:56.107119-04	170	EXECUTED	3:9620f7022690573df35979f1245fd227	Custom SQL		\N	2.0.5-ONMS20160524b
1.15.1-enforceUniqueOutages	jwhite	1.15.1/changelog.xml	2016-10-20 10:28:56.140348-04	171	EXECUTED	3:5b3d77e1400d4e8ed5e971c647300ef6	Custom SQL		\N	2.0.5-ONMS20160524b
1.15.2-usetextforlldpcdp	rssntn67	1.15.2/changelog.xml	2016-10-20 10:28:56.269226-04	172	EXECUTED	3:5797a911951aa0bea2f087cdedd649b2	Modify data type (x18)		\N	2.0.5-ONMS20160524b
1.15.2-cdplink-fix-nms7563	rssntn67	1.15.2/changelog.xml	2016-10-20 10:28:56.316097-04	173	EXECUTED	3:34791e71a34e7d40095257c5e8dbc404	Modify data type, Drop Not-Null Constraint, Add Column, Update Data, Add Not-Null Constraint, Create Index		\N	2.0.5-ONMS20160524b
16.0.4-useVarcharInsteadOfCharInAssetsTable	jwhite	16.0.4/changelog.xml	2016-10-20 10:28:56.420052-04	174	EXECUTED	3:3510bd16981e8e7d73bfcb662fc7c0ba	Modify data type, Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-drop-vulnplugins-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:56.442199-04	175	EXECUTED	3:8a85910a47b6671163dbd430ebbc01a9	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-drop-vulnerabilities-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:56.464475-04	176	EXECUTED	3:4bfc7afa8caa87627b8c553786cd529b	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-drop-vulnnxtid-sequence	seth	17.0.0/changelog.xml	2016-10-20 10:28:56.473159-04	177	EXECUTED	3:d0243e4928923916b1e5848d667d5321	Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-remove-misspelled-outage-index	seth	17.0.0/changelog.xml	2016-10-20 10:28:57.900441-04	178	EXECUTED	3:c22c71b263e9815e2a23f2895eff34a9	Drop Index		\N	2.0.5-ONMS20160524b
17.0.0-add-correctly-spelled-outage-index	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.125001-04	179	EXECUTED	3:2107e8dcd25aca62449d76337d9b90f7	Create Index		\N	2.0.5-ONMS20160524b
17.0.0-remove-legacy-triggers-and-functions	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.139228-04	180	EXECUTED	3:5b92a9e05f1cdc74a107403f73f8754a	Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-remove-legacy-ifservices-composite-key-fields	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.198095-04	181	EXECUTED	3:81b0ed1493103094ef7ae6fca49532d9	Drop Index (x4), Custom SQL, Create Index (x2), Drop Column (x2)		\N	2.0.5-ONMS20160524b
17.0.0-remove-legacy-ipinterface-composite-key-fields	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.215144-04	182	EXECUTED	3:248cc56037f920402dc7c5c2e6fb455c	Drop Column		\N	2.0.5-ONMS20160524b
17.0.0-remove-legacy-outages-composite-key-fields	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.28778-04	183	EXECUTED	3:d52f2488cf3157eab5614d38ba9e3d80	Drop Index (x4), Drop Column (x3)		\N	2.0.5-ONMS20160524b
1.17.0-linkd-removal	cpape	17.0.0/changelog.xml	2016-10-20 10:28:59.340976-04	184	EXECUTED	3:d493d0628415adcc3b076bad7ab836b6	Drop Table (x9)		\N	2.0.5-ONMS20160524b
17.0.0-drop-servermap-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.361808-04	185	EXECUTED	3:0fc0f485f16aa5235ac5172d16ab0f6a	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-create-cdpglobaldeviceidformat-on-cdpelement-table	rssntn67	17.0.0/changelog.xml	2016-10-20 10:28:59.38331-04	186	EXECUTED	3:db0ad992881928525e62b226e9fab7b7	Add Column		\N	2.0.5-ONMS20160524b
17.0.0-create-monitoringsystems-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.438157-04	187	EXECUTED	3:6601d08b9e4fd2f54bf98e757611416c	Create Table		\N	2.0.5-ONMS20160524b
17.0.0-create-monitoringsystemsproperties-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.513701-04	188	EXECUTED	3:6d8a10039bbf99dac21a87812d3193a4	Create Table, Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-insert-default-monitoringsystem	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.532815-04	189	EXECUTED	3:fa5ac612f3b27e818b64111ea6d8c914	Insert Row		\N	2.0.5-ONMS20160524b
17.0.0-disassociate-node-from-distpoller	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.553251-04	190	EXECUTED	3:bdf42c641301d6320268bcabd11d87f6	Drop Column		\N	2.0.5-ONMS20160524b
17.0.0-reassociate-alarms-with-monitoringsystems	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.595106-04	191	EXECUTED	3:6c0eff30634b77f194535a9c8ec8ea6f	Drop Column, Add Column, Add Not-Null Constraint, Add Foreign Key Constraint, Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-reassociate-events-with-monitoringsystems	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.615637-04	192	EXECUTED	3:42891304e22b5f4e78a4778cebf09e05	Drop Column, Add Column, Add Not-Null Constraint		\N	2.0.5-ONMS20160524b
17.0.0-copy-location_monitors-into-monitoringsystems	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.625271-04	193	EXECUTED	3:1ea9208abd1a1fc5b4e31c4dd3fa0a0d	Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-reassociate-loc_spec_status_changes-with-systems	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.718553-04	194	EXECUTED	3:b92304ea86bee8447f0adbda773c65a0	Add Column, Custom SQL, Add Not-Null Constraint, Add Foreign Key Constraint, Custom SQL, Drop Column		\N	2.0.5-ONMS20160524b
17.0.0-drop-distpoller-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.741299-04	195	EXECUTED	3:48dd6e42fd9c4fb4d66f3a440d391309	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-drop-location-monitor-details-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.767154-04	196	EXECUTED	3:75b0d1ace97e7c40cf98989873015342	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-drop-location-monitors-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.785117-04	197	EXECUTED	3:39a3b00108702dac9625bd3af4671e23	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-drop-minions-properties-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.81356-04	198	EXECUTED	3:5c300237b39c6963dea0adf1a8198748	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-drop-minions-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.834152-04	199	EXECUTED	3:99acb2e14997d318720822137499554b	Drop Table		\N	2.0.5-ONMS20160524b
17.0.0-create-monitoringlocations-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.891907-04	200	EXECUTED	3:79d4d6f120d13f65ca3bfa1e3411d955	Create Table		\N	2.0.5-ONMS20160524b
17.0.0-create-monitoringlocationspollingpackages-table	seth	17.0.0/changelog.xml	2016-10-20 10:28:59.961094-04	201	EXECUTED	3:11a2a834a3e65ec1ed8bfbe503f25735	Create Table, Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-create-monitoringlocationscollectionpackages-table	seth	17.0.0/changelog.xml	2016-10-20 10:29:00.024811-04	202	EXECUTED	3:3337200e071dfa2207a95c6ddc67f904	Create Table, Custom SQL		\N	2.0.5-ONMS20160524b
17.0.0-create-monitoringlocationstags-table	seth	17.0.0/changelog.xml	2016-10-20 10:29:00.103016-04	203	EXECUTED	3:fe4345e7409047f84afa36990a374ef2	Create Table, Custom SQL		\N	2.0.5-ONMS20160524b
18.0.0-add-scanreports-tables	ranger	18.0.0/changelog.xml	2016-10-20 10:29:00.283001-04	204	EXECUTED	3:81f9dd9561351a79ecbc2b55578f7642	Create Table, Custom SQL, Create Table, Custom SQL, Create Table, Custom SQL		\N	2.0.5-ONMS20160524b
18.0.0-add-scanreportlogs-table	ranger	18.0.0/changelog.xml	2016-10-20 10:29:00.347122-04	205	EXECUTED	3:0d9fcee13a5c7c3d661bbd06fd325cdf	Create Table, Add Foreign Key Constraint, Custom SQL		\N	2.0.5-ONMS20160524b
18.0.0-bsm-initialize	bsm-team	18.0.0/changelog.xml	2016-10-20 10:29:00.62788-04	206	EXECUTED	3:3e32110b0dbbe075589e2aad58dd6ac3	Create Table (x2), Add Primary Key, Add Foreign Key Constraint, Create Table, Add Primary Key, Create Table, Add Primary Key, Add Foreign Key Constraint, Create Table, Add Primary Key, Add Foreign Key Constraint (x2), Create Table, Add Primary Key, Add...		\N	2.0.5-ONMS20160524b
18.0.0-bsm-highest-severity-above	mvrueden	18.0.0/changelog.xml	2016-10-20 10:29:00.655609-04	207	EXECUTED	3:8b7d3059ba8c844603b657edcbe321bc	Add Column		\N	2.0.5-ONMS20160524b
18.0.0-bsm-most-critical-to-highest-severity	fooker	18.0.0/changelog.xml	2016-10-20 10:29:00.669023-04	208	EXECUTED	3:bb316e4ec0f744fc1eb6d8cac6c1701e	Update Data		\N	2.0.5-ONMS20160524b
18.0.0-bsm-friendlynames	cpape	18.0.0/changelog.xml	2016-10-20 10:29:00.696092-04	209	EXECUTED	3:07d21430feecf8eb0815cb922580d201	Add Column (x2)		\N	2.0.5-ONMS20160524b
18.0.0-bsm-map-reduce-cleanup	mvrueden	18.0.0/changelog.xml	2016-10-20 10:29:00.708279-04	210	EXECUTED	3:acf5f73f05a296040f8711565165803e	Delete Data (x2)		\N	2.0.5-ONMS20160524b
18.0.0-bsm-unique-names	fooker	18.0.0/changelog.xml	2016-10-20 10:29:00.770925-04	211	EXECUTED	3:8e0b63f29fb6f0e361cab7bbd0155b7d	Modify data type, Add Unique Constraint		\N	2.0.5-ONMS20160524b
19.0.0-insert-default-monitoringlocation	seth	19.0.0/changelog.xml	2016-10-20 10:29:00.787116-04	212	EXECUTED	3:dd72ba95d57a973712e03b6f49ccd9d7	Insert Row		\N	2.0.5-ONMS20160524b
19.0.0-associate-nodes-with-monitoringlocation	seth	19.0.0/changelog.xml	2016-10-20 10:29:00.807827-04	213	EXECUTED	3:bfab5d017f22da87aaae6e385ff89d77	Add Column, Add Not-Null Constraint, Add Foreign Key Constraint		\N	2.0.5-ONMS20160524b
19.0.0-bsm-exponentatial-propagate	fooker	19.0.0/changelog.xml	2016-10-20 10:29:00.82855-04	214	EXECUTED	3:71685df0b019759d7e9cab1f7c347872	Add Column		\N	2.0.5-ONMS20160524b
19.0.0-drop-mapnxtid-reportnxtid-sequences	seth	19.0.0/changelog.xml	2016-10-20 10:29:00.850148-04	215	EXECUTED	3:15165dc9767513e7ec8946973684315e	Custom SQL		\N	2.0.5-ONMS20160524b
19.0.0-change-name-of-default-monitoringlocation	seth	19.0.0/changelog.xml	2016-10-20 10:29:00.882089-04	216	EXECUTED	3:7af6b8d432ce1720db69272a45454e04	Drop Foreign Key Constraint (x5), Add Foreign Key Constraint (x5), Update Data (x4)		\N	2.0.5-ONMS20160524b
getManagePercentAvailIntfWindow	rangerrick	stored-procedures/getManagePercentAvailIntfWindow.xml	2016-10-20 10:29:00.904323-04	217	EXECUTED	3:b6bf5f1908660a31eb8da442b549f86b	Create Procedure		\N	2.0.5-ONMS20160524b
getManagePercentAvailNodeWindow	rangerrick	stored-procedures/getManagePercentAvailNodeWindow.xml	2016-10-20 10:29:00.926876-04	218	EXECUTED	3:3907f03e96d3f6dff90ca23279966059	Create Procedure		\N	2.0.5-ONMS20160524b
getManagedOutageForIntfInWindow	rangerrick	stored-procedures/getManagedOutageForIntfInWindow.xml	2016-10-20 10:29:00.948025-04	219	EXECUTED	3:b75d90a329ffa4e4799dbb37680b2bf8	Create Procedure		\N	2.0.5-ONMS20160524b
getManagedOutageForNodeInWindow	rangerrick	stored-procedures/getManagedOutageForNodeInWindow.xml	2016-10-20 10:29:00.968512-04	220	EXECUTED	3:751d4e391a4ee149a64c0339a698e86a	Create Procedure		\N	2.0.5-ONMS20160524b
getManagedServiceCountForIntf	rangerrick	stored-procedures/getManagedServiceCountForIntf.xml	2016-10-20 10:29:00.990391-04	221	EXECUTED	3:fcd88e218a8e03481b68dcdea7601409	Create Procedure		\N	2.0.5-ONMS20160524b
getManagedServiceCountForNode	rangerrick	stored-procedures/getManagedServiceCountForNode.xml	2016-10-20 10:29:01.011027-04	222	EXECUTED	3:16986bf7d9dd6e4d1e7eb13a78ccff66	Create Procedure		\N	2.0.5-ONMS20160524b
getOutageTimeInWindow	rangerrick	stored-procedures/getOutageTimeInWindow.xml	2016-10-20 10:29:01.0404-04	223	EXECUTED	3:b2ffc35efefbc53555a1df7883f678eb	Create Procedure		\N	2.0.5-ONMS20160524b
getPercentAvailabilityInWindow	rangerrick	stored-procedures/getPercentAvailabilityInWindow.xml	2016-10-20 10:29:01.062239-04	224	EXECUTED	3:5beeda598efd6771dd4263ea86be3618	Create Procedure		\N	2.0.5-ONMS20160524b
dropTriggerIfExists	rangerrick	stored-procedures/dropTriggerIfExists.xml	2016-10-20 10:29:01.084466-04	225	EXECUTED	3:3a93b4f228f38d7a72c43e90bdcdbad6	Create Procedure		\N	2.0.5-ONMS20160524b
generate_daily_series	thedesloge	stored-procedures/generate_daily_series.xml	2016-10-20 10:29:01.113554-04	226	EXECUTED	3:0d204277dd3aa194f43d827174b60797	Create a new column type., Create Procedure		\N	2.0.5-ONMS20160524b
ncs-initialize	brozow	changelog.xml	2016-10-20 10:29:01.280633-04	227	EXECUTED	3:27ec2dd430e8f29d084d6ca9b49a1b0b	Create Table (x2), Add Primary Key, Create Table, Add Primary Key, Add Foreign Key Constraint (x3)		\N	2.0.5-ONMS20160524b
ncs-unique	rangerrick	changelog.xml	2016-10-20 10:29:01.343423-04	228	EXECUTED	3:aa41f7948851d3b823fce465e6895a66	Create Procedure, Custom SQL (x2), Add Unique Constraint (x2)		\N	2.0.5-ONMS20160524b
\.


--
-- Data for Name: databasechangeloglock; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY databasechangeloglock (id, locked, lockgranted, lockedby) FROM stdin;
1	f	\N	\N
\.


--
-- Name: demandpollnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('demandpollnxtid', 1, false);


--
-- Data for Name: demandpolls; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY demandpolls (id, requesttime, username, description) FROM stdin;
\.


--
-- Data for Name: events; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY events (eventid, eventuei, nodeid, eventtime, eventhost, eventsource, ipaddr, eventsnmphost, serviceid, eventsnmp, eventparms, eventcreatetime, eventdescr, eventloggroup, eventlogmsg, eventseverity, eventpathoutage, eventcorrelation, eventsuppressedcount, eventoperinstruct, eventautoaction, eventoperaction, eventoperactionmenutext, eventnotification, eventtticket, eventtticketstate, eventforward, eventmouseovertext, eventlog, eventdisplay, eventackuser, eventacktime, alarmid, ifindex, systemid) FROM stdin;
\.


--
-- Name: eventsnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('eventsnxtid', 1, false);


--
-- Data for Name: filterfavorites; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY filterfavorites (filterid, username, filtername, page, filter) FROM stdin;
\.


--
-- Name: filternextid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('filternextid', 1, false);


--
-- Data for Name: hwentity; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY hwentity (id, parentid, nodeid, entphysicalindex, entphysicalparentrelpos, entphysicalname, entphysicaldescr, entphysicalalias, entphysicalvendortype, entphysicalclass, entphysicalmfgname, entphysicalmodelname, entphysicalhardwarerev, entphysicalfirmwarerev, entphysicalsoftwarerev, entphysicalserialnum, entphysicalassetid, entphysicalisfru, entphysicalmfgdate, entphysicaluris) FROM stdin;
\.


--
-- Data for Name: hwentityattribute; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY hwentityattribute (id, hwentityid, hwattribtypeid, attribvalue) FROM stdin;
\.


--
-- Data for Name: hwentityattributetype; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY hwentityattributetype (id, attribname, attriboid, attribclass) FROM stdin;
\.


--
-- Data for Name: ifservices; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ifservices (id, ifindex, serviceid, lastgood, lastfail, qualifier, status, source, notify, ipinterfaceid) FROM stdin;
\.


--
-- Data for Name: inventory; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY inventory (nodeid, name, createtime, lastpolltime, pathtofile, status) FROM stdin;
\.


--
-- Data for Name: ipinterface; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ipinterface (id, nodeid, ipaddr, iphostname, ismanaged, ipstatus, iplastcapsdpoll, issnmpprimary, snmpinterfaceid) FROM stdin;
\.


--
-- Data for Name: ipnettomedia; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ipnettomedia (id, netaddress, physaddress, sourcenodeid, sourceifindex, createtime, lastpolltime) FROM stdin;
\.


--
-- Data for Name: isiselement; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY isiselement (id, nodeid, isissysid, isissysadminstate, isisnodecreatetime, isisnodelastpolltime) FROM stdin;
\.


--
-- Data for Name: isislink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY isislink (id, nodeid, isiscircindex, isisisadjindex, isiscircifindex, isiscircadminstate, isisisadjstate, isisisadjneighsnpaaddress, isisisadjneighsystype, isisisadjneighsysid, isisisadjnbrextendedcircid, isislinkcreatetime, isislinklastpolltime) FROM stdin;
\.


--
-- Data for Name: lldpelement; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY lldpelement (id, nodeid, lldpchassisid, lldpchassisidsubtype, lldpsysname, lldpnodecreatetime, lldpnodelastpolltime) FROM stdin;
\.


--
-- Data for Name: lldplink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY lldplink (id, nodeid, lldplocalportnum, lldpportid, lldpportidsubtype, lldpportdescr, lldpportifindex, lldpremchassisid, lldpremchassisidsubtype, lldpremsysname, lldpremportid, lldpremportidsubtype, lldpremportdescr, lldplinkcreatetime, lldplinklastpolltime) FROM stdin;
\.


--
-- Data for Name: location_specific_status_changes; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY location_specific_status_changes (id, ifserviceid, statuscode, statustime, statusreason, responsetime, systemid) FROM stdin;
\.


--
-- Name: memonxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('memonxtid', 1, false);


--
-- Data for Name: memos; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY memos (id, created, updated, author, body, reductionkey, type) FROM stdin;
\.


--
-- Data for Name: monitoringlocations; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY monitoringlocations (id, monitoringarea, geolocation, latitude, longitude, priority) FROM stdin;
Default	localhost	\N	\N	\N	\N
\.


--
-- Data for Name: monitoringlocationscollectionpackages; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY monitoringlocationscollectionpackages (monitoringlocationid, packagename) FROM stdin;
\.


--
-- Data for Name: monitoringlocationspollingpackages; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY monitoringlocationspollingpackages (monitoringlocationid, packagename) FROM stdin;
\.


--
-- Data for Name: monitoringlocationstags; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY monitoringlocationstags (monitoringlocationid, tag) FROM stdin;
\.


--
-- Data for Name: monitoringsystems; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY monitoringsystems (id, label, location, type, status, last_updated) FROM stdin;
00000000-0000-0000-0000-000000000000	localhost	Default	OpenNMS	\N	\N
\.


--
-- Data for Name: monitoringsystemsproperties; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY monitoringsystemsproperties (monitoringsystemid, property, propertyvalue) FROM stdin;
\.


--
-- Data for Name: ncs_attributes; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ncs_attributes (ncscomponent_id, key, value) FROM stdin;
\.


--
-- Data for Name: ncscomponent; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ncscomponent (id, version, name, type, foreignsource, foreignid, depsrequired, nodeforeignsource, nodeforeignid, upeventuei, downeventuei) FROM stdin;
\.


--
-- Data for Name: node; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY node (nodeid, nodecreatetime, nodeparentid, nodetype, nodesysoid, nodesysname, nodesysdescription, nodesyslocation, nodesyscontact, nodelabel, nodelabelsource, nodenetbiosname, nodedomainname, operatingsystem, lastcapsdpoll, foreignsource, foreignid, location) FROM stdin;
\.


--
-- Name: nodenxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('nodenxtid', 1, false);


--
-- Data for Name: notifications; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY notifications (notifyid, textmsg, subject, numericmsg, pagetime, respondtime, answeredby, nodeid, interfaceid, serviceid, queueid, eventid, eventuei, notifconfigname) FROM stdin;
\.


--
-- Name: notifynxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('notifynxtid', 1, false);


--
-- Name: opennmsnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('opennmsnxtid', 1, false);


--
-- Data for Name: ospfelement; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ospfelement (id, nodeid, ospfrouterid, ospfadminstat, ospfversionnumber, ospfbdrrtrstatus, ospfasbdrrtrstatus, ospfrouteridnetmask, ospfrouteridifindex, ospfnodecreatetime, ospfnodelastpolltime) FROM stdin;
\.


--
-- Data for Name: ospflink; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY ospflink (id, nodeid, ospfipaddr, ospfipmask, ospfaddresslessindex, ospfifindex, ospfremrouterid, ospfremipaddr, ospfremaddresslessindex, ospflinkcreatetime, ospflinklastpolltime) FROM stdin;
\.


--
-- Name: outagenxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('outagenxtid', 1, false);


--
-- Data for Name: outages; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY outages (outageid, svclosteventid, svcregainedeventid, iflostservice, ifregainedservice, suppresstime, suppressedby, ifserviceid) FROM stdin;
\.


--
-- Data for Name: pathoutage; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY pathoutage (nodeid, criticalpathip, criticalpathservicename) FROM stdin;
\.


--
-- Name: pollresultnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('pollresultnxtid', 1, false);


--
-- Data for Name: pollresults; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY pollresults (id, pollid, nodeid, ipaddr, ifindex, serviceid, statuscode, statusname, reason) FROM stdin;
\.


--
-- Data for Name: qrtz_blob_triggers; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_blob_triggers (trigger_name, trigger_group, blob_data) FROM stdin;
\.


--
-- Data for Name: qrtz_calendars; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_calendars (calendar_name, calendar) FROM stdin;
\.


--
-- Data for Name: qrtz_cron_triggers; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_cron_triggers (trigger_name, trigger_group, cron_expression, time_zone_id) FROM stdin;
\.


--
-- Data for Name: qrtz_fired_triggers; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_fired_triggers (entry_id, trigger_name, trigger_group, is_volatile, instance_name, fired_time, state, job_name, job_group, is_stateful, requests_recovery, priority) FROM stdin;
\.


--
-- Data for Name: qrtz_job_details; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_job_details (job_name, job_group, description, job_class_name, is_durable, is_volatile, is_stateful, requests_recovery, job_data) FROM stdin;
\.


--
-- Data for Name: qrtz_job_listeners; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_job_listeners (job_name, job_group, job_listener) FROM stdin;
\.


--
-- Data for Name: qrtz_locks; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_locks (lock_name) FROM stdin;
TRIGGER_ACCESS
JOB_ACCESS
CALENDAR_ACCESS
STATE_ACCESS
MISFIRE_ACCESS
\.


--
-- Data for Name: qrtz_paused_trigger_grps; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_paused_trigger_grps (trigger_group) FROM stdin;
\.


--
-- Data for Name: qrtz_scheduler_state; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_scheduler_state (instance_name, last_checkin_time, checkin_interval, recoverer) FROM stdin;
\.


--
-- Data for Name: qrtz_simple_triggers; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_simple_triggers (trigger_name, trigger_group, repeat_count, repeat_interval, times_triggered) FROM stdin;
\.


--
-- Data for Name: qrtz_trigger_listeners; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_trigger_listeners (trigger_name, trigger_group, trigger_listener) FROM stdin;
\.


--
-- Data for Name: qrtz_triggers; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY qrtz_triggers (trigger_name, trigger_group, job_name, job_group, is_volatile, description, next_fire_time, prev_fire_time, trigger_state, trigger_type, start_time, end_time, calendar_name, misfire_instr, job_data, priority) FROM stdin;
\.


--
-- Data for Name: reportcatalog; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY reportcatalog (id, reportid, title, date, location) FROM stdin;
\.


--
-- Name: reportcatalognxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('reportcatalognxtid', 1, false);


--
-- Data for Name: requisitioned_categories; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY requisitioned_categories (id, nodeid, categoryid) FROM stdin;
\.


--
-- Data for Name: resourcereference; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY resourcereference (id, resourceid) FROM stdin;
\.


--
-- Data for Name: scanreportlogs; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY scanreportlogs (scanreportid, logtext) FROM stdin;
\.


--
-- Data for Name: scanreportpollresults; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY scanreportpollresults (id, scanreportid, servicename, serviceid, nodelabel, nodeid, ipaddress, statusreason, responsetime, statuscode, statustime) FROM stdin;
\.


--
-- Data for Name: scanreportproperties; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY scanreportproperties (scanreportid, property, propertyvalue) FROM stdin;
\.


--
-- Data for Name: scanreports; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY scanreports (id, location, locale, "timestamp") FROM stdin;
\.


--
-- Data for Name: service; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY service (serviceid, servicename) FROM stdin;
\.


--
-- Data for Name: servicemap; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY servicemap (ipaddr, servicemapname) FROM stdin;
\.


--
-- Name: servicenxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('servicenxtid', 1, false);


--
-- Data for Name: snmpinterface; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY snmpinterface (id, nodeid, snmpipadentnetmask, snmpphysaddr, snmpifindex, snmpifdescr, snmpiftype, snmpifname, snmpifspeed, snmpifadminstatus, snmpifoperstatus, snmpifalias, snmpcollect, snmplastcapsdpoll, snmppoll, snmplastsnmppoll) FROM stdin;
\.


--
-- Data for Name: statisticsreport; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY statisticsreport (id, startdate, enddate, name, description, jobstarteddate, jobcompleteddate, purgedate) FROM stdin;
\.


--
-- Data for Name: statisticsreportdata; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY statisticsreportdata (id, reportid, resourceid, value) FROM stdin;
\.


--
-- Data for Name: subcomponents; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY subcomponents (component_id, subcomponent_id) FROM stdin;
\.


--
-- Name: usernotifnxtid; Type: SEQUENCE SET; Schema: public; Owner: opennms
--

SELECT pg_catalog.setval('usernotifnxtid', 1, false);


--
-- Data for Name: usersnotified; Type: TABLE DATA; Schema: public; Owner: opennms
--

COPY usersnotified (id, userid, notifyid, notifytime, media, contactinfo, autonotify) FROM stdin;
\.


--
-- Name: applications_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY applications
    ADD CONSTRAINT applications_pkey PRIMARY KEY (id);


--
-- Name: bsm_map_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_map
    ADD CONSTRAINT bsm_map_pkey PRIMARY KEY (id);


--
-- Name: bsm_reduce_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_reduce
    ADD CONSTRAINT bsm_reduce_pkey PRIMARY KEY (id);


--
-- Name: bsm_service_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_attributes
    ADD CONSTRAINT bsm_service_attributes_pkey PRIMARY KEY (bsm_service_id, key);


--
-- Name: bsm_service_children_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_children
    ADD CONSTRAINT bsm_service_children_pkey PRIMARY KEY (id);


--
-- Name: bsm_service_edge_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_edge
    ADD CONSTRAINT bsm_service_edge_pkey PRIMARY KEY (id);


--
-- Name: bsm_service_ifservices_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_ifservices
    ADD CONSTRAINT bsm_service_ifservices_pkey PRIMARY KEY (id);


--
-- Name: bsm_service_name_key; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service
    ADD CONSTRAINT bsm_service_name_key UNIQUE (name);


--
-- Name: bsm_service_reductionkeys_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_reductionkeys
    ADD CONSTRAINT bsm_service_reductionkeys_pkey PRIMARY KEY (id);


--
-- Name: bsm_services_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service
    ADD CONSTRAINT bsm_services_pkey PRIMARY KEY (id);


--
-- Name: category_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY categories
    ADD CONSTRAINT category_pkey PRIMARY KEY (categoryid);


--
-- Name: demandpoll_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY demandpolls
    ADD CONSTRAINT demandpoll_pkey PRIMARY KEY (id);


--
-- Name: ifservices_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ifservices
    ADD CONSTRAINT ifservices_pkey PRIMARY KEY (id);


--
-- Name: ipinterface_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ipinterface
    ADD CONSTRAINT ipinterface_pkey PRIMARY KEY (id);


--
-- Name: location_specific_status_changes_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY location_specific_status_changes
    ADD CONSTRAINT location_specific_status_changes_pkey PRIMARY KEY (id);


--
-- Name: memos_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY memos
    ADD CONSTRAINT memos_pkey PRIMARY KEY (id);


--
-- Name: monitoringlocations_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY monitoringlocations
    ADD CONSTRAINT monitoringlocations_pkey PRIMARY KEY (id);


--
-- Name: monitoringsystems_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY monitoringsystems
    ADD CONSTRAINT monitoringsystems_pkey PRIMARY KEY (id);


--
-- Name: ncs_attributes_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ncs_attributes
    ADD CONSTRAINT ncs_attributes_pkey PRIMARY KEY (ncscomponent_id, key);


--
-- Name: ncscomponent_type_foreignsource_foreignid_key; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ncscomponent
    ADD CONSTRAINT ncscomponent_type_foreignsource_foreignid_key UNIQUE (type, foreignsource, foreignid);


--
-- Name: pk_accessLocks; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY accesslocks
    ADD CONSTRAINT "pk_accessLocks" PRIMARY KEY (lockname);


--
-- Name: pk_acks; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY acks
    ADD CONSTRAINT pk_acks PRIMARY KEY (id);


--
-- Name: pk_alarmid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY alarms
    ADD CONSTRAINT pk_alarmid PRIMARY KEY (alarmid);


--
-- Name: pk_assetid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY assets
    ADD CONSTRAINT pk_assetid PRIMARY KEY (id);


--
-- Name: pk_bridgebridgelink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgebridgelink
    ADD CONSTRAINT pk_bridgebridgelink PRIMARY KEY (id);


--
-- Name: pk_bridgeelement; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgeelement
    ADD CONSTRAINT pk_bridgeelement PRIMARY KEY (id);


--
-- Name: pk_bridgemaclink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgemaclink
    ADD CONSTRAINT pk_bridgemaclink PRIMARY KEY (id);


--
-- Name: pk_bridgestplink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgestplink
    ADD CONSTRAINT pk_bridgestplink PRIMARY KEY (id);


--
-- Name: pk_cdpelement; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY cdpelement
    ADD CONSTRAINT pk_cdpelement PRIMARY KEY (id);


--
-- Name: pk_cdplink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY cdplink
    ADD CONSTRAINT pk_cdplink PRIMARY KEY (id);


--
-- Name: pk_databasechangelog; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY databasechangelog
    ADD CONSTRAINT pk_databasechangelog PRIMARY KEY (id, author, filename);


--
-- Name: pk_databasechangeloglock; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY databasechangeloglock
    ADD CONSTRAINT pk_databasechangeloglock PRIMARY KEY (id);


--
-- Name: pk_eventid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY events
    ADD CONSTRAINT pk_eventid PRIMARY KEY (eventid);


--
-- Name: pk_filterid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY filterfavorites
    ADD CONSTRAINT pk_filterid PRIMARY KEY (filterid);


--
-- Name: pk_hwentity; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentity
    ADD CONSTRAINT pk_hwentity PRIMARY KEY (id);


--
-- Name: pk_hwentityattribute; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentityattribute
    ADD CONSTRAINT pk_hwentityattribute PRIMARY KEY (id);


--
-- Name: pk_hwentityattributetype; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentityattributetype
    ADD CONSTRAINT pk_hwentityattributetype PRIMARY KEY (id);


--
-- Name: pk_ipnettomedia; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ipnettomedia
    ADD CONSTRAINT pk_ipnettomedia PRIMARY KEY (id);


--
-- Name: pk_isiselement; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY isiselement
    ADD CONSTRAINT pk_isiselement PRIMARY KEY (id);


--
-- Name: pk_isislink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY isislink
    ADD CONSTRAINT pk_isislink PRIMARY KEY (id);


--
-- Name: pk_lldpelement; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY lldpelement
    ADD CONSTRAINT pk_lldpelement PRIMARY KEY (id);


--
-- Name: pk_lldplink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY lldplink
    ADD CONSTRAINT pk_lldplink PRIMARY KEY (id);


--
-- Name: pk_ncsid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ncscomponent
    ADD CONSTRAINT pk_ncsid PRIMARY KEY (id);


--
-- Name: pk_nodeid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY node
    ADD CONSTRAINT pk_nodeid PRIMARY KEY (nodeid);


--
-- Name: pk_notifyid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY notifications
    ADD CONSTRAINT pk_notifyid PRIMARY KEY (notifyid);


--
-- Name: pk_ospfelement; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ospfelement
    ADD CONSTRAINT pk_ospfelement PRIMARY KEY (id);


--
-- Name: pk_ospflink; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ospflink
    ADD CONSTRAINT pk_ospflink PRIMARY KEY (id);


--
-- Name: pk_outageid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY outages
    ADD CONSTRAINT pk_outageid PRIMARY KEY (outageid);


--
-- Name: pk_physaddr; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY accesspoints
    ADD CONSTRAINT pk_physaddr PRIMARY KEY (physaddr);


--
-- Name: pk_qrtz_blob_triggers; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_blob_triggers
    ADD CONSTRAINT pk_qrtz_blob_triggers PRIMARY KEY (trigger_name, trigger_group);


--
-- Name: pk_qrtz_calendars; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_calendars
    ADD CONSTRAINT pk_qrtz_calendars PRIMARY KEY (calendar_name);


--
-- Name: pk_qrtz_cron_triggers; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_cron_triggers
    ADD CONSTRAINT pk_qrtz_cron_triggers PRIMARY KEY (trigger_name, trigger_group);


--
-- Name: pk_qrtz_fired_triggers; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_fired_triggers
    ADD CONSTRAINT pk_qrtz_fired_triggers PRIMARY KEY (entry_id);


--
-- Name: pk_qrtz_job_listeners; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_job_listeners
    ADD CONSTRAINT pk_qrtz_job_listeners PRIMARY KEY (job_name, job_group, job_listener);


--
-- Name: pk_qrtz_locks; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_locks
    ADD CONSTRAINT pk_qrtz_locks PRIMARY KEY (lock_name);


--
-- Name: pk_qrtz_paused_trigger_grps; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_paused_trigger_grps
    ADD CONSTRAINT pk_qrtz_paused_trigger_grps PRIMARY KEY (trigger_group);


--
-- Name: pk_qrtz_scheduler_state; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_scheduler_state
    ADD CONSTRAINT pk_qrtz_scheduler_state PRIMARY KEY (instance_name);


--
-- Name: pk_qrtz_simple_triggers; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_simple_triggers
    ADD CONSTRAINT pk_qrtz_simple_triggers PRIMARY KEY (trigger_name, trigger_group);


--
-- Name: pk_qrtz_trigger_listeners; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_trigger_listeners
    ADD CONSTRAINT pk_qrtz_trigger_listeners PRIMARY KEY (trigger_name, trigger_group, trigger_listener);


--
-- Name: pk_qrtz_triggers; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_triggers
    ADD CONSTRAINT pk_qrtz_triggers PRIMARY KEY (trigger_name, trigger_group);


--
-- Name: pk_requisitioned_categories; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY requisitioned_categories
    ADD CONSTRAINT pk_requisitioned_categories PRIMARY KEY (id);


--
-- Name: pk_resourcereference_id; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY resourcereference
    ADD CONSTRAINT pk_resourcereference_id PRIMARY KEY (id);


--
-- Name: pk_serviceid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY service
    ADD CONSTRAINT pk_serviceid PRIMARY KEY (serviceid);


--
-- Name: pk_statisticsreport_id; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY statisticsreport
    ADD CONSTRAINT pk_statisticsreport_id PRIMARY KEY (id);


--
-- Name: pk_statsdata_id; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY statisticsreportdata
    ADD CONSTRAINT pk_statsdata_id PRIMARY KEY (id);


--
-- Name: pk_usernotificationid; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY usersnotified
    ADD CONSTRAINT pk_usernotificationid PRIMARY KEY (id);


--
-- Name: pollresult_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY pollresults
    ADD CONSTRAINT pollresult_pkey PRIMARY KEY (id);


--
-- Name: qrtz_job_details_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_job_details
    ADD CONSTRAINT qrtz_job_details_pkey PRIMARY KEY (job_name, job_group);


--
-- Name: scanreportlogs_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreportlogs
    ADD CONSTRAINT scanreportlogs_pkey PRIMARY KEY (scanreportid);


--
-- Name: scanreportpollresults_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreportpollresults
    ADD CONSTRAINT scanreportpollresults_pkey PRIMARY KEY (id);


--
-- Name: scanreports_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreports
    ADD CONSTRAINT scanreports_pkey PRIMARY KEY (id);


--
-- Name: service_servicename_key; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY service
    ADD CONSTRAINT service_servicename_key UNIQUE (servicename);


--
-- Name: snmpinterface_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY snmpinterface
    ADD CONSTRAINT snmpinterface_pkey PRIMARY KEY (id);


--
-- Name: subcomponents_component_id_subcomponent_id_key; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY subcomponents
    ADD CONSTRAINT subcomponents_component_id_subcomponent_id_key UNIQUE (component_id, subcomponent_id);


--
-- Name: subcomponents_pkey; Type: CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY subcomponents
    ADD CONSTRAINT subcomponents_pkey PRIMARY KEY (component_id, subcomponent_id);


--
-- Name: accesspoint_package_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX accesspoint_package_idx ON accesspoints USING btree (pollingpackage);


--
-- Name: ack_time_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ack_time_idx ON acks USING btree (acktime);


--
-- Name: ack_user_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ack_user_idx ON acks USING btree (ackuser);


--
-- Name: alarm_app_dn; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_app_dn ON alarms USING btree (applicationdn);


--
-- Name: alarm_attributes_aan_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX alarm_attributes_aan_idx ON alarm_attributes USING btree (alarmid, attributename);


--
-- Name: alarm_attributes_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_attributes_idx ON alarm_attributes USING btree (alarmid);


--
-- Name: alarm_clearkey_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_clearkey_idx ON alarms USING btree (clearkey);


--
-- Name: alarm_eventid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_eventid_idx ON alarms USING btree (lasteventid);


--
-- Name: alarm_lasteventtime_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_lasteventtime_idx ON alarms USING btree (lasteventtime);


--
-- Name: alarm_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_nodeid_idx ON alarms USING btree (nodeid);


--
-- Name: alarm_oss_primary_key; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_oss_primary_key ON alarms USING btree (ossprimarykey);


--
-- Name: alarm_reduction_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_reduction_idx ON alarms USING btree (alarmid, eventuei, systemid, nodeid, serviceid, reductionkey);


--
-- Name: alarm_reductionkey_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX alarm_reductionkey_idx ON alarms USING btree (reductionkey);


--
-- Name: alarm_uei_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX alarm_uei_idx ON alarms USING btree (eventuei);


--
-- Name: appid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX appid_idx ON application_service_map USING btree (appid);


--
-- Name: appid_ifserviceid_idex; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX appid_ifserviceid_idex ON application_service_map USING btree (appid, ifserviceid);


--
-- Name: applications_name_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX applications_name_idx ON applications USING btree (name);


--
-- Name: assets_an_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX assets_an_idx ON assets USING btree (assetnumber);


--
-- Name: assets_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX assets_nodeid_idx ON assets USING btree (nodeid);


--
-- Name: bridgebridgelink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgebridgelink_lastpoll_idx ON bridgebridgelink USING btree (bridgebridgelinklastpolltime);


--
-- Name: bridgebridgelink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgebridgelink_nodeid_idx ON bridgebridgelink USING btree (nodeid);


--
-- Name: bridgebridgelink_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgebridgelink_pk_idx ON bridgebridgelink USING btree (nodeid, bridgeport);


--
-- Name: bridgeelement_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgeelement_lastpoll_idx ON bridgeelement USING btree (bridgenodelastpolltime);


--
-- Name: bridgeelement_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgeelement_nodeid_idx ON bridgeelement USING btree (nodeid);


--
-- Name: bridgeelement_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgeelement_pk_idx ON bridgeelement USING btree (nodeid, basebridgeaddress);


--
-- Name: bridgemaclink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgemaclink_lastpoll_idx ON bridgemaclink USING btree (bridgemaclinklastpolltime);


--
-- Name: bridgemaclink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgemaclink_nodeid_idx ON bridgemaclink USING btree (nodeid);


--
-- Name: bridgemaclink_pk_idx1; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgemaclink_pk_idx1 ON bridgemaclink USING btree (nodeid, bridgeport);


--
-- Name: bridgemaclink_pk_idx2; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgemaclink_pk_idx2 ON bridgemaclink USING btree (macaddress);


--
-- Name: bridgestplink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgestplink_lastpoll_idx ON bridgestplink USING btree (bridgestplinklastpolltime);


--
-- Name: bridgestplink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgestplink_nodeid_idx ON bridgestplink USING btree (nodeid);


--
-- Name: bridgestplink_pk_idx1; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgestplink_pk_idx1 ON bridgestplink USING btree (nodeid, stpport);


--
-- Name: bridgestplink_pk_idx2; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX bridgestplink_pk_idx2 ON bridgestplink USING btree (designatedbridge);


--
-- Name: category_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX category_idx ON categories USING btree (categoryname);


--
-- Name: catenode_unique_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX catenode_unique_idx ON category_node USING btree (categoryid, nodeid);


--
-- Name: catgroup_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX catgroup_idx ON category_group USING btree (groupid);


--
-- Name: catgroup_unique_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX catgroup_unique_idx ON category_group USING btree (categoryid, groupid);


--
-- Name: catid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX catid_idx ON category_node USING btree (categoryid);


--
-- Name: catid_idx3; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX catid_idx3 ON category_group USING btree (categoryid);


--
-- Name: catnode_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX catnode_idx ON category_node USING btree (nodeid);


--
-- Name: cdp_deviceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdp_deviceid_idx ON cdpelement USING btree (cdpglobaldeviceid);


--
-- Name: cdpelement_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdpelement_nodeid_idx ON cdpelement USING btree (nodeid);


--
-- Name: cdplink_address_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdplink_address_idx ON cdplink USING btree (cdpcacheaddress);


--
-- Name: cdplink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdplink_lastpoll_idx ON cdplink USING btree (cdplinklastpolltime);


--
-- Name: cdplink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdplink_nodeid_idx ON cdplink USING btree (nodeid);


--
-- Name: cdplink_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdplink_pk_idx ON cdplink USING btree (nodeid, cdpcachedeviceid, cdpcachedeviceport);


--
-- Name: cdplink_pk_new_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX cdplink_pk_new_idx ON cdplink USING btree (nodeid, cdpcacheifindex, cdpcachedeviceindex);


--
-- Name: demandpoll_request_time; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX demandpoll_request_time ON demandpolls USING btree (requesttime);


--
-- Name: events_acktime_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_acktime_idx ON events USING btree (eventacktime);


--
-- Name: events_ackuser_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_ackuser_idx ON events USING btree (eventackuser);


--
-- Name: events_alarmid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_alarmid_idx ON events USING btree (alarmid);


--
-- Name: events_display_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_display_idx ON events USING btree (eventdisplay);


--
-- Name: events_ipaddr_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_ipaddr_idx ON events USING btree (ipaddr);


--
-- Name: events_log_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_log_idx ON events USING btree (eventlog);


--
-- Name: events_nodeid_display_ackuser; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_nodeid_display_ackuser ON events USING btree (nodeid, eventdisplay, eventackuser);


--
-- Name: events_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_nodeid_idx ON events USING btree (nodeid);


--
-- Name: events_serviceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_serviceid_idx ON events USING btree (serviceid);


--
-- Name: events_severity_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_severity_idx ON events USING btree (eventseverity);


--
-- Name: events_time_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_time_idx ON events USING btree (eventtime);


--
-- Name: events_uei_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX events_uei_idx ON events USING btree (eventuei);


--
-- Name: filternamesidx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX filternamesidx ON filterfavorites USING btree (username, filtername, page);


--
-- Name: hwentity_entphysicalindex_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX hwentity_entphysicalindex_idx ON hwentity USING btree (entphysicalindex);


--
-- Name: hwentity_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX hwentity_nodeid_idx ON hwentity USING btree (nodeid);


--
-- Name: hwentityattribute_unique_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX hwentityattribute_unique_idx ON hwentityattribute USING btree (hwentityid, hwattribtypeid);


--
-- Name: hwentityattributetype_unique_name_dx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX hwentityattributetype_unique_name_dx ON hwentityattributetype USING btree (attribname);


--
-- Name: hwentityattributetype_unique_oid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX hwentityattributetype_unique_oid_idx ON hwentityattributetype USING btree (attriboid);


--
-- Name: ifserviceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ifserviceid_idx ON application_service_map USING btree (ifserviceid);


--
-- Name: ifservices_ipinterfaceid_status; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ifservices_ipinterfaceid_status ON ifservices USING btree (ipinterfaceid, status);


--
-- Name: ifservices_ipinterfaceid_svc_unique; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX ifservices_ipinterfaceid_svc_unique ON ifservices USING btree (ipinterfaceid, serviceid);


--
-- Name: ifservices_serviceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ifservices_serviceid_idx ON ifservices USING btree (serviceid);


--
-- Name: ifservicves_ipinterfaceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ifservicves_ipinterfaceid_idx ON ifservices USING btree (ipinterfaceid);


--
-- Name: inventory_lastpolltime_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX inventory_lastpolltime_idx ON inventory USING btree (lastpolltime);


--
-- Name: inventory_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX inventory_nodeid_idx ON inventory USING btree (nodeid);


--
-- Name: inventory_nodeid_name_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX inventory_nodeid_name_idx ON inventory USING btree (nodeid, name);


--
-- Name: inventory_status_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX inventory_status_idx ON inventory USING btree (status);


--
-- Name: ipinterface_ipaddr_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipinterface_ipaddr_idx ON ipinterface USING btree (ipaddr);


--
-- Name: ipinterface_ipaddr_ismanaged_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipinterface_ipaddr_ismanaged_idx ON ipinterface USING btree (ipaddr, ismanaged);


--
-- Name: ipinterface_ismanaged_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipinterface_ismanaged_idx ON ipinterface USING btree (ismanaged);


--
-- Name: ipinterface_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipinterface_nodeid_idx ON ipinterface USING btree (nodeid);


--
-- Name: ipinterface_nodeid_ipaddr_ismanaged_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipinterface_nodeid_ipaddr_ismanaged_idx ON ipinterface USING btree (nodeid, ipaddr, ismanaged);


--
-- Name: ipinterface_nodeid_ipaddr_notzero_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX ipinterface_nodeid_ipaddr_notzero_idx ON ipinterface USING btree (nodeid, ipaddr);


--
-- Name: ipinterface_snmpinterfaceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipinterface_snmpinterfaceid_idx ON ipinterface USING btree (snmpinterfaceid);


--
-- Name: ipnettomedia_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipnettomedia_lastpoll_idx ON ipnettomedia USING btree (lastpolltime);


--
-- Name: ipnettomedia_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipnettomedia_nodeid_idx ON ipnettomedia USING btree (sourcenodeid);


--
-- Name: ipnettomedia_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ipnettomedia_pk_idx ON ipnettomedia USING btree (netaddress, physaddress);


--
-- Name: isiselement_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX isiselement_nodeid_idx ON isiselement USING btree (nodeid);


--
-- Name: isiselement_sysid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX isiselement_sysid_idx ON isiselement USING btree (isissysid);


--
-- Name: isislink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX isislink_lastpoll_idx ON isislink USING btree (isislinklastpolltime);


--
-- Name: isislink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX isislink_nodeid_idx ON isislink USING btree (nodeid);


--
-- Name: isislink_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX isislink_pk_idx ON isislink USING btree (nodeid, isiscircindex, isisisadjindex);


--
-- Name: lldp_chassis_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX lldp_chassis_idx ON lldpelement USING btree (lldpchassisid);


--
-- Name: lldp_sysname_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX lldp_sysname_idx ON lldpelement USING btree (lldpsysname);


--
-- Name: lldpelement_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX lldpelement_nodeid_idx ON lldpelement USING btree (nodeid);


--
-- Name: lldplink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX lldplink_lastpoll_idx ON lldplink USING btree (lldplinklastpolltime);


--
-- Name: lldplink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX lldplink_nodeid_idx ON lldplink USING btree (nodeid);


--
-- Name: lldplink_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX lldplink_pk_idx ON lldplink USING btree (nodeid, lldplocalportnum);


--
-- Name: location_specific_status_changes_ifserviceid; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX location_specific_status_changes_ifserviceid ON location_specific_status_changes USING btree (ifserviceid);


--
-- Name: location_specific_status_changes_statustime; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX location_specific_status_changes_statustime ON location_specific_status_changes USING btree (statustime);


--
-- Name: location_specific_status_changes_systemid; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX location_specific_status_changes_systemid ON location_specific_status_changes USING btree (systemid);


--
-- Name: location_specific_status_changes_systemid_if_time; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX location_specific_status_changes_systemid_if_time ON location_specific_status_changes USING btree (systemid, ifserviceid, statustime);


--
-- Name: location_specific_status_changes_systemid_ifserviceid; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX location_specific_status_changes_systemid_ifserviceid ON location_specific_status_changes USING btree (systemid, ifserviceid);


--
-- Name: monitoringlocationscollectionpackages_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX monitoringlocationscollectionpackages_id_idx ON monitoringlocationscollectionpackages USING btree (monitoringlocationid);


--
-- Name: monitoringlocationscollectionpackages_id_pkg_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX monitoringlocationscollectionpackages_id_pkg_idx ON monitoringlocationscollectionpackages USING btree (monitoringlocationid, packagename);


--
-- Name: monitoringlocationspollingpackages_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX monitoringlocationspollingpackages_id_idx ON monitoringlocationspollingpackages USING btree (monitoringlocationid);


--
-- Name: monitoringlocationspollingpackages_id_pkg_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX monitoringlocationspollingpackages_id_pkg_idx ON monitoringlocationspollingpackages USING btree (monitoringlocationid, packagename);


--
-- Name: monitoringlocationstags_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX monitoringlocationstags_id_idx ON monitoringlocationstags USING btree (monitoringlocationid);


--
-- Name: monitoringlocationstags_id_pkg_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX monitoringlocationstags_id_pkg_idx ON monitoringlocationstags USING btree (monitoringlocationid, tag);


--
-- Name: monitoringsystemsproperties_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX monitoringsystemsproperties_id_idx ON monitoringsystemsproperties USING btree (monitoringsystemid);


--
-- Name: monitoringsystemsproperties_id_property_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX monitoringsystemsproperties_id_property_idx ON monitoringsystemsproperties USING btree (monitoringsystemid, property);


--
-- Name: node_foreign_unique_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX node_foreign_unique_idx ON node USING btree (foreignsource, foreignid);


--
-- Name: node_id_type_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX node_id_type_idx ON node USING btree (nodeid, nodetype);


--
-- Name: node_label_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX node_label_idx ON node USING btree (nodelabel);


--
-- Name: node_sysdescr_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX node_sysdescr_idx ON node USING btree (nodesysdescription);


--
-- Name: node_sysoid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX node_sysoid_idx ON node USING btree (nodesysoid);


--
-- Name: notifications_answeredby_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_answeredby_idx ON notifications USING btree (answeredby);


--
-- Name: notifications_eventid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_eventid_idx ON notifications USING btree (eventid);


--
-- Name: notifications_eventuei_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_eventuei_idx ON notifications USING btree (eventuei);


--
-- Name: notifications_interfaceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_interfaceid_idx ON notifications USING btree (interfaceid);


--
-- Name: notifications_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_nodeid_idx ON notifications USING btree (nodeid);


--
-- Name: notifications_respondtime_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_respondtime_idx ON notifications USING btree (respondtime);


--
-- Name: notifications_serviceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX notifications_serviceid_idx ON notifications USING btree (serviceid);


--
-- Name: one_outstanding_outage_per_service_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX one_outstanding_outage_per_service_idx ON outages USING btree (ifserviceid) WHERE (ifregainedservice IS NULL);


--
-- Name: ospfelement_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ospfelement_nodeid_idx ON ospfelement USING btree (nodeid);


--
-- Name: ospfelement_routerid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ospfelement_routerid_idx ON ospfelement USING btree (ospfrouterid);


--
-- Name: ospflink_lastpoll_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ospflink_lastpoll_idx ON ospflink USING btree (ospflinklastpolltime);


--
-- Name: ospflink_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ospflink_nodeid_idx ON ospflink USING btree (nodeid);


--
-- Name: ospflink_pk_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX ospflink_pk_idx ON ospflink USING btree (nodeid, ospfremrouterid, ospfremipaddr, ospfremaddresslessindex);


--
-- Name: outages_ifserviceid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX outages_ifserviceid_idx ON outages USING btree (ifserviceid);


--
-- Name: outages_regainedservice_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX outages_regainedservice_idx ON outages USING btree (ifregainedservice);


--
-- Name: outages_svclostid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX outages_svclostid_idx ON outages USING btree (svclosteventid);


--
-- Name: outages_svcregainedid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX outages_svcregainedid_idx ON outages USING btree (svcregainedeventid);


--
-- Name: pathoutage_criticalpathip; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX pathoutage_criticalpathip ON pathoutage USING btree (criticalpathip);


--
-- Name: pathoutage_criticalpathservicename_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX pathoutage_criticalpathservicename_idx ON pathoutage USING btree (criticalpathservicename);


--
-- Name: pathoutage_nodeid; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX pathoutage_nodeid ON pathoutage USING btree (nodeid);


--
-- Name: pollresults_poll_id; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX pollresults_poll_id ON pollresults USING btree (pollid);


--
-- Name: pollresults_service; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX pollresults_service ON pollresults USING btree (nodeid, ipaddr, ifindex, serviceid);


--
-- Name: requisitioned_category_node_unique_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX requisitioned_category_node_unique_idx ON requisitioned_categories USING btree (nodeid, categoryid);


--
-- Name: resourcereference_resourceid; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX resourcereference_resourceid ON resourcereference USING btree (resourceid);


--
-- Name: scanreportlogs_scanreportid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX scanreportlogs_scanreportid_idx ON scanreportlogs USING btree (scanreportid);


--
-- Name: scanreportpollresults_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX scanreportpollresults_id_idx ON scanreportpollresults USING btree (id);


--
-- Name: scanreportpollresults_id_scanreportid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX scanreportpollresults_id_scanreportid_idx ON scanreportpollresults USING btree (id, scanreportid);


--
-- Name: scanreportproperties_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX scanreportproperties_id_idx ON scanreportproperties USING btree (scanreportid);


--
-- Name: scanreportproperties_id_property_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX scanreportproperties_id_property_idx ON scanreportproperties USING btree (scanreportid, property);


--
-- Name: scanreports_id_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX scanreports_id_idx ON scanreports USING btree (id);


--
-- Name: servicemap_ipaddr_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX servicemap_ipaddr_idx ON servicemap USING btree (ipaddr);


--
-- Name: servicemap_name_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX servicemap_name_idx ON servicemap USING btree (servicemapname);


--
-- Name: snmpinterface_nodeid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX snmpinterface_nodeid_idx ON snmpinterface USING btree (nodeid);


--
-- Name: snmpinterface_nodeid_ifindex_unique_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX snmpinterface_nodeid_ifindex_unique_idx ON snmpinterface USING btree (nodeid, snmpifindex);


--
-- Name: statisticsreport_name; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX statisticsreport_name ON statisticsreport USING btree (name);


--
-- Name: statisticsreport_purgedate; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX statisticsreport_purgedate ON statisticsreport USING btree (purgedate);


--
-- Name: statisticsreport_startdate; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX statisticsreport_startdate ON statisticsreport USING btree (startdate);


--
-- Name: statsdata_unique; Type: INDEX; Schema: public; Owner: opennms
--

CREATE UNIQUE INDEX statsdata_unique ON statisticsreportdata USING btree (reportid, resourceid);


--
-- Name: userid_notifyid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX userid_notifyid_idx ON usersnotified USING btree (userid, notifyid);


--
-- Name: usersnotified_notifyid_idx; Type: INDEX; Schema: public; Owner: opennms
--

CREATE INDEX usersnotified_notifyid_idx ON usersnotified USING btree (notifyid);


--
-- Name: applicationid_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY application_service_map
    ADD CONSTRAINT applicationid_fkey1 FOREIGN KEY (appid) REFERENCES applications(id) ON DELETE CASCADE;


--
-- Name: categoryid_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY category_node
    ADD CONSTRAINT categoryid_fkey1 FOREIGN KEY (categoryid) REFERENCES categories(categoryid) ON DELETE CASCADE;


--
-- Name: categoryid_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY category_group
    ADD CONSTRAINT categoryid_fkey2 FOREIGN KEY (categoryid) REFERENCES categories(categoryid) ON DELETE CASCADE;


--
-- Name: fk_alarmid1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY alarm_attributes
    ADD CONSTRAINT fk_alarmid1 FOREIGN KEY (alarmid) REFERENCES alarms(alarmid) ON DELETE CASCADE;


--
-- Name: fk_alarms_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY alarms
    ADD CONSTRAINT fk_alarms_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_alarms_systemid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY alarms
    ADD CONSTRAINT fk_alarms_systemid FOREIGN KEY (systemid) REFERENCES monitoringsystems(id) ON DELETE CASCADE;


--
-- Name: fk_bridgebridgelink_designatednodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgebridgelink
    ADD CONSTRAINT fk_bridgebridgelink_designatednodeid FOREIGN KEY (designatednodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_bridgebridgelink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgebridgelink
    ADD CONSTRAINT fk_bridgebridgelink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_bridgeelement_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgeelement
    ADD CONSTRAINT fk_bridgeelement_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_bridgemaclink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgemaclink
    ADD CONSTRAINT fk_bridgemaclink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_bridgestplink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bridgestplink
    ADD CONSTRAINT fk_bridgestplink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_bsm_service_attributes_service_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_attributes
    ADD CONSTRAINT fk_bsm_service_attributes_service_id FOREIGN KEY (bsm_service_id) REFERENCES bsm_service(id);


--
-- Name: fk_bsm_service_child_service_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_children
    ADD CONSTRAINT fk_bsm_service_child_service_id FOREIGN KEY (bsm_service_child_id) REFERENCES bsm_service(id) ON DELETE CASCADE;


--
-- Name: fk_bsm_service_children_edge_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_children
    ADD CONSTRAINT fk_bsm_service_children_edge_id FOREIGN KEY (id) REFERENCES bsm_service_edge(id) ON DELETE CASCADE;


--
-- Name: fk_bsm_service_edge_map_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_edge
    ADD CONSTRAINT fk_bsm_service_edge_map_id FOREIGN KEY (bsm_map_id) REFERENCES bsm_map(id);


--
-- Name: fk_bsm_service_edge_service_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_edge
    ADD CONSTRAINT fk_bsm_service_edge_service_id FOREIGN KEY (bsm_service_id) REFERENCES bsm_service(id);


--
-- Name: fk_bsm_service_ifservices_edge_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_ifservices
    ADD CONSTRAINT fk_bsm_service_ifservices_edge_id FOREIGN KEY (id) REFERENCES bsm_service_edge(id) ON DELETE CASCADE;


--
-- Name: fk_bsm_service_ifservices_ifserviceid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_ifservices
    ADD CONSTRAINT fk_bsm_service_ifservices_ifserviceid FOREIGN KEY (ifserviceid) REFERENCES ifservices(id) ON DELETE CASCADE;


--
-- Name: fk_bsm_service_reduce_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service
    ADD CONSTRAINT fk_bsm_service_reduce_id FOREIGN KEY (bsm_reduce_id) REFERENCES bsm_reduce(id);


--
-- Name: fk_bsm_service_reductionkeys_edge_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY bsm_service_reductionkeys
    ADD CONSTRAINT fk_bsm_service_reductionkeys_edge_id FOREIGN KEY (id) REFERENCES bsm_service_edge(id);


--
-- Name: fk_cdpelement_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY cdpelement
    ADD CONSTRAINT fk_cdpelement_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_cdplink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY cdplink
    ADD CONSTRAINT fk_cdplink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_demandpollid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY pollresults
    ADD CONSTRAINT fk_demandpollid FOREIGN KEY (pollid) REFERENCES demandpolls(id) ON DELETE CASCADE;


--
-- Name: fk_eventid1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY outages
    ADD CONSTRAINT fk_eventid1 FOREIGN KEY (svclosteventid) REFERENCES events(eventid) ON DELETE CASCADE;


--
-- Name: fk_eventid2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY outages
    ADD CONSTRAINT fk_eventid2 FOREIGN KEY (svcregainedeventid) REFERENCES events(eventid) ON DELETE CASCADE;


--
-- Name: fk_eventid3; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY notifications
    ADD CONSTRAINT fk_eventid3 FOREIGN KEY (eventid) REFERENCES events(eventid) ON DELETE CASCADE;


--
-- Name: fk_eventidak2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY alarms
    ADD CONSTRAINT fk_eventidak2 FOREIGN KEY (lasteventid) REFERENCES events(eventid) ON DELETE CASCADE;


--
-- Name: fk_hwentity_hwentityattribute; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentityattribute
    ADD CONSTRAINT fk_hwentity_hwentityattribute FOREIGN KEY (hwentityid) REFERENCES hwentity(id) ON DELETE CASCADE;


--
-- Name: fk_hwentity_node; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentity
    ADD CONSTRAINT fk_hwentity_node FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_hwentity_parent; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentity
    ADD CONSTRAINT fk_hwentity_parent FOREIGN KEY (parentid) REFERENCES hwentity(id) ON DELETE CASCADE;


--
-- Name: fk_hwentityattribute_hwentityattributetype; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY hwentityattribute
    ADD CONSTRAINT fk_hwentityattribute_hwentityattributetype FOREIGN KEY (hwattribtypeid) REFERENCES hwentityattributetype(id) ON DELETE CASCADE;


--
-- Name: fk_ia_nodeid7; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY inventory
    ADD CONSTRAINT fk_ia_nodeid7 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_ipnettomedia_sourcenodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ipnettomedia
    ADD CONSTRAINT fk_ipnettomedia_sourcenodeid FOREIGN KEY (sourcenodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_isiselement_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY isiselement
    ADD CONSTRAINT fk_isiselement_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_isislink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY isislink
    ADD CONSTRAINT fk_isislink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_lldpelement_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY lldpelement
    ADD CONSTRAINT fk_lldpelement_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_lldplink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY lldplink
    ADD CONSTRAINT fk_lldplink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_ncs_attr_comp_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ncs_attributes
    ADD CONSTRAINT fk_ncs_attr_comp_id FOREIGN KEY (ncscomponent_id) REFERENCES ncscomponent(id);


--
-- Name: fk_node_location; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY node
    ADD CONSTRAINT fk_node_location FOREIGN KEY (location) REFERENCES monitoringlocations(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: fk_nodeid1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ipinterface
    ADD CONSTRAINT fk_nodeid1 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_nodeid2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY snmpinterface
    ADD CONSTRAINT fk_nodeid2 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_nodeid5; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY assets
    ADD CONSTRAINT fk_nodeid5 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_nodeid7; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY notifications
    ADD CONSTRAINT fk_nodeid7 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_nodeid8; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY pathoutage
    ADD CONSTRAINT fk_nodeid8 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_notifid2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY usersnotified
    ADD CONSTRAINT fk_notifid2 FOREIGN KEY (notifyid) REFERENCES notifications(notifyid) ON DELETE CASCADE;


--
-- Name: fk_ospfelement_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ospfelement
    ADD CONSTRAINT fk_ospfelement_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_ospflink_nodeid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ospflink
    ADD CONSTRAINT fk_ospflink_nodeid FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: fk_qrtz_blob_triggers; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_blob_triggers
    ADD CONSTRAINT fk_qrtz_blob_triggers FOREIGN KEY (trigger_name, trigger_group) REFERENCES qrtz_triggers(trigger_name, trigger_group);


--
-- Name: fk_qrtz_cron_triggers; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_cron_triggers
    ADD CONSTRAINT fk_qrtz_cron_triggers FOREIGN KEY (trigger_name, trigger_group) REFERENCES qrtz_triggers(trigger_name, trigger_group);


--
-- Name: fk_qrtz_job_listeners; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_job_listeners
    ADD CONSTRAINT fk_qrtz_job_listeners FOREIGN KEY (job_name, job_group) REFERENCES qrtz_job_details(job_name, job_group);


--
-- Name: fk_qrtz_simple_triggers; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_simple_triggers
    ADD CONSTRAINT fk_qrtz_simple_triggers FOREIGN KEY (trigger_name, trigger_group) REFERENCES qrtz_triggers(trigger_name, trigger_group);


--
-- Name: fk_qrtz_trigger_listeners; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_trigger_listeners
    ADD CONSTRAINT fk_qrtz_trigger_listeners FOREIGN KEY (trigger_name, trigger_group) REFERENCES qrtz_triggers(trigger_name, trigger_group);


--
-- Name: fk_qrtz_triggers; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY qrtz_triggers
    ADD CONSTRAINT fk_qrtz_triggers FOREIGN KEY (job_name, job_group) REFERENCES qrtz_job_details(job_name, job_group);


--
-- Name: fk_serviceid1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ifservices
    ADD CONSTRAINT fk_serviceid1 FOREIGN KEY (serviceid) REFERENCES service(serviceid) ON DELETE CASCADE;


--
-- Name: fk_statsdata_reportid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY statisticsreportdata
    ADD CONSTRAINT fk_statsdata_reportid FOREIGN KEY (reportid) REFERENCES statisticsreport(id) ON DELETE CASCADE;


--
-- Name: fk_statsdata_resourceid; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY statisticsreportdata
    ADD CONSTRAINT fk_statsdata_resourceid FOREIGN KEY (resourceid) REFERENCES resourcereference(id) ON DELETE CASCADE;


--
-- Name: fk_stickyMemo; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY alarms
    ADD CONSTRAINT "fk_stickyMemo" FOREIGN KEY (stickymemo) REFERENCES memos(id) ON DELETE CASCADE;


--
-- Name: fk_subcomp_comp_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY subcomponents
    ADD CONSTRAINT fk_subcomp_comp_id FOREIGN KEY (component_id) REFERENCES ncscomponent(id);


--
-- Name: fk_subcomp_subcomp_id; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY subcomponents
    ADD CONSTRAINT fk_subcomp_subcomp_id FOREIGN KEY (subcomponent_id) REFERENCES ncscomponent(id);


--
-- Name: ifservices_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY outages
    ADD CONSTRAINT ifservices_fkey2 FOREIGN KEY (ifserviceid) REFERENCES ifservices(id) ON DELETE CASCADE;


--
-- Name: ifservices_fkey3; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY application_service_map
    ADD CONSTRAINT ifservices_fkey3 FOREIGN KEY (ifserviceid) REFERENCES ifservices(id) ON DELETE CASCADE;


--
-- Name: ifservices_fkey4; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY location_specific_status_changes
    ADD CONSTRAINT ifservices_fkey4 FOREIGN KEY (ifserviceid) REFERENCES ifservices(id) ON DELETE CASCADE;


--
-- Name: ipinterface_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ifservices
    ADD CONSTRAINT ipinterface_fkey FOREIGN KEY (ipinterfaceid) REFERENCES ipinterface(id) ON DELETE CASCADE;


--
-- Name: location_specific_status_changes_systemid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY location_specific_status_changes
    ADD CONSTRAINT location_specific_status_changes_systemid_fkey FOREIGN KEY (systemid) REFERENCES monitoringsystems(id) ON DELETE CASCADE;


--
-- Name: monitoringlocationscollectionpackages_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY monitoringlocationscollectionpackages
    ADD CONSTRAINT monitoringlocationscollectionpackages_fkey FOREIGN KEY (monitoringlocationid) REFERENCES monitoringlocations(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: monitoringlocationspollingpackages_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY monitoringlocationspollingpackages
    ADD CONSTRAINT monitoringlocationspollingpackages_fkey FOREIGN KEY (monitoringlocationid) REFERENCES monitoringlocations(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: monitoringlocationstags_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY monitoringlocationstags
    ADD CONSTRAINT monitoringlocationstags_fkey FOREIGN KEY (monitoringlocationid) REFERENCES monitoringlocations(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: monitoringsystemsproperties_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY monitoringsystemsproperties
    ADD CONSTRAINT monitoringsystemsproperties_fkey FOREIGN KEY (monitoringsystemid) REFERENCES monitoringsystems(id) ON DELETE CASCADE;


--
-- Name: nodeid_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY category_node
    ADD CONSTRAINT nodeid_fkey1 FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: requisitioned_categoryid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY requisitioned_categories
    ADD CONSTRAINT requisitioned_categoryid_fkey FOREIGN KEY (categoryid) REFERENCES categories(categoryid) ON DELETE CASCADE;


--
-- Name: requisitioned_nodeid_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY requisitioned_categories
    ADD CONSTRAINT requisitioned_nodeid_fkey FOREIGN KEY (nodeid) REFERENCES node(nodeid) ON DELETE CASCADE;


--
-- Name: scanreportlogs_scanreports_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreportlogs
    ADD CONSTRAINT scanreportlogs_scanreports_fkey FOREIGN KEY (scanreportid) REFERENCES scanreports(id) ON DELETE CASCADE;


--
-- Name: scanreportpollresults_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreportpollresults
    ADD CONSTRAINT scanreportpollresults_fkey FOREIGN KEY (scanreportid) REFERENCES scanreports(id) ON DELETE CASCADE;


--
-- Name: scanreportproperties_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreportproperties
    ADD CONSTRAINT scanreportproperties_fkey FOREIGN KEY (scanreportid) REFERENCES scanreports(id) ON DELETE CASCADE;


--
-- Name: scanreports_monitoringlocations_fkey; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY scanreports
    ADD CONSTRAINT scanreports_monitoringlocations_fkey FOREIGN KEY (location) REFERENCES monitoringlocations(id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: snmpinterface_fkey2; Type: FK CONSTRAINT; Schema: public; Owner: opennms
--

ALTER TABLE ONLY ipinterface
    ADD CONSTRAINT snmpinterface_fkey2 FOREIGN KEY (snmpinterfaceid) REFERENCES snmpinterface(id) ON DELETE SET NULL;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

ALTER TABLE events ALTER COLUMN eventid SET DEFAULT nextval('eventsnxtid');

