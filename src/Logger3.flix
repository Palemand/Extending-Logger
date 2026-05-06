eff Logger3 {
    def log(r: Logger3.LogRecord): Unit
}

mod Logger3 {
    import java.lang.ProcessHandle;
    use Net.IpAddr
    use RichString.{gray, cyan, green, yellow, red, bold, text};
    use Logger.Severity
    use Logger3.LogRecord.LogRecord

    pub enum LogRecord ({
        message = RichString,
        severity = Logger.Severity, 
        timestamp = Option[String], 
        aux = Map[String, String]
    })

    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Logger3) + IO =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let rs = formatMessage(defaultFormat(), r);
                println(rs);
                k()
        }
    
    pub def runWithIOFmt(fmt: String, f: Unit -> a \ ef): a \ (ef - Logger3) + IO =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let rs = formatMessage(fmt, r);
                println(rs);
                k()
        }

    def formatMessage(fmt: String, r: LogRecord): String \ {} = 
        let LogRecord({
            message = msg,
            severity = sv,
            timestamp = ts,
            aux = aux
        }) = r;
        let pref = "(?<!(?<!\\\\)\\\\)";
        let s = Map.foldLeftWithKey(acc -> reg -> v ->
            String.replaceMatches(regex = pref + reg, to = v, acc), 
            fmt, 
            aux |> Map.insert("%severity", "${sv}")
            |> Map.insert("%timestamp", "${Option.getWithDefault("", ts)}")
        );
        String.replaceMatches(regex = pref + "%msg", to = "${msg}", s)

    pub def withTimestamp(f: Unit -> a \ ef): a \ ef + Logger3 + Timestamp = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let LogRecord(i) = r;
                let record = LogRecord({timestamp = Some(Timestamp.getTimestamp()) | i});
                Logger3.log(record);
                k()
        }

    pub def withAux(aux: Map[String, String], f: Unit -> a \ ef): a \ ef + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let LogRecord({aux = old | _}) = r;
                let LogRecord(i) = r;
                let n = Map.union(aux, old);
                let info_ = (LogRecord({aux = n | i}));
                Logger3.log(info_);
                k()
        }

    def logSeverity(s: Severity, m: String): Unit \ Logger3 = 
        let i = LogRecord({
            severity = s, 
            message = RichString.text(m),
            timestamp = None,
            aux = Map.empty()
        });
        Logger3.log(i)

    pub def trace(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Trace, m)

    pub def debug(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Debug, m)

    pub def info(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Info, m)

    pub def warn(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Warn, m)

    pub def fatal(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Fatal, m)

    pub def withOldLogger(f: Unit -> a \ ef): a \ (ef - Logger3) + Logger =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let LogRecord({severity = s | _}) = r;
                let m_ = formatMessage(defaultFormat(), r);
                Logger.log(s, RichString.text(m_));
                k()
        }

    pub def defaultFormat(): String = 
        "%timestamp[%severity] %msg"

    pub def logCommonLog(ip: Net.IpAddr, name: String, message: String, status: Int32, size: Int32, s: Option[Severity]): Unit \ Logger3 + Timestamp = 
        let ip_ = match ip {
            case IpAddr.V4(i) => "${i}"
            case IpAddr.V6(i) => "${i}"
        };
        let other = Map#{
            "%ip" => "${ip_}",
            "%name" => name,
            "%status" => "${status}",
            "%size" => "${size}"
        };
        let r = LogRecord({
            message = RichString.text(message),
            severity = Option.getWithDefault(Severity.Info, s), 
            timestamp = Some(Timestamp.getTimestamp()),
            aux = other
        });
        Logger3.log(r)

    pub def runWithCommonLogFormat(f: Unit -> a \ ef): a \ (ef - Logger3) + IO = 
        runWithIOFmt(commonLogFormat(), f)

    pub def commonLogFormat(): String = 
        "%ip - %name [%timestamp] \"%msg\" %status %size"

    pub def syslogFormat(): String = 
        "<%priority>%timestamp %hostname %application[%pid]: %msg"

    pub def runWithSyslogFormat(f: Unit -> a \ ef): a \ (ef - Logger3) + IO =
        runWithIOFmt(syslogFormat(), f)
}