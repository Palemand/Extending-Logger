eff Logger3 {
    def log(info: Logger3.Info, m: RichString): Unit
}

mod Logger3 {
    use Net.IpAddr
    use Info.Info
    use RichString.{gray, cyan, green, yellow, red, bold, text};

    /**
    TODO:
    - RichString handling
    - Escape characters
     */

    pub enum Info2 {
        case Severity(Severity)
        case Timestamp(String) // maybe a datastructure?
        case SysInfo(Map[String, String])
        case Format(String)
        case Other(String)
    }

    pub enum Info ({
        severity = Severity, 
        timestamp = Option[String], 
        sysInfo = Map[String, String],
        format = String,
        other = Map[String, String]
    })

    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Logger3) + IO =
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let rs = formatMessage(m, info);
                println(rs);
                k()
        }

    def formatMessage(m: RichString, info: Logger3.Info): String \ {} = 
        let Info({
            severity = sv,
            timestamp = ts,
            sysInfo = si,
            format = fmt,
            other = ot
        }) = info;
        let pref = "(?<!(?<!\\\\)\\\\)";
        let s = Map.foldLeftWithKey(acc -> reg -> v ->
            //Regex.replace(src = reg, dst = v, acc),
            String.replaceMatches(regex = pref + reg, to = v, acc), 
            fmt, 
            ot |> Map.insert("%severity", "${sv}")
            |> Map.insert("%timestamp", "${Option.getWithDefault("", ts)}")
            |> Map.insert("%sysInfo", "${if (Map.isEmpty(si)) "None" else "${si}"}")
        );
        String.replaceMatches(regex = pref + "%msg", to = "${m}", s)

    pub def withTimestamp(f: Unit -> a \ ef): a \ ef + Logger3 + Timestamp = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info(i) = info;
                let info_ = Info({timestamp = Some(Timestamp.getTimestamp()) | i});
                Logger3.log(info_, m);
                k()
        }

    pub def withOther(other: Map[String, String], f: Unit -> a \ ef): a \ ef + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info({other = old | _}) = info;
                let Info(i) = info;
                let n = Map.union(other, old);
                let info_ = (Info({other = n | i}));
                Logger3.log(info_, m);
                k()
        }

    def logSeverity(s: Severity, m: String): Unit \ Logger3 = 
        let i = Info({
            severity = s, 
            timestamp = None,
            sysInfo = Map.empty(),
            format = defaultFormat(),
            other = Map.empty()
        });
        Logger3.log(i, RichString.text(m))

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
            def log(info, m, k) = 
                let Info({severity = s | _}) = info;
                let m_ = formatMessage(m, info);
                Logger.log(s, RichString.text(m_));
                k()
        }

    def defaultFormat(): String = 
        "%timestamp[%severity] %msg"

    // maybe not
    pub def logHttpRequest(res: Net.HttpResponse, socket: Net.TcpSocket): Unit \ Logger3 + Timestamp + IO + Abort = 
        let Net.TcpSocket.TcpSocket(s, _, _) = socket;
        let ips = s.getInetAddress().toString();
        let ip = Option.getOrAbort(Net.IpAddr.fromString(ips));
        Logger3.logCommonLog(ip, "-", "msg", Net.HttpResponse., 2326, None)

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
        let info = Info({
            severity = Option.getWithDefault(Severity.Info, s), 
            timestamp = Some(Timestamp.getTimestamp()),
            sysInfo = Map.empty(),
            format = commonLogFormat(),
            other = other
        });
        Logger3.log(info, RichString.text(message))

    pub def withCommonLogFormat(f: Unit -> a \ ef): a \ ef + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info(i) = info;
                let info_ = Info({format = commonLogFormat() | i});
                Logger3.log(info_, m);
                k()
        }

    def commonLogFormat(): String = 
        "%ip - %name [%timestamp] \"%msg\" %status %size"

    pub def withCustomFormat(format: String, f: Unit -> a \ ef): a \ ef + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info(i) = info;
                let info_ = (Info({format = format | i}));
                Logger3.log(info_, m);
                k()
        }

    pub def withSyslogFormat(priority: Int32, hostname: String, application: String, pid: Int32, f: Unit -> a \ ef): a \ ef + Logger3 + Timestamp = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info({other = old, timestamp = timestamp | _}) = info;
                let ts = match timestamp {
                    case None => Some(Timestamp.getTimestamp())
                    case Some(t) => Some(t)
                };
                let other = Map#{
                    "%priority" => "${priority}",
                    "%hostname" => hostname,
                    "%application" => application,
                    "%pid" => "${pid}"
                };
                let Info(i) = info;
                let n = Map.union(other, old);
                let info_ = Info({other = n, format = syslogFormat(), timestamp = ts | i});
                Logger3.log(info_, m);
                k()
        } 

    def syslogFormat(): String = 
        "<%priority>%timestamp %hostname %application[%pid]: %msg"

    pub def withRichSeverity(f: Unit -> a \ ef): a \ ef + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info(i) = info;
                let {format = fmt, severity = sv | _} = i;
                let s = "%severity";
                let sv_ = match sv {
                    case Severity.Debug => cyan(s)
                    case Severity.Fatal => bold(red(s))
                    case Severity.Info  => green(s)
                    case Severity.Trace => gray(s)
                    case Severity.Warn  => yellow(s)
                };
                let newFmt = String.replaceMatches(regex=s, to="${sv_}", fmt);
                let info_ = Info({format = newFmt | i});
                Logger3.log(info_, m);
                k()
        }
}