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

    def formatMessage(m: RichString, info: Logger3.Info): String = 
        let Info({
            severity = sv,
            timestamp = ts,
            sysInfo = si,
            format = fmt,
            other = ot
        }) = info;
        let s = Map.foldLeftWithKey(acc -> reg -> v ->
            String.replaceMatches(regex = reg, to = v, acc), 
            fmt, ot 
            |> Map.insert("%severity", "${sv}") 
            |> Map.insert("%timestamp", "${Option.getWithDefault("None", ts)}")
            |> Map.insert("%sysInfo", "${if (Map.isEmpty(si)) "None" else "${si}"}")
            |> Map.insert("%msg", "${m}")
        );
        s

    pub def withTimestamp(f: Unit -> a \ ef): a \ ef + Logger3 + Timestamp = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info(i) = info;
                let info_ = Info.Info({timestamp = Some(Timestamp.getTimestamp()) | i});
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

    pub def fatal(s: String): Unit \ Logger3 = 
        let i = Info({
            severity = Severity.Fatal, 
            timestamp = None,
            sysInfo = Map.empty(),
            format = defaultFormat(),
            other = Map.empty()
            });
        Logger3.log(i, RichString.text(s))

    pub def toOldLogger(f: Unit -> a \ ef): a \ (ef - Logger3) + Logger =
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
        "[%severity] %msg"

    pub def withCommonLogFormat(ip: Net.IpAddr, name: String, status: Int32, size: Int32, f: Unit -> a \ ef): a \ ef + Logger3 + Timestamp = 
        run {
            f()
        } with handler Logger3 {
            def log(info, m, k) = 
                let Info({other = old, timestamp = timestamp | _}) = info;
                let ip_ = match ip {
                    case IpAddr.V4(i) => "${i}"
                    case IpAddr.V6(i) => "${i}"
                };
                let ts = match timestamp {
                    case None => Some(Timestamp.getTimestamp())
                    case Some(t) => Some(t)
                };
                let other = Map#{
                    "%ip" => "${ip_}",
                    "%name" => name,
                    "%status" => "${status}",
                    "%size" => "${size}"
                };
                let Info(i) = info;
                let n = Map.union(other, old);
                let info_ = (Info({other = n, format = commonLogFormat(), timestamp = ts | i}));
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
        "<%priority> %timestamp %hostname %application[%pid]: %msg"

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