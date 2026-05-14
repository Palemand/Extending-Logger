eff Logger3 {
    def log(r: Logger3.LogRecord): Unit
}

mod Logger3 {
    use Logger3.LogRecord
    use Net.IpAddr
    use RichString.{gray, cyan, green, yellow, red, bold, text};
    use Logger.Severity
    use Logger3.LogRecord.LogRecord

    ///
    /// Enum that keeps track of information in a logrecord.
    ///
    pub enum LogRecord ({
        message = RichString,
        severity = Logger.Severity, 
        timestamp = Option[String], 
        aux = Map[String, String]
    }) 

    ///
    /// LogRecords are equal when everything matches.
    ///
    instance Eq[LogRecord] {
        pub def eq(x: LogRecord, y: LogRecord): Bool = 
            match (x,y) {
                case (
                    LogRecord({message = rsx, severity = sx, timestamp = tsx, aux = auxx}),
                    LogRecord({message = rsy, severity = sy, timestamp = tsy, aux = auxy})
                ) => rsx == rsy and sx == sy and auxx == auxy and tsy == tsx
            }
    }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `IO` effect.
    ///
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

    ///
    /// Runs the `Logger` effect of the given function `f`, with a specified format.
    ///
    pub def runWithIOFmt(format: d, f: Unit -> a \ ef): a \ (ef - Logger3) + IO with ToString[d] =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let rs = formatMessage(format, r);
                println(rs);
                k()
        }

    def formatMessage(fmt: d, r: LogRecord): String \ {} with ToString[d] = 
        let LogRecord({
            message = msg,
            severity = sv,
            timestamp = ts,
            aux = aux
        }) = r;
        let s = Map.foldLeftWithKey(acc -> reg -> v ->
            String.replaceMatches(regex = "\\{${reg}\\}", to = v, acc), 
            ToString.toString(fmt), 
            aux |> Map.insert("severity", "${sv}")
            |> Map.insert("timestamp", "${Option.getWithDefault("", ts)}")
        );
        String.replaceMatches(regex = "\\{message\\}", to = "${msg}", s)
    
    ///
    /// Middleware handler that attaches a timestamp from whenever the handler is invoked.
    ///
    /// Raises the Timestamp effect.
    ///
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

    ///
    /// Middleware handler that attaches auxillery information to the log record.
    ///
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

    def logSeverity(s: Severity, m: a): Unit \ Logger3 + Formattable.Aef[a] with Formattable[a] = 
        let i = LogRecord({
            severity = s, 
            message = RichString.text(m),
            timestamp = None,
            aux = Map.empty()
        });
        Logger3.log(i)
    
    ///
    /// Logs the message `m` at the `Trace` level.
    ///
    pub def trace(m: a): Unit \ Logger3 + Formattable.Aef[a] with Formattable[a]= 
        logSeverity(Severity.Trace, m)
    ///
    /// Logs the message `m` at the `Debug` level.
    ///
    pub def debug(m: a): Unit \ Logger3 + Formattable.Aef[a] with Formattable[a]= 
        logSeverity(Severity.Debug, m)
    ///
    /// Logs the message `m` at the `Info` level.
    ///
    pub def info(m: a): Unit \ Logger3 + Formattable.Aef[a] with Formattable[a]= 
        logSeverity(Severity.Info, m)
    ///
    /// Logs the message `m` at the `Warn` level.
    ///
    pub def warn(m: a): Unit \ Logger3 + Formattable.Aef[a] with Formattable[a]= 
        logSeverity(Severity.Warn, m)
    ///
    /// Logs the message `m` at the `Fatal` level.
    ///
    pub def fatal(m: a): Unit \ Logger3 + Formattable.Aef[a] with Formattable[a]= 
        logSeverity(Severity.Fatal, m)
    ///
    /// Default format for log events.
    ///
    pub def defaultFormat(): String = 
        "{timestamp}[{severity}] {message}"
    ///
    /// Logs an event given all the auxillery information to fulfill the commonlog format.
    ///
    pub def logCommonLog(ip: Net.IpAddr, name: String, message: String, status: Int32, size: Int32, s: Option[Severity]): Unit \ Logger3 + Timestamp = 
        let ip_ = match ip {
            case IpAddr.V4(i) => "${i}"
            case IpAddr.V6(i) => "${i}"
        };
        let other = Map#{
            "ip" => "${ip_}",
            "name" => name,
            "status" => "${status}",
            "size" => "${size}"
        };
        let r = LogRecord({
            message = RichString.text(message),
            severity = Option.getWithDefault(Severity.Info, s), 
            timestamp = Some(Timestamp.getTimestamp()),
            aux = other
        });
        Logger3.log(r)
    ///
    /// IO handler that uses the commonlog format.
    ///
    pub def runWithCommonLogFormat(f: Unit -> a \ ef): a \ (ef - Logger3) + IO = 
        runWithIOFmt(commonLogFormat(), f)
    ///
    /// Commonlog format.
    ///
    pub def commonLogFormat(): String = 
        "{ip} - {name} [{timestamp}] \"{messsage}\" {status} {size}"

    ///
    /// IO handler that uses the syslog format.
    ///
    pub def runWithSyslogFormat(f: Unit -> a \ ef): a \ (ef - Logger3) + IO =
        runWithIOFmt(syslogFormat(), f)

    ///
    /// Syslog format.
    ///
    pub def syslogFormat(): String = 
        "<{priority}>{timestamp} {hostname} {application}[{pid}]: {messsage}"

    ///
    /// Handles the `Logger` effect of the given function `f`, printing to StdOut.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    pub def handleWithStdOut(format: d, f: Unit -> b \ ef): b \ ef - Logger3 + Sys.Console with ToString[d] = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let rs = formatMessage(format, r);
                Sys.Console.println(rs);
                k()
            }
        }
    
    ///
    /// Handles the `Logger` effect of the given function `f`, printing to StdErr.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    pub def handleWithStdErr(format: d, f: Unit -> b \ ef): b \ ef - Logger3 + Sys.Console with ToString[d] = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let rs = formatMessage(format, r);
                Sys.Console.eprintln(rs);
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Aborts if an error is encountered while writing to file.
    ///
    pub def runWithFileWriteAbort(format: d, file: String, f: Unit -> a \ ef): a \ (ef - Logger3) + Fs.FileWrite + Abort with ToString[d] =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let rs = formatMessage(format, r);
                let res = Fs.FileWrite.append({str = rs}, file);
                match res {
                    case Err(e) => Abort.abort(RichString.text(e))
                    case Ok() => ()
                };
                k()
            }
        }

    ///
    /// Aborts with the message from `Logger3.log` if given function `g` returns true on the logrecord.
    /// Otherwise, lets `Logger` effect propagate.
    ///
    pub def runWithAbort(g: Logger3.LogRecord -> Bool \ ef1, f: Unit -> a \ ef2): a \ ef1 + ef2 + Abort + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) =
                if (g(r)) {
                    Abort.abort({let LogRecord({message = msg | _}) = r; msg})
                } else {
                    Logger3.log(r); k()
                }
            }
    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, returns a list of `LogRecord`, which is ordered by the calls to log.
    ///
    pub def runWithList(f: Unit -> b \ ef): (b, List[LogRecord]) \ (ef - Logger3) = 
        run {
            (f(), List.empty())
        } with handler Logger3 {
            def log(r, k) = {
                let (res, l) = k();
                (res, r :: l)
            }
        }
    
    ///
    /// Middleware handler that filters the logging, by the `filter` function,
    /// only allowing the records that return true.
    ///
    pub def runWithFilter(filter: (LogRecord) -> Bool \ ef1, f: Unit -> b \ ef2): b \ (ef2 - Logger3) + Logger3 + ef1 =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                if (filter(r)) {
                    Logger3.log(r);
                    k()
                } else {
                    k()
                }
            }
        }
    ///
    /// Filters logs with greater or equal severity level
    ///
    pub def runWithSeverityFilter(s: Severity, f: Unit -> b \ ef): b \ (ef - Logger3) + Logger3 =
        run {
            f()
        } with runWithFilter((r) -> let LogRecord({severity = sev | _}) = r; Order.greaterEqual(sev, s))

    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    /// 
    /// Raises a new `Logger` effect for fallback incase Http fails.
    ///
    pub def handleWithHttp(format: d, url: String, f: a -> b \ ef1): a -> b \ (ef1 - Logger3) + Net.Http + Logger3 with ToString[d] = 
        x -> run {
            f(x)
        } with handler Logger3 {
            def log(r, k) = {
                let res = Net.Http.post(url, formatMessage(format, r));
                match res {
                    case Ok(_response) => k()
                    case Err(_t) => {
                        Logger3.log(r);
                        k()
                    }
                }
            }
        }

    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    ///
    pub def runWithHttp(format: d, url: String, f: Unit -> b \ ef1 ): b \ (ef1 - Logger3) + Net.Http with ToString[d] = 
        run {
            handleWithHttp(format, url, f)()
        } with runWithIgnore

    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    ///
    /// Raises a new `Logger` effect for fallback incase Http fails.
    ///
    pub def runWithHttpFallback(format: d, url: String, f: Unit -> b \ ef1 ): b \ (ef1 - Logger3) + Net.Http + Logger3 with ToString[d] = 
        handleWithHttp(format, url, f)()

    ///
    /// Handles the `Logger` effect, by sending it over a TCP connection.
    ///
    def handleWithTCP(format: d, ip: Net.IpAddr, port: Int16, f: a -> b \ ef): 
        a -> b \ (ef - Logger3) + Net.TcpConnect + Logger3 + IO with ToString[d] = {
        x -> {
            let resSock = Net.TcpConnect.connect(ip, port);
            run {
                f(x)
            } with handler Logger3 {
                def log(r, k) = {
                    match resSock {
                        case Err(_t) => Logger3.log(r); k()
                        case Ok(sock) => {
                            let str = formatMessage(format, r);
                            region rc {
                                let bytes = String.toBytes(str);
                                let arr = Vector.toArray(rc, bytes);
                                let res = Net.TcpSocket.write(arr, sock);
                                match res {
                                    case Err(_t) => Logger3.log(r)
                                    case Ok(_) => ()
                                }
                            };
                            k()
                        }
                    }
                }
            }
        }
    }

    ///
    /// Handles the `Logger` effect, by sending it over a TCP connection.
    ///
    /// Raises a new `Logger` effect for fallback incase TCP fails.
    ///
    pub def runWithTCPFallback(format: d, ip: Net.IpAddr, port: Int16, f: Unit -> b \ ef):b \ (ef - Logger3) + Net.TcpConnect + Logger3 + IO with ToString[d] = 
        handleWithTCP(format, ip, port, f)()


    ///
    /// Handles the `Logger` effect, by sending it over a TCP connection.
    ///
    pub def runWithTCP(format: d, ip: Net.IpAddr, port: Int16, f: Unit -> b \ ef): b \ (ef - Logger3) + IO + Net.TcpConnect with ToString[d] =
        run {
            handleWithTCP(format, ip, port, f)()
        } with runWithIgnore
    
    ///
    /// Handles the `Logger` effect by writing to a file. 
    /// If the file does not exist, it is created.
    /// Discards style from the logged messages. 
    ///
    /// Raises a new `Logger` effect for fallback incase FileWrite fails.
    ///
    def handleWithFileWrite(format: d, file: String, f: a -> b \ ef): a -> b \ (ef - Logger3) + Fs.FileWrite + Logger3 with ToString[d] = 
        x -> run {
            f(x)
        } with handler Logger3 {
            def log(r, k) = {
                let output = formatMessage(format, r);
                let writeRes = Fs.FileWrite.append({str = output}, file);
                match writeRes {
                    case Err(_) => Logger3.log(r); k()
                    case Ok(_) => k()
                }
            }
        }

    ///
    /// Runs the `Logger` effect by writing to a file. 
    /// If the file does not exist, it is created.
    /// Discards style from the logged messages. 
    ///
    /// Raises a new `Logger` effect for fallback incase FileWrite fails.
    ///
    pub def runWithFileWriteFallback(format: d, file: String, f: Unit -> a \ ef): a \ (ef - Logger3) + Fs.FileWrite + Logger3 with ToString[d] = 
        handleWithFileWrite(format, file, f)()

    ///
    /// Runs the `Logger` effect by writing to a file. 
    /// If the file does not exist, it is created.
    /// Discards style from the logged messages. 
    ///
    /// Raises a new `Logger` effect for fallback incase FileWrite fails.
    ///
    pub def runWithFileWrite(format: d, file: String, f: Unit -> a \ ef): a \ (ef - Logger3) + Fs.FileWrite with ToString[d] = 
        run {
            handleWithFileWrite(format, file, f)()
        } with runWithIgnore

    ///
    /// Handles the `Logger` effect, by ignoring it.
    ///
    pub def runWithIgnore(f: Unit -> b \ ef): b \ (ef - Logger3) = 
        run {
            f()
        } with handler Logger3 {
            def log(_r, k) = k()
        }
    ///
    /// Handles the Logger effect in `f` with both `h1` and `h2`. 
    ///
    pub def runWithMultisink(
        h1: (Unit -> Unit \ Logger3) -> x \ ef4, 
        h2: (Unit -> Unit \ Logger3) -> y \ ef5, 
        f: Unit -> a \ ef3): a \ (ef3 - Logger3) + ef4 + ef5 = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let _ = run {
                     Logger3.log(r)
                 } with h1;
                 let _ = run {
                     Logger3.log(r)
                 } with h2;
                k()
        }
    
    ///
    /// Terminating handler for the `Logger` effect, that will batch multiples of the same log messages that are sent within `timeout` of each other.
    ///
    /// Uses a seperate thread for handling the batching. 
    ///
    pub def withBatchingThread(h1: (Unit -> Unit \ (Logger3 + NonDet + Chan + IO)) -> Unit \ IO + NonDet + Chan, 
    timeout: Time.Duration, f: Unit -> a \ ef ): a \ (ef - Logger3) + Chan + NonDet + IO =
        region rc {
            let (tx, rx) = Channel.unbuffered();
            let (closetx, closerx) = Channel.unbuffered();
            spawn batchingThread(h1, timeout, rx, closerx) @ rc;
            run {
                let res = f();
                Channel.send((), closetx);
                res
            } with handler Logger3 {
                def log(r, k) = {
                    Channel.send(r, tx);
                    k()
                }
            }
        }
        
    ///
    /// Helper function for `withBatchingThread` doing the logging 
    ///
    def batchingThread(h1: (Unit -> Unit \ (Logger3 + NonDet + Chan + IO)) -> Unit \ IO + NonDet + Chan, timeout: Time.Duration, reciever: Receiver[LogRecord], closer: Receiver[Unit]): Unit \ IO + NonDet + Chan =
        run {
            let timeoutms = Time.Duration.toMillis(timeout) |> Int64.clampToInt32(min = 0, max = Int32.maxValue());
            region rc {
                let batch = Ref.fresh(rc, List.empty());
                let last = Ref.fresh(rc, LogRecord(
                    { aux = Map.empty(), message = RichString.fromString(""), severity = Logger.Severity.Info, timestamp = None}));
                let times = Ref.fresh(rc, 0);
                let endBatching = Ref.fresh(rc, false);
                def loop() = {
                    let chanTimeout = Channel.timeout(timeoutms, Time.TimeUnit.Milliseconds);
                    select {
                        case r <- Channel.recv(reciever) => {
                            Ref.transform((xs) -> r :: xs, batch)
                        }
                        case _ <- Channel.recv(chanTimeout) => {
                            let logs = Ref.get(batch);
                            Ref.put(List.empty(), batch);
                            logs |> List.reverse |> List.forEach(r -> {
                                let it = Ref.get(times);
                                let lr = Ref.get(last);
                                let eq = {match (r, lr) { case (
                                        LogRecord({ aux = auxx, message = rs1, severity = sx, timestamp = _ }),
                                        LogRecord({ aux = auxy, message = rs2, severity = sy, timestamp = _ })
                                    ) => auxx == auxy and rs1 == rs2 and sx == sy}}; 
                                if (it == 0) {
                                    Ref.put(r, last);
                                    Ref.put(it + 1, times);
                                    Logger3.log(r)
                                } else if (eq) {
                                    Ref.put(it + 1, times)
                                } else if (it == 1) {
                                    Ref.put(r, last);
                                    Logger3.log(r)
                                } else {
                                    Logger3.info("-- Repeated ${it}X times --");
                                    Ref.put(r, last);
                                    Logger3.log(r);
                                    Ref.put(1, times)
                                }
                            });
                            if (Ref.get(times) > 1) {
                                Logger3.info(("-- Repeated ${Ref.get(times)}X times --"))
                            };
                            Ref.put(0, times)
                        }
                        case _ <- Channel.recv(closer) => {
                            Ref.put(true, endBatching)
                        }
                    };
                    if (not List.isEmpty(Ref.get(batch)) or not Ref.get(endBatching)) {
                        loop()
                    }
                };
                loop()
            }
        } with h1

    ///
    /// Middleware for removing repeated logs. Only lets each distinct (s, m)-pair be logged once.
    ///
    pub def withNoRepeats(f: Unit -> a \ ef): a \ ef + Logger3 = 
        region rc {
            let ref: Ref[List[LogRecord], rc] = Ref.fresh(rc, Nil);
            run {
                f()
            } with handler Logger3 {
                def log(r, k) = 
                    let last = Ref.get(ref);
                    match last {
                        case Nil => 
                            Logger3.log(r);
                            Ref.put(r :: Nil, ref)
                        case xs => 
                            if (List.memberOf(r, xs)) {
                                ()
                            } else {
                                Logger3.log(r);
                                Ref.put((r) :: xs, ref)
                            }
                    };
                    k()
            }
        }

    ///
    /// Handles the `Logger` effect, by writing to a set of files seperated by a schedule.
    ///
    /// The `path` controls directory, base name and extension of versioned log files.
    ///
    pub def runWithScheduledFiles(format: d, startMillis: Int64, dur: Time.Duration, path: String, f: Unit -> b \ ef): b \ (ef - Logger3) + Time.Clock + Fs.FileWrite + Fs.MkDirs + Sys.Env + IO with ToString[d] = 
        let durationMillis = Time.Duration.toMillis(dur);
        let sep = Sys.Env.getFileSeparator();
        let split = String.findIndexOfRight(c -> c == String.charAt(0, sep), path) |> Option.map(o -> String.splitAt(o + 1, path));
        let dir = split |> Option.getWithDefault((".${sep}", "")) |> fst;
        let _ = Fs.MkDirs.mkDirs(dir);
        let file = split |> Option.getWithDefault(("", path)) |> snd;
        let fileNameSplit = file |> String.findIndexOfRight(c -> c == '.') |> Option.map(o -> String.splitAt(o, file));
        let fileBase = fileNameSplit |> Option.getWithDefault((file, "")) |> fst;
        let fileExt = fileNameSplit |> Option.getWithDefault(("", "")) |> snd;
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let now = Time.Clock.now();
                let st = now - startMillis;
                let ver = st / durationMillis;
                let newfile = "${dir}${sep}${fileBase}${ver}${fileExt}";
                let output = formatMessage(format, r);
                let _ = Fs.FileWrite.append(str = output, newfile);
                k()
            }
        }
    ///
    /// Handles the `Logger` effect, by writing to a set of files, each with a max fileSize.
    ///
    /// The `path` controls directory, base name and extension of versioned log files.
    ///
    pub def runWithFileSizeRollover(format: d, max: Fs.Size, maxFiles: Int32, path: String, f: Unit -> b \ ef): b \ (ef - Logger3) + Fs.FileWrite + Fs.FileStat + Fs.MkDirs + Sys.Env with ToString[d] =
        let sep = Sys.Env.getFileSeparator();
        let split = String.findIndexOfRight(c -> c == String.charAt(0, sep), path) |> Option.map(o -> String.splitAt(o + 1, path));
        let dir = split |> Option.getWithDefault((".${sep}", "")) |> fst;
        let _ = Fs.MkDirs.mkDirs(dir);
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let _ = run {
                    Fs.FileWrite.append(str = formatMessage(format, r), path)
                } with Fs.FileWrite.withSizeRotation(max, maxFiles);
                k()
            }
        }

    ///
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Reraises `Logger` to report errors encountered while writing. 
    ///
    pub def runWithFileWriteLog(format: d, file: String, f: Unit -> a \ ef): a \ (ef - Logger3) + Fs.FileWrite + Logger3 with ToString[d] =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let output = formatMessage(format, r);
                let res = Fs.FileWrite.append({str = output}, file);
                match res {
                    case Err(e) => /* Logger.log(s, m); */ Logger3.warn("${e}")
                    case Ok() => ()
                };
                k()
            }
        }

    ///
    /// A wrapper for a terminal handler of the `Logger` effect.
    /// Creates a handler that handles `Logger` with handler `h`,
    /// and then reraises the effect.
    ///
    pub def reraise(h: (Unit -> Unit \ Logger3) -> x \ ef1, f: Unit -> a \ ef2): a \ ef2 + ef1 + Logger3 = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = 
                let _ = run {
                    Logger3.log(r)
                } with h;
                Logger3.log(r);
                k()
        }

    pub def logWithAux(m: RichString, s: Logger.Severity, aux: Map[String, String]): Unit \ Logger3 = 
        let r = LogRecord({ 
            aux = aux,
            message = m, 
            severity = s, 
            timestamp = None 
        });
        Logger3.log(r)
}