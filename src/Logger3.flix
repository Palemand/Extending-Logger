eff Logger3 {
    def log(r: Logger3.LogRecord): Unit
}

mod Logger3 {
    use Logger2.runWithHttpFallback
    use Logger3.LogRecord
    import java.lang.ProcessHandle;
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

    def logSeverity(s: Severity, m: String): Unit \ Logger3 = 
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
    pub def trace(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Trace, m)
    ///
    /// Logs the message `m` at the `Debug` level.
    ///
    pub def debug(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Debug, m)
    ///
    /// Logs the message `m` at the `Info` level.
    ///
    pub def info(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Info, m)
    ///
    /// Logs the message `m` at the `Warn` level.
    ///
    pub def warn(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Warn, m)
    ///
    /// Logs the message `m` at the `Fatal` level.
    ///
    pub def fatal(m: String): Unit \ Logger3 = 
        logSeverity(Severity.Fatal, m)
    ///
    /// Default format for log events.
    ///
    pub def defaultFormat(): String = 
        "%timestamp[%severity] %msg"
    ///
    /// Logs an event given all the auxillery information to fulfill the commonlog format.
    ///
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
    ///
    /// IO handler that uses the commonlog format.
    ///
    pub def runWithCommonLogFormat(f: Unit -> a \ ef): a \ (ef - Logger3) + IO = 
        runWithIOFmt(commonLogFormat(), f)
    ///
    /// Commonlog format.
    ///
    pub def commonLogFormat(): String = 
        "%ip - %name [%timestamp] \"%msg\" %status %size"

    ///
    /// IO handler that uses the syslog format.
    ///
    pub def runWithSyslogFormat(f: Unit -> a \ ef): a \ (ef - Logger3) + IO =
        runWithIOFmt(syslogFormat(), f)

    ///
    /// Syslog format.
    ///
    pub def syslogFormat(): String = 
        "<%priority>%timestamp %hostname %application[%pid]: %msg"

    ///
    /// Handles the `Logger` effect of the given function `f`, printing to StdOut.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    pub def handleWithStdOut(format: String, f: Unit -> b \ ef): b \ ef - Logger3 + Sys.Console = 
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
    pub def handleWithStdErr(format: String, f: Unit -> b \ ef): b \ ef - Logger3 + Sys.Console = 
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
    pub def runWithFileWriteAbort(format: String, file: String, f: Unit -> a \ ef): a \ (ef - Logger3) + Fs.FileWrite + Abort =
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
    /// Runs the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Ignores errors encountered while writing.
    ///
    pub def runWithFileWrite(format: String, file: String, f: Unit -> a \ ef): a \ (ef - Logger3) + Fs.FileWrite = 
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let rs = formatMessage(format, r);
                let _ = Fs.FileWrite.append({str = rs}, file);
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
    pub def runWithHttpFallback(format: String, url: String, f: Unit -> b \ ef1 ): b \ (ef1 - Logger3) + Net.Http + Logger3 =
        run {
            f()
        } with handler Logger3 {
            def log(r, k) = {
                let res = Net.Http.post(url, formatMessage(format, r));
                match res {
                    case Ok(_response) => k()
                    case Err(_t) => {
                        Logger3.warn("Logging to http failed. Using fallback");
                        Logger3.log(r);
                        k()
                    }
                }
            }
        }
    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    /// Silently fails if http fails.
    ///
    pub def runWithHttp(format: String, url: String, f: Unit -> b \ ef1 ): b \ (ef1 - Logger3) + Net.Http = 
        run {
            f()
        } with Logger3.runWithHttpFallback(format, url)
        with Logger3.runWithIgnore
    
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
    /// Handles the `Logger` effect, by sending it over a TCP connection.
    ///
    pub def runWithTCP(format: String, ip: Net.IpAddr, port: Int16, f: Unit -> b \ ef): b \ (ef - Logger3) + Net.TcpConnect + Logger3 + IO =
        let resSock = Net.TcpConnect.connect(ip, port);
            run {
                f()
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
            } with handler Logger {
                def log(r, k) = {
                    Channel.send(r, tx);
                    k()
                }
            }
        }
        
    ///
    /// Helper function for `withBatchingThread` doing the logging 
    ///
    def batchingThread(h1: (Unit -> Unit \ (Logger + NonDet + Chan + IO)) -> Unit \ IO + NonDet + Chan, timeout: Time.Duration, reciever: Receiver[(Severity, RichString)], closer: Receiver[Unit]): Unit \ IO + NonDet + Chan =
        run {
            let timeoutms = Time.Duration.toMillis(timeout) |> Int64.clampToInt32(min = 0, max = Int32.maxValue());
            region rc {
                let batch = Ref.fresh(rc, List.empty());
                let last = Ref.fresh(rc, (Severity.Info, RichString.empty()));
                let times = Ref.fresh(rc, 0);
                let endBatching = Ref.fresh(rc, false);
                def loop() = {
                    let chanTimeout = Channel.timeout(timeoutms, Time.TimeUnit.Milliseconds);
                    select {
                        case pair <- Channel.recv(reciever) => {
                            let (s, m) = pair;
                            Ref.transform((xs) -> (s, m) :: xs, batch)
                        }
                        case _ <- Channel.recv(chanTimeout) => {
                            let logs = Ref.get(batch);
                            Ref.put(List.empty(), batch);
                            logs |> List.reverse |> List.forEach(match (s, m) -> {
                                let it = Ref.get(times);
                                let (ls, lm) = Ref.get(last);
                                let eq = s == ls and (m == lm);
                                if (it == 0) {
                                    Ref.put((s, m), last);
                                    Ref.put(it + 1, times);
                                    Logger.log(s, m)
                                } else if (eq) {
                                    Ref.put(it + 1, times)
                                } else if (it == 1) {
                                    Ref.put((s, m), last);
                                    Logger.log(s, m)
                                } else {
                                    Logger.log(Info, RichString.fromString("-- Repeated ${it}X times --"));
                                    Ref.put((s, m), last);
                                    Logger.log(s, m);
                                    Ref.put(1, times)
                                }
                            });
                            if (Ref.get(times) > 1) {
                                Logger.log(Info, RichString.fromString("-- Repeated ${Ref.get(times)}X times --"))
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
}