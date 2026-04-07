pub mod Logger2 {
    use Net.TcpSocket
    use Sys.Console
    use RichString.{gray, cyan, green, yellow, red, bold, text};
    use Severity.{Warn, Fatal, Trace, Debug, Info}

    ///
    /// Logs the message `m` at the `Trace` level.
    ///
    pub def trace(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Trace, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Debug` level.
    ///
    pub def debug(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Debug, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Info` level.
    ///
    pub def info(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Info, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Warn` level.
    ///
    pub def warn(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Warn, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Fatal` level.
    ///
    pub def fatal(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Fatal, Formattable.format(m))

    ///
    /// Handles the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Logger) + IO = x ->
        run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let prefix = match s {
                    case Trace => gray("[Trace]")
                    case Debug => cyan("[Debug]")
                    case Info  => green("[Info]")
                    case Warn  => yellow("[Warn]")
                    case Fatal => bold(red("[Fatal]"))
                };
                let output = prefix + text(" ") + m;
                println(RichString.toString(output));
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `IO` effect.
    ///
    // @DefaultHandler
    // pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Logger) + IO = handle(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard out.
    ///
    pub def handleWithStdOut(f: a -> b \ ef): (a -> b \ ef - Logger + Console) = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let prefix = match s {
                    case Severity.Debug => cyan("[Debug]")
                    case Severity.Fatal => bold(red("[Fatal]"))
                    case Severity.Info  => green("[Info]")
                    case Severity.Trace => gray("[Trace]")
                    case Severity.Warn  => yellow("[Warn]")
                };
                let output = prefix + text(" ") + m;
                Console.println(RichString.toString(output));
                k()
            }
        }
    
    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard out.
    ///
    pub def runWithStdOut(f: Unit -> a \ ef): a \ ef - Logger + Console = handleWithStdOut(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard error.
    ///
    pub def handleWithStdErr(f: a -> b \ ef): (a -> b \ ef - Logger + Console) = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let prefix = match s {
                    case Severity.Debug => cyan("[Debug]")
                    case Severity.Fatal => bold(red("[Fatal]"))
                    case Severity.Info  => green("[Info]")
                    case Severity.Trace => gray("[Trace]")
                    case Severity.Warn  => yellow("[Warn]")
                };
                let output = prefix + text(" ") + m;
                Console.eprintln(RichString.toString(output));
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard error.
    ///
    pub def runWithStdErr(f: Unit -> a \ ef): a \ ef - Logger + Console = handleWithStdErr(f)()

    ///
    /// Aborts with the message `m` from `Logger.log` if given function `g` returns true.
    /// Otherwise, lets `Logger` effect propagate.
    ///
    pub def handleWithAbort(g: (Severity, RichString) -> Bool \ ef1, f: a -> b \ ef2): a -> b \ ef1 + ef2 + Abort + Logger = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) =
                if (g(s, m)) {
                    Abort.abort(m)
                } else {
                    Logger.log(s, m); k()
                }
            }

    ///
    /// Aborts with the message `m` from `Logger.log` if given function `g` returns true.
    /// Otherwise, lets `Logger` effect propagate.
    ///
    pub def runWithAbort(g: (Severity, RichString) -> Bool \ ef1, f: Unit -> a \ ef2): a \ ef1 + ef2 + Abort + Logger = 
        handleWithAbort(g, f)() 

    ///
    /// Handles the `Logger` effect of the given function `f`.
    ///
    /// In other words, returns a list of `(Severity, RichString)` that represents and is ordered by the calls to log.
    ///
    pub def handleWithList(f: a -> b \ ef): a -> (b, List[(Severity, RichString)]) \ (ef - Logger) =
        x -> run {
            (f(x), List.empty())
        } with handler Logger {
            def log(s, m, k) = {
                let (r, l) = k();
                (r, (s, m) :: l)
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, returns a list of `(Severity, RichString)` that represents and is ordered by the calls to log.
    ///
    pub def runWithList(f: Unit -> b \ ef): (b, List[(Severity, RichString)]) \ (ef - Logger) = handleWithList(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`.
    ///
    /// Filters the logging, by the `filter` function.
    ///
    pub def handleWithFilter(filter: (Severity, RichString) -> Bool \ ef1, f: a -> b \ ef2): a -> b \ (ef2 - Logger) + Logger + ef1 = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                if (filter(s, m)) {
                    Logger.log(s, m);
                    k()
                } else {
                    k()
                }
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// Filters the logging, by the `filter` function.
    ///
    pub def runWithFilter(filter: (Severity, RichString) -> Bool \ ef1, f: Unit -> b \ ef2): b \ (ef2 - Logger) + Logger + ef1 = handleWithFilter(filter, f)()
    
    ///
    /// Filters logs with greater or equal severity level
    ///
    pub def runWithSeverityFilter(s: Severity, f: Unit -> b \ ef): b \ (ef - Logger) + Logger =
        run {
            f()
        } with runWithFilter((sev, _) -> Order.greaterEqual(sev, s))


    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    /// 
    /// Raises a new `Logger` effect for fallback incase Http fails.
    ///
    pub def handleWithHttp(url: String, f: a -> b \ ef1): a -> b \ (ef1 - Logger) + Net.Http + Logger = 
    x -> run {
        f(x)
    } with handler Logger {
        def log(s, m, k) = {
            let res = Net.Http.post(url, "[" + ToString.toString(s) + "] " + RichString.toPlainString(m));
            match res {
                case Ok(_response) => k()
                case Err(_t) => {
                    Logger.warn(RichString.fromString("Logging to http failed. Using fallback"));
                    Logger.log(s, m);
                    k()
                }
            }
        }
    }

    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    ///
    pub def runWithHttp(url: String, f: Unit -> b \ ef1 ): b \ (ef1 - Logger) + Net.Http = 
        run {
            handleWithHttp(url, f)()
        } with runWithIgnore

    ///
    /// Handles the `Logger` effect by sending a post request with the `Http` effect.
    ///
    /// Raises a new `Logger` effect for fallback incase Http fails.
    ///
    pub def runWithHttpFallback(url: String, f: Unit -> b \ ef1 ): b \ (ef1 - Logger) + Net.Http + Logger = handleWithHttp(url, f)()


    ///
    /// Handles the `Logger` effect, by ignoring it.
    ///
    pub def runWithIgnore(f: Unit -> b \ ef): b \ (ef - Logger) = 
        run {
            f()
        } with handler Logger {
            def log(_s, _m, k) = k()
        }

    ///
    /// Handles the `Logger` effect, by sending it over a TCP connection.
    ///
    pub def handleWithTCP(ip: Net.IpAddr, port: Int16, f: a -> b \ ef): a -> b \ (ef - Logger) + Net.TcpConnect + Logger + IO = {
        x -> {
            let resSock = Net.TcpConnect.connect(ip, port);
            run {
                f(x)
            } with handler Logger {
                def log(s, m, k) = {
                    match resSock {
                        case Err(_t) => Logger.log(s, m); k()
                        case Ok(sock) => {
                            let str = "[" + ToString.toString(s)+ "] " + RichString.toPlainString(m) + "\n";
                            region rc {
                                let bytes = String.toBytes(str);
                                let arr = Vector.toArray(rc, bytes);
                                let res = Net.TcpSocket.write(arr, sock);
                                match res {
                                    case Err(_t) => Logger.log(s, m)
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

    pub def runWithTCP(ip: Net.IpAddr, port: Int16, f: Unit -> b \ ef): b \ (ef - Logger) + Net.TcpConnect + Logger + IO = handleWithTCP(ip, port, f)()
}