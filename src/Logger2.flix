pub mod Logger2 {
    use Sys.Console.print
    use Logger2.handleWithNoFormatting
    use Net.TcpSocket
    use Sys.{Console, Env}
    use RichString.{gray, cyan, green, yellow, red, bold, text};
    use Logger.Severity.{Warn, Fatal, Trace, Debug, Info}
    use Logger.Severity

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
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Aborts if an error is encountered while writing to file.
    ///
    pub def handleWithFileWriteAbort(file: String, f: a -> b \ ef): a -> b \ (ef - Logger) + Fs.FileWrite + Abort = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let output = "[${s}] ${RichString.toPlainString(m)}\n";
                let res = Fs.FileWrite.append({str = output}, file);
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
    /// Aborts if an error is encountered while writing to file.
    ///
    pub def runWithFileWriteAbort(file: String, f: Unit -> a \ ef): a \ (ef - Logger) + Fs.FileWrite + Abort =
        handleWithFileWriteAbort(file, f)()

    ///
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Ignores errors encountered while writing.
    ///
    pub def handleWithFileWrite(file: String, f: a -> b \ ef): a -> b \ (ef - Logger) + Fs.FileWrite = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let output = "[${s}] ${RichString.toPlainString(m)}\n";
                let _ = Fs.FileWrite.append({str = output}, file);
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Ignores errors encountered while writing.
    ///
    pub def runWithFileWrite(file: String, f: Unit -> a \ ef): a \ (ef - Logger) + Fs.FileWrite = 
        handleWithFileWrite(file, f)()
    
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
    /// Middleware handler that prepends an emoji corresponding to the severity level
    ///
    pub def withEmoji(f: Unit -> a \ ef): a \ ef + Logger = 
        run {
            f()
        } with handler Logger {
            def log(s, m, k) = 
                let e = match s {
                    case Severity.Fatal => "🔥"
                    case Severity.Warn => "⚠️"
                    case Severity.Trace => "🫆"
                    case Severity.Debug => "🐛"
                    case Severity.Info => "ℹ️"
                } + " ";
                let m_ = RichString.text(e) + m;
                k(Logger.log(s, m_))
        }

    ///
    /// Handles the Logger effect in `f` with both `h1` and `h2`. 
    ///
    pub def runWithMultisink(
        h1: (Unit -> Unit \ Logger) -> x \ ef4, 
        h2: (Unit -> Unit \ Logger) -> y \ ef5, 
        f: Unit -> a \ ef3): a \ (ef3 - Logger) + ef4 + ef5 = 
        run {
            f()
        } with handler Logger {
            def log(s, m, k) = 
                let _ = run {
                     Logger.log(s, m)
                 } with h1;
                 let _ = run {
                     Logger.log(s, m)
                 } with h2;
                k()
        }

    ///
    /// Handles the Logger effect with IO, while discarding all formatting
    ///
    pub def handleWithNoFormatting(f: a -> b \ ef): a -> b \ ef - Logger + IO = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(_, m, k) = 
                let _ = println(RichString.toPlainString(m));
                k()
        }

    ///
    /// Runs the Logger effect with IO, while discarding all formatting
    ///
    pub def runWithNoFormatting(f: Unit -> a \ ef): a \ ef - Logger + IO = 
        handleWithNoFormatting(f)() 

    ///
    /// Middleware for appending all available system-info to logs.
    ///
    pub def handleWithEnvInfo(f: a -> b \ ef): a -> b \ ef + Logger + Sys.Env = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = 
                let linesep = match Env.getLineSeparator() {
                    case "\n" => "\\n"
                    case "\r" => "\\r"
                    case _ => "Unknown"
                };
                let p = Map#{
                    "args"              => List.toString(Sys.Env.getArgs()),
                    "currentWorkingDir" => Option.getWithDefault("None", Sys.Env.getCurrentWorkingDirectory()),
                    "fileSeparator"     => Env.getFileSeparator(),
                    "lineSeparator"     => linesep,
                    "osArch"            => Option.getWithDefault("None", Env.getOsArch()),
                    "osName"            => Option.getWithDefault("None", Env.getOsName()),
                    "osVersion"         => Option.getWithDefault("None", Env.getOsVersion()),
                    "pathSeparator"     => Env.getPathSeparator(),
                    "temporaryDirectory"=> Option.getWithDefault("None", Env.getTemporaryDirectory()),
                    "userHomeDirectory" => Option.getWithDefault("None", Env.getUserHomeDirectory()),
                    "userName"          => Option.getWithDefault("None", Env.getUserName()),
                    "virtualProcessors" => Int32.toString(Env.getVirtualProcessors())
                }
                |> Map.filter(v -> v != "None")
                |> Map.foldLeftWithKey(acc -> c -> v -> "${acc}\n${c} => ${v}", "");
                let m_ = m + RichString.text("${p}");
                Logger.log(s, m_);
                k()
        }

    ///
    /// Middleware for appending all available system-info to logs.
    ///
    pub def withEnvInfo(f: Unit -> b \ ef): b \ ef + Logger + Sys.Env = 
        handleWithEnvInfo(f)()



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
                            let str = "[${s}] ${RichString.toPlainString(m)}\n";
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
    
    ///
    /// Handles the `Logger` effect, by sending it over a TCP connection.
    ///
    pub def runWithTCP(ip: Net.IpAddr, port: Int16, f: Unit -> b \ ef): b \ (ef - Logger) + Net.TcpConnect + Logger + IO = handleWithTCP(ip, port, f)()

    ///
    /// Handles the `Logger` effect, by batching multiples of the same log messages.
    ///
    // pub def withBatching(f: Unit -> a \ ef): a \ (ef - Logger) + Logger =
    //     region rc {
    //         let last = Ref.fresh(rc, (Severity.Info, RichString.empty()));
    //         let n = Ref.fresh(rc, 0);
    //         run {
    //             f()
    //         } with handler Logger {
    //             def log(s, m, k) = {
    //                 let (ls, lm) = Ref.get(last);
    //                 let cmp = s == ls and equals(m, lm);
    //                 let count = Ref.get(n);
    //                 match (cmp, count) {
    //                     case (_, 0) => {
    //                         Ref.put(1, n);
    //                         Ref.put((s, m), last);
    //                         Logger.log(s, m)
    //                     }
    //                     case (true, _) => {
    //                         Ref.put(count + 1, n)
    //                     }
    //                     case (_) if (count == 1) => {
    //                         Ref.put(1, n);
    //                         Ref.put((s, m), last);
    //                         Logger.log(s, m)
    //                     }
    //                     case _ => {
    //                         Logger.log(Info, RichString.fromString("-- Repeated ${count-1}X times --"));
    //                         Ref.put(1, n);
    //                         Ref.put((s, m), last);
    //                         Logger.log(s, m)
    //                     }
    //                 };
    //                 k()
    //             }
    //         }
    //     }
    ///
    /// Handles the `Logger` effect, by returning a string instead.
    ///
    pub def handleWithRichString(f: a -> b \ ef): a -> RichString \ (ef - Logger) = 
        x -> run {
            let _ = f(x); RichString.fromString("")
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
                let _ = k();
                output
            }
        }

    ///
    /// Handles the `Logger` effect, by returning a string instead.
    ///
    pub def runWithRichString(f: Unit -> a \ ef): RichString \ (ef - Logger) = handleWithRichString(f)()
    @Deprecated
    pub def withBatching(f: Unit -> a \ ef): a \ (ef - Logger) + Logger =
        region rc {
            let last = Ref.fresh(rc, (Severity.Info, RichString.empty()));
            let n = Ref.fresh(rc, 0);
            run {
                f()
            } with handler Logger {
                def log(s, m, k) = {
                    let (ls, lm) = Ref.get(last);
                    let cmp = s == ls and equals(m, lm);
                    let count = Ref.get(n);
                    match (cmp, count) {
                        case (_, 0) => {
                            Ref.put(1, n);
                            Ref.put((s, m), last);
                            Logger.log(s, m)
                        }
                        case (true, _) => {
                            Ref.put(count + 1, n)
                        }
                        case (_) if (count == 1) => {
                            Ref.put(1, n);
                            Ref.put((s, m), last);
                            Logger.log(s, m)
                        }
                        case _ => {
                            Logger.log(Info, RichString.fromString("-- Repeated ${count-1}X times --"));
                            Ref.put(1, n);
                            Ref.put((s, m), last);
                            Logger.log(s, m)
                        }
                    };
                    k()
                }
            }
        }

    ///
    /// Prepends a timestamp on every instance of `Logger.log`
    ///
    pub def withTimestamp(f: Unit -> a \ ef): a \ (ef - Logger) + Logger + Timestamp = 
    run {
        f()
    } with handler Logger {
        def log(s, m, k) = 
            let output = RichString.text("(${Timestamp.getTimestamp()})  ${m}");
            Logger.log(s, output);
            k()
    }
    
    ///
    /// Spawns a seperate thread for doing the logging.
    ///
    @Deprecated
    pub def withThread(h1: (Unit -> Unit \ (Logger + NonDet + Chan)) -> a \ IO + NonDet + Chan, f: Unit -> a \ ef ): a \ (ef - Logger) + Chan + NonDet + IO =
        region rc {
            let (tx, rx) = Channel.unbuffered();
            spawn loggingThread(h1, rx) @ rc;
            run {
                f()
            } with handler Logger {
                def log(s, m, k) = {
                    Channel.send((s, m), tx);
                    k()
                }
            }
        }
        
    ///
    /// Helper function for `withThread` doing the logging 
    ///
    @Deprecated
    def loggingThread(h1: (Unit -> Unit \ (Logger + NonDet + Chan)) -> a \ IO + NonDet + Chan , reciever: Receiver[(Severity, RichString)]): Unit \ IO + NonDet + Chan =
        let _x = run {
            let (s, m) = Channel.recv(reciever);
            Logger.log(s, m)
        } with h1;
        loggingThread(h1, reciever)
    
    ///
    /// Terminating handler for the `Logger` effect, that will batch multiples of the same log messages that are sent within `timeout` of each other.
    ///
    /// Uses a seperate thread for handling the batching. 
    ///
    pub def withBatchingThread(h1: (Unit -> Unit \ (Logger + NonDet + Chan + IO)) -> Unit \ IO + NonDet + Chan, timeout: Time.Duration, f: Unit -> a \ ef ): a \ (ef - Logger) + Chan + NonDet + IO =
        region rc {
            let (tx, rx) = Channel.unbuffered();
            let (closetx, closerx) = Channel.unbuffered();
            spawn batchingThread(h1, timeout, rx, closerx) @ rc;
            run {
                let res = f();
                Channel.send((), closetx);
                res
            } with handler Logger {
                def log(s, m, k) = {
                    Channel.send((s, m), tx);
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
                                let eq = s == ls and equals(m, lm);
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
    
    ///
    /// Middleware for removing repeated logs. Only lets each distinct (s, m)-pair be logged once.
    ///
    pub def withNoRepeats(f: Unit -> a \ ef): a \ ef + Logger = 
        region rc {
            let r: Ref[List[(Severity, RichString)], rc] = Ref.fresh(rc, Nil);
            run {
                f()
            } with handler Logger {
                def log(s, m, k) = 
                    let last = Ref.get(r);
                    match last {
                        case Nil => 
                            Logger.log(s, m);
                            Ref.put((s, m) :: Nil, r)
                        case xs => 
                            if (List.memberOf((s, m), xs)) {
                                ()
                            } else {
                                Logger.log(s, m);
                                Ref.put((s, m) :: xs, r)
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
    pub def handleWithScheduledFiles(startMillis: Int64, dur: Time.Duration, path: String, f: a -> b \ ef): a -> b \ (ef - Logger) + Time.Clock + Fs.FileWrite + Fs.MkDirs + Env + IO = 
        let durationMillis = Time.Duration.toMillis(dur);
        x -> {
            let sep = Env.getFileSeparator();
            let split = String.findIndexOfRight(c -> c == String.charAt(0, sep), path) |> Option.map(o -> String.splitAt(o + 1, path));
            let dir = split |> Option.getWithDefault((".${sep}", "")) |> fst;
            let _ = Fs.MkDirs.mkDirs(dir);
            let file = split |> Option.getWithDefault(("", path)) |> snd;
            let fileNameSplit = file |> String.findIndexOfRight(c -> c == '.') |> Option.map(o -> String.splitAt(o, file));
            let fileBase = fileNameSplit |> Option.getWithDefault((file, "")) |> fst;
            let fileExt = fileNameSplit |> Option.getWithDefault(("", "")) |> snd;
            run {
                f(x)
            } with handler Logger {
                def log(s, m, k) = {
                    let now = Time.Clock.now();
                    let st = now - startMillis;
                    let ver = st / durationMillis;
                    let newfile = "${dir}${sep}${fileBase}${ver}${fileExt}";
                    let output = "[${s}] ${RichString.toPlainString(m)}\n";
                    let _ = Fs.FileWrite.append(str = output, newfile);
                    k()
                }
            }
        }
    ///
    /// Handles the `Logger` effect, by writing to a set of files seperated by a schedule.
    ///
    /// The `path` controls directory, base name and extension of versioned log files.
    ///
    pub def runWithScheduledFiles(startMillis: Int64, dur: Time.Duration, path: String, f: Unit -> b \ ef): b \ (ef - Logger) + Time.Clock + Fs.FileWrite + Fs.MkDirs + Env + IO = 
        handleWithScheduledFiles(startMillis, dur, path, f)()

    ///
    /// Handles the `Logger` effect, by writing to a set of files, each with a max fileSize.
    ///
    /// The `path` controls directory, base name and extension of versioned log files.
    ///
    pub def handleWithFileSizeRollover(max: Fs.Size, maxFiles: Int32, path: String, f: a -> b \ ef): (a -> b \ (ef - Logger) + Fs.FileWrite + Fs.FileStat + Fs.MkDirs + Env) = 
        x -> {
            let sep = Env.getFileSeparator();
            let split = String.findIndexOfRight(c -> c == String.charAt(0, sep), path) |> Option.map(o -> String.splitAt(o + 1, path));
            let dir = split |> Option.getWithDefault((".${sep}", "")) |> fst;
            let _ = Fs.MkDirs.mkDirs(dir);
            run {
                f(x)
            } with handler Logger {
                def log(s, m, k) = {
                    let _ = run {
                        Fs.FileWrite.append(str = "[${s}] ${RichString.toPlainString(m)}\n", path)
                    } with Fs.FileWrite.withSizeRotation(max, maxFiles);
                    k()
                }
            }
        }
    ///
    /// Handles the `Logger` effect, by writing to a set of files, each with a max fileSize.
    ///
    /// The `path` controls directory, base name and extension of versioned log files.
    ///
    pub def runWithFileSizeRollover(max: Fs.Size, maxFiles: Int32, path: String, f: Unit -> b \ ef): b \ (ef - Logger) + Fs.FileWrite + Fs.FileStat + Fs.MkDirs + Env = handleWithFileSizeRollover(max, maxFiles, path, f)()
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Reraises `Logger` to report errors encountered while writing. 
    ///
    pub def handleWithFileWriteLog(file: String, f: a -> b \ ef1): a -> b \ (ef1 - Logger) + Fs.FileWrite + Logger = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let output = "[${s}] ${RichString.toPlainString(m)}\n";
                let res = Fs.FileWrite.append({str = output}, file);
                match res {
                    case Err(e) => /* Logger.log(s, m); */ Logger.warn("${e}")
                    case Ok() => ()
                };
                k()
            }
        }

    ///
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Reraises `Logger` to report errors encountered while writing. 
    ///
    pub def runWithFileWriteLog(file: String, f: Unit -> a \ ef): a \ (ef - Logger) + Fs.FileWrite + Logger =
        handleWithFileWriteLog(file, f)()

    ///
    /// A wrapper for a terminal handler of the `Logger` effect.
    /// Creates a handler that handles `Logger` with handler `h`,
    /// and then reraises the effect.
    ///
    pub def reraise(h: (Unit -> Unit \ Logger) -> x \ ef1, f: Unit -> a \ ef2): a \ ef1 + ef2 + Logger = 
        run {
            f()
        } with handler Logger {
            def log(s, m, k) = 
                let _ = run {
                    Logger.log(s, m)
                } with h;
                Logger.log(s, m);
                k()
        }
        
}