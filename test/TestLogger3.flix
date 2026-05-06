mod TestLogger3{
    import java.util.regex.Pattern

    use Net.Ipv4Addr
    use Net.IpAddr

    pub def runWithIO00(): Unit \ IO = 
        run {
            Logger3.fatal("Hello, world")
        } 
        with Logger3.runWithIO

    // prints [Fatal] Hello, world
    // Ignores Timestamp, as it is not in std. format
    pub def runWithIO01(): Unit \ IO = 
        run {
            Logger3.fatal("Hello, world")
        } 
        with Logger3.withTimestamp
        with Logger3.runWithIO
        with Timestamp.runWithIO

    // prints [Fatal] Hello, world
    pub def runWithIO02(): Unit \ IO = 
        run {
            Logger3.fatal("%test")
        } 
        with Logger3.withAux(Map#{"%test" => "Hello, world"})
        with Logger3.runWithIO

    // prints a message formatted as a apache common-log
    // currently, missing correct formatting of timestamp
    pub def runWithIO03(): Unit \ IO = 
        run {
            Logger3.fatal("GET /server-status HTTP/1.1")
        } with Logger3.withTimestamp
        with Logger3.runWithCommonLogFormat
        with Timestamp.runWithIO

    // prints [Fatal] Hello, world [Fatal]
    // where Fatal is bolded and red
    // pub def runWithIO04(): Unit \ IO = 
    //     run {
    //         Logger3.fatal("Hello, world")
    //     } 
    //     with Logger3.withCustomFormat("[%severity] %msg [%severity]")
    //     with Logger3.withRichSeverity
    //     with Logger3.runWithIO

    // prints a message formatted as syslog
    // pub def runWithIO05(): Unit \ IO = 
    //     run {
    //         Logger3.fatal("Failed password for user root")
    //     }
    //     with Logger3.withTimestamp 
    //     with Logger3.withSyslogFormat(34, "my-server", "sshd", 1234)
    //     with Timestamp.runWithIO
    //     with Logger3.runWithIO

    // pub def runWithIO06(): Unit \ IO = 
    //     run {
    //         Logger3.fatal("Hello, world")
    //     }
    //     with Logger3.withCustomFormat("\\%severity")
    //     with Logger3.runWithIO

    pub def runWithIO07(): Unit \ IO = 
        run {
            let ip = IpAddr.V4(Ipv4Addr.localhost());
            Logger3.logCommonLog(ip, "Scott", "GET /server-status HTTP 1.1", 200, 2326, None)
        } 
        with Logger3.runWithIO
        with Timestamp.runWithIO

    // pub def runWithIO08(): Unit \ IO = 
    //     run {
    //         Logger3.trace("Hello, world")
    //     } 
    //     with Logger3.withCommonLogFormat
    //     with Logger3.withOldLogger
    //     with Logger2.runWithFileWrite(".test/test.log")
    //     with Fs.FileWrite.runWithIO
    //     with Timestamp.runWithIO
    //     with Logger3.runWithIO
}