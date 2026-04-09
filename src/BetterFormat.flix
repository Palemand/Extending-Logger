use Logger2.{><, sev, msg}
def pretty(): Unit \ IO = 
    run {
        Logger.fatal("This feels fatal")
    } with Logger2.withCustomFormat((s, m) -> RichString.text("\"${m}\" has been logged with severity-level: ${s}"))
    with Logger2.runWithIOButDifferentFromTheOtherRunWithIO

def prettyAndTimestampy(): Unit \ IO + Timestamp = 
    let _ = run {
        Logger.info("This is info")
    } 
    with Logger2.withStdFormat
    with Logger2.withMoreCustomFormat((_, m, p) -> RichString.text("${p} ${m}"), Timestamp.getTimestamp())
    with Logger2.runWithIOButDifferentFromTheOtherRunWithIO;
    ()

def formatOp(): Unit \ IO = 
    let fmt1: Severity -> RichString -> String = s -> m -> ((("string1" >< "string2")(s)) >< "string3")(m);
    let fmt2 = "string1" `sev` "string2" `msg` "string3";
    let fmt3 = s -> m -> "string1${s}string2${m}string3${}";
    let fmt4: String -> String = m -> "${m}${}";
    println(fmt1(Severity.Debug, RichString.text("h1")));
    println(fmt2(Severity.Debug, RichString.text("h2")));
    println(fmt3(Severity.Debug, RichString.text("h3")));
    println(fmt4("Hello world"))
