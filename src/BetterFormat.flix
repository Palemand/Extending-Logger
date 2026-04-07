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