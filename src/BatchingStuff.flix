use Severity.Info
def tryBatching(): Unit \ IO =
    run {
        Logger.info("Hej");
        Logger.info("Hej");
        Logger.info("Hej med dig");
        Logger.info("Hej")
    } with Logger2.withBatching
    with Logger.runWithIO

def what(): Unit \ IO =
    let rc1 = RichString.fromString("ABCD");
    let rc2 = RichString.fromString("1234");
    println(rc1 == rc2)