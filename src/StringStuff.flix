def runStringThingy(): Unit \ IO = 
    let str = run {
        Logger.info("Houston, we got a problem.");
        Logger.info("I love gamlbing")
    } with Logger2.runWithString;
    println(str)

def runRichStringThingy(): Unit \ IO = 
    let rs = run {
        Logger.info("Houston, we got a problem.");
        Logger.info("I love gamlbing")
    } with Logger2.runWithRichString;
    println(rs)