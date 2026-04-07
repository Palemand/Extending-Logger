def threadstarter(): Unit \ Chan =
    run {
        Logger.info("Hej")
    } with Logger2.withThread(Logger.runWithIO)