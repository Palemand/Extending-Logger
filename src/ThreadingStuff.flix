def threadstarter(): Unit \ IO + NonDet + Chan = 
    run {
        foreach (i <- List.range(0, 1000)) {
            Logger.info("This is the ${i}th call")
        }
    } with Logger2.withThread(Logger.runWithIO)