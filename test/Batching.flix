use Time.Duration
def batchingTest(): Unit \ IO + NonDet + Chan = 
    run {
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej");
        Logger3.info("hej")
    } with Logger3.runWithBatchingThread(Logger3.runWithIO, Time.Duration.milliseconds(1))