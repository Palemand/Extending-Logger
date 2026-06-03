def trySchedule(): Unit \ Fs.FileWrite + Time.Clock + Sys.Env + Time.Sleep + IO = {
    run {
        foreach (i <- List.range(0, 1)) {            
            Logger3.info("Iteration ${i}");
            println("Currently at ${i}");
            Time.Sleep.sleep(Time.Duration.milliseconds(300))
        }
    } with Logger3.runWithScheduledFiles(Logger3.defaultFormat(), Time.Clock.now(), Time.Duration.seconds(5), "testing/schedule/anders.bertram/testlog.wdawd.txt")
    with Fs.MkDirs.runWithFileWrite
}

def tryFileSize(): Unit \ Sys.Env + Fs.FileWrite + Fs.FileStat = {
    run {
        foreach (i <- List.range(0, 10)) {            
            Logger3.info("Iteration ${i}")
        }
    } with Logger3.runWithFileSizeRollover(Logger3.defaultFormat(), Fs.Size.bytes(20i64), 100,"testing/sizes/anderserenost.txt")
    with Fs.FileSize.runWithFileStat
    with Fs.MkDirs.runWithFileWrite
}