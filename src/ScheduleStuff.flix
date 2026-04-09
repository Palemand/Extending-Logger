def trySchedule(): Unit \ Fs.FileWrite + Time.Clock + Sys.Env + Time.Sleep + IO = {
    run {
        foreach (i <- List.range(0, 1)) {            
            Logger.info("Iteration ${i}");
            println("Currently at ${i}");
            Time.Sleep.sleep(Time.Duration.milliseconds(300))
        }
    } with Logger2.runWithScheduledFiles(Time.Clock.now(), Time.Duration.seconds(5), "testing/schedule/anders.bertram/testlog.")
    with Fs.MkDirs.runWithFileWrite
}

def tryFileSize(): Unit \ Sys.Env + Fs.FileWrite + Fs.FileStat = {
    run {
        foreach (i <- List.range(0, 10)) {            
            Logger.info("Iteration ${i}")
        }
    } with Logger2.runWithFileSizeRollover(Fs.Size.bytes(20i64), "sizes/anderserenost.txt")
    with Fs.FileSize.runWithFileStat
    with Fs.MkDirs.runWithFileWrite
}