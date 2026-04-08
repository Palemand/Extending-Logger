def trySchedule(): Unit \ Fs.FileWrite + Time.Clock + Sys.Env + Time.Sleep + IO = {
    run {
        foreach (i <- List.range(0, 50)) {            
            Logger.info("Iteration ${i}");
            println("Currently at ${i}");
            Time.Sleep.sleep(Time.Duration.milliseconds(300))
        }
    } with Logger2.runWithScheduledFiles(Time.Clock.now(), Time.Duration.seconds(5), "schedules")
}
