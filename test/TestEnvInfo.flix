mod TestEnvInfo {
    @Test
    pub def withEnvInfo(): Unit \ IO = 
        run {
            Logger.fatal("Hello, world")
        } 
        with Logger2.withEnvInfo
        with Logger.runWithIO
        with Sys.Env.runWithIO
}