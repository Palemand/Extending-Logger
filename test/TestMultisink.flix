mod TestMultiSink {
    @Test
    def runWithMultisink01(): Unit \ IO = 
        let _ = run {
            Logger.fatal("Hello, world")
        } with Logger2.runWithMultisink(
            Util.withReraise, 
            Logger2.withEmoji
        )
        with Logger.runWithIO;
        ()
}