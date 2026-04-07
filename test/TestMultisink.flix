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
    
    @Test
    def runWithMultisink02(): Unit \ IO = 
        let h1 = Logger2.runWithMultisink(
            Util.withReraise, 
            Logger2.withEmoji
        );
        let h2 = Logger2.runWithMultisink(
            Util.withReraise, h1
        );
        let h3 = Logger2.runWithMultisink(
            Logger2.withEmoji, h2
        );
        let _ = run {
            Logger.fatal("Hello, world")
        } with h3
        with Logger.runWithIO;
        ()
}