mod TestNoRepeats {
    @Test
    def withNoRepeats01(): Unit \ Assert = 
        let (_, l) = run {
            Logger.fatal("Hej");
            Logger.fatal("Hej");
            Logger.fatal("Hej");
            Logger.fatal("Hej");
            Logger.fatal("Hej");
            Logger.fatal("Hej");
            Logger.info("Hej");
            Logger.fatal("Hej")
        } with Logger2.withNoRepeats
        with Logger.runWithList;
        Assert.assertEq(expected = (Severity.Fatal, RichString.text("Hej")) :: (Severity.Info, RichString.text("Hej")) :: Nil, l)
}