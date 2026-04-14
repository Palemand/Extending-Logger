use Severity.Info
use RichString.RichString
use RichString.Span
def tryBatching(): Unit \ Chan + IO + NonDet =
    run {
        Logger.info("Hej");
        Logger.info("Hej");
        Logger.info("Hej med dig");
        Logger.info("Hej med dig");
        Logger.info("Hej med dig");
        Logger.info("Hej");
        Logger.info("Hej");
        Logger.info("Hej")
    } with Logger2.withBatchingThread(Logger.runWithIO, Time.Duration.milliseconds(10))


    ///
    /// Returns a `Bool` according to the equality of `rs1` and `rs2`.
    ///
    /// Checks whether text and styling is equivalent for each individual character.
    ///
    pub def equals(rs1: RichString, rs2: RichString): Bool = 
        let (RichString(chain1), RichString(chain2)) = (rs1, rs2);
        let splitSpans = (match Span.Span({text = t, fgColor = fg, bgColor = bg, styles}) -> { 
            String.toList(t) |> List.toChain |> Chain.map(c -> (Span.Span({text = Char.toString(c), fgColor = fg, bgColor = bg, styles = styles})))
        });
        let l1 = Chain.flatMap(splitSpans, chain1);
        let l2 = Chain.flatMap(splitSpans, chain2);
        if (Chain.length(l1) != Chain.length(l2)) {
            false
        } else {
            Chain.zip(l1, l2) |> Chain.forAll((match (e1, e2) -> spanEquals(e1, e2)))
        }

    def spanEquals(s1: Span, s2: Span): Bool =
        let Span.Span({text = t1, fgColor = fg1, bgColor = bg1, styles = style1}) = s1;
        let Span.Span({text = t2, fgColor = fg2, bgColor = bg2, styles = style2}) = s2;
        t1 == t2 and fg1 == fg2 and bg1 == bg2 and style1 == style2