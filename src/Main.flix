use Logger3.{reraise, runWithMultisink, runWithStdOut, runWithStdErr, defaultFormat, withSeverityFilter, withNoRepeats}
use Logger.Severity.{Debug, Info, Warn, Fatal, Trace}

def main(): Unit \ IO = 
    run {
        Logger3.debug("hello, world")
    } with Logger3.reraise(Logger3.runWithIO)
    with Logger3.withFilter(_ -> true)
    with Logger3.runWithIO

def branchingHandlersReraise(): Unit \ {Sys.Console} = 
    run {
        Logger3.warn("hello, world")
    } with Logger3.reraise(
        f -> run {
            f()
        } with withSeverityFilter(Warn)
        with runWithStdErr(RichString.red(defaultFormat()))
    ) with withNoRepeats
    with runWithStdOut(defaultFormat())

def branchingHandlers(): Unit \ {Sys.Console} = 
    run {
        Logger3.warn("hello, world")
    } with runWithMultisink(
        f -> run {
            f()
        } with withSeverityFilter(Warn)
        with runWithStdErr(RichString.red(defaultFormat())),
        f -> run {
            f()
        } with withNoRepeats
        with runWithStdOut(defaultFormat())
    ) 