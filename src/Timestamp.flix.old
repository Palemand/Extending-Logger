eff Timestamp {

    def getDayOfMonth(): String
    def getMonthOfYear(): String
    def getYear(): String
    def getSecondOfHour(): String
    def getMinuteOfHour(): String
    def getHourOfDay(): String

}


mod Timestamp {
    use Time.Duration.minutes
    import java.time.{ZonedDateTime => JZonedDateTime}
    import java.lang.{Long => JLong}

    pub def getDayStr(): String \ IO = 
        let zonedDateTimeJObj = JZonedDateTime.now();
        let dayOfWeekJObj = zonedDateTimeJObj.getDayOfWeek();
        let dayOfWeekStr = match dayOfWeekJObj.getValue() {
            case 1 => "mon"
            case 2 => "tues"
            case 3 => "wed"
            case 4 => "thur"
            case 5 => "fri"
            case 6 => "sat"
            case 7 => "sun"
            case _ => unreachable!()
        };
        dayOfWeekStr

    pub def getTimestamp(): String \ Timestamp = 
        let day = Timestamp.getDayOfMonth();
        let month = Timestamp.getMonthOfYear();
        let year = Timestamp.getYear();
        let sec = Timestamp.getSecondOfHour();
        let min = Timestamp.getMinuteOfHour();
        let hour = Timestamp.getHourOfDay();
        let tstamp = "${day}-${month}-${year} ${hour}:${min}:${sec}";
        tstamp

    pub def handle(f: a -> b \ ef): a -> b \ (ef - Timestamp) + IO = x -> 
        run {
            f(x)
        } with handler Timestamp {
            def getDayOfMonth (k) = 
                let zonedDateTimeJObj = JZonedDateTime.now();
                let day = zonedDateTimeJObj.getDayOfMonth();
                let dayStr = if (day < 10) "0${day}" else "${day}";
                k(dayStr)
            def getMonthOfYear(k) = 
                let zonedDateTimeJObj = JZonedDateTime.now();
                let month = zonedDateTimeJObj.getMonthValue();
                let monthStr = if (month < 10) "0${month}" else "${month}";
                k(monthStr)
            def getYear(k) = 
                let zonedDateTimeJObj = JZonedDateTime.now();
                let year = zonedDateTimeJObj.getYear();
                k("${year}")
            def getSecondOfHour(k) =
                let zonedDateTimeJObj = JZonedDateTime.now();
                let sec = zonedDateTimeJObj.getSecond();
                let secStr = if (sec < 10) "0${sec}" else "${sec}";
                k(secStr)
            def getMinuteOfHour(k) = 
                let zonedDateTimeJObj = JZonedDateTime.now();
                let minute = zonedDateTimeJObj.getMinute();
                let minuteStr = if (minute < 10) "0${minute}" else "${minute}";
                k(minuteStr)
            def getHourOfDay(k) =    
                let zonedDateTimeJObj = JZonedDateTime.now();
                let hour = zonedDateTimeJObj.getHour();
                let hourStr = if (hour < 10) "0${hour}" else "${hour}";
                k(hourStr)
        }
    
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Timestamp) + IO = handle(f)()
}