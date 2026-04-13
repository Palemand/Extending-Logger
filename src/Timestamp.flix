eff Timestamp {
    def getTS(): Timestamp.TsFmtLst
}

pub mod Timestamp {
    use Timestamp.TsFormat
    
    //////////////////////////////
    /// Enums
    //////////////////////////////
    pub enum TsFmtLst(List[TsFormat]) /* with ToString */

    pub enum TsFormat /* with ToString??? */ {
        case Day(Day),
        case Month(Month),
        case Sep(String)
    }

    pub enum Day /* with ToString??? */ {
        case DOY(Int32),    //day of year
        case DOW(Int32),    //day of week as number
        case WD(String)     //day of week as text
    }

    pub enum Month /* with ToString??? */ {
        case MAN(Int32),    //month as number
        case MAT(String)    //month as text
    }

    //////////////////////////////
    /// Handlers
    //////////////////////////////

    pub def withTSFormat(fmt: String, f: a -> b \ ef): a -> b \ (ef - Timestamp) + IO = x -> 
        run {
            f(x)
        } with handler Timestamp {
            def getTS(k) = 
                let fmtLst = String.toList(fmt);
                let resLst = List#{};
                let res = ???;
                k(res)
        }

}