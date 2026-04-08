eff Timestamp {
    def getTS(): String
}

pub mod Timestamp {
    pub enum TsFmtLst(List[TsFormat]) /* with ToString */

    pub enum TsFormat /* with ToString??? */ {
        case Day(Day),
        case Month(Month),
        case Sep(String)
    }

    pub enum Day /* with ToString??? */ {
        case DOY,   //day of year
        case DOW,   //day of week as number
        case WD     //day of week as text
    }

    pub enum Month /* with ToString??? */ {
        case MAN,   //month as number
        case MAT     //month as text
    }

}