mod Util {
    pub def withReraise(f: Unit -> a \ ef): a \ ef + Logger = 
        run {
            f()
        } with handler Logger {
            def log(s, m, k) = k(Logger.log(s, m))
        }
}