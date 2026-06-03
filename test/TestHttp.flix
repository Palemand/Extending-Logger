def abcd(): Unit \ IO =
    let res = run {
        Logger3.info("Something wrong");
        ""
    } with Logger3.runWithHttp(Logger3.defaultFormat(), "notawebsite.not")
    with handler Net.Http {
        def request(req, _k) = {
            let txt = match Net.HttpRequest.body(req) {
                case Some(tx) => tx
                case None => "???"
            };
            txt
        }
    };
    println(res)