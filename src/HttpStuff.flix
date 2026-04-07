def abcd(): Unit \ IO =
    let res = run {
        Logger2.info("Something wrong");
        ""
    } with Logger2.runWithHttp("notawebsite.not")
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