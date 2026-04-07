use Net.Ipv4Addr
use Net.TcpConnect
use Net.TcpSocket
use Net.TcpAccept
use Net.TcpBind
def runTcp(): Unit \ IO = 
    run {
        let ip = Net.IpAddr.V4(Ipv4Addr.localhost());
        let server = Result.getOrAbort(TcpBind.bind(ip, 32001i16));
        region rc {
            spawn host(server) @ rc;
            run {
                Logger2.info("helloes over tcp")
            } with Logger2.runWithTCP(ip, 32001i16)
            with TcpConnect.runWithIO
            with Logger2.runWithIgnore
        }
    } with TcpBind.runWithIO
    with Abort.runWithIO


def host(server: Net.TcpServer): Unit \ IO =
    run {
        let sock = Result.getOrAbort(Net.TcpAccept.accept(server));
        region rc {
            let buf = BufReader.withCapacity(rc, 64, sock);
            let res = Result.getOrAbort(Peekable.readWhile(c -> c != 10i8, buf));
            let str = String.fromBytes(res);
            println(str)
        }
    } with TcpAccept.runWithIO
    with Abort.runWithIO

def main(): Unit \ IO = runTcp()