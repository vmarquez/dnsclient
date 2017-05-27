package dnsclient

import scodec.{bits => _, _}
import scodec.codecs._
import scodec.bits._
import scalaz.stream._
import scalaz.concurrent.Task
import java.net.InetSocketAddress
import java.util.concurrent.CountDownLatch
import scala.concurrent.duration._
import scalaz.syntax.std.option._
import scalaz.NonEmptyList


object NettyClientTest {

  import scalaz.NonEmptyList
  import scalaz.syntax.either._
  import java.net.InetSocketAddress
  import dnsclient._
  import Data._
  import scalaz.concurrent.Task
  import scalaz._
  import scodec.Err
  val socketAddr = new InetSocketAddress("128.138.129.76", 53) 
  val socketAddress = new InetSocketAddress("8.8.8.8", 53)
  val request2 = (socketAddress, dnsRequest(1234, DnsString(NonEmptyList("scala", List("org"): _*)))).right[Err] 
  val request1 = (socketAddress, dnsRequest(1234, DnsString(NonEmptyList("msn", List("com"): _*)))).right[Err] 

  val f = NettyHandler.sendPacket
  f(t => Task.now(println(t))).flatMap(ff => ff(request1)) 
  val dns = dnsRequest(1234, DnsString(NonEmptyList("scala", List("org"): _*)))

}
