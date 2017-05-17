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

object Test {
  import DnsCodec._
  import Data._

  def client(nel: NonEmptyList[String]) = udp.listen(8080) {
    udp.eval_(Task.delay { println("hello world") }) ++
    (for {
      request <- asProcess(dnsPacketCodec.encode(dnsRequest(30144, DnsString(nel))))
      _ <- udp.send(to = l3dns, request.bytes)
      _ = println("ok we sent")
      //packet <- udp.receive(maxSize)
      packet <- udp.receive(1024, 20.seconds.some)
      _ = println("recieved")
      //_  = println(packet.bytes.bits.toList.map(_.toHex))
      _  = println(packet.bytes.bits.toByteVector)

      response <- asProcess(dnsPacketCodec.decode(packet.bytes.bits))
      _ = println("response = " + response)
    } yield List(response))
  }
  val l3dns = new InetSocketAddress("8.8.8.8", 53)
  
  def asProcess[T](attempt: Attempt[T]): Process0[T] = 
    Process.emit(
      attempt.fold(
        f => throw new Exception(f.messageWithContext),
        v => v       
      )
    )
}


object NettyClientTest {

  import scalaz.NonEmptyList
  import scalaz.syntax.either._
  import java.net.InetSocketAddress
  import dnsclient._
  import Data._
  import scalaz.concurrent.Task

  val socketAddress = new InetSocketAddress("8.8.8.8", 53) 
  (socketAddress, dnsRequest(1234, DnsString(NonEmptyList("reddit", List("com"): _*)))).right[Err] 

}
