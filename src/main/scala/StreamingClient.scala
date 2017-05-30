package dnsclient


object NettyClientTest {

  import scalaz.NonEmptyList
  import scalaz.syntax.either._
  import java.net.InetSocketAddress
  import dnsclient._
  import Data._
  import scalaz.concurrent.Task
  import scalaz._
  import scodec.Err
  import Data._
  import DnsCodec._ 
  import Util._


  val socketAddress = new InetSocketAddress("128.138.129.76", 53) 
  val request1 = (socketAddress, dnsRequest(1234, DnsString(NonEmptyList("msn", List("com"): _*)))).right[Err] 
  val request3 = (new InetSocketAddress("128.138.129.76", 53),  dnsRequest(1234, DnsString(NonEmptyList("github", List("com"): _*)))).right[Err]  

  val iso = CodecLenses.codecToBytes[DnsPacket].reverse
  
  val f = (NettyHandler.getNettyClient).choiceRight[Err]
  ???  
}
