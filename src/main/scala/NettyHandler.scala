package dnsclient

import io.netty.buffer.Unpooled
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.socket.DatagramPacket
import scalaz.{\/, -\/, \/-}
import scalaz.concurrent.Task
import scala.concurrent.duration._
import java.net.InetSocketAddress



object NettyHandler {
  import io.netty.bootstrap.Bootstrap
  import io.netty.channel.ChannelFuture
  import io.netty.channel.ChannelInitializer
  import io.netty.channel.nio.NioEventLoopGroup
  import io.netty.channel.udt.UdtChannel
  import io.netty.channel.udt.nio.NioUdtProvider
  import io.netty.handler.logging.LogLevel
  import io.netty.handler.logging.LoggingHandler
  import io.netty.util.concurrent.DefaultThreadFactory
  import scalaz.syntax.either._
  import scodec.Err
  import Data._
  import DnsCodec._ 
  import Util._
  import java.net.InetSocketAddress
  import io.netty.channel.socket.nio.NioDatagramChannel

  
  def sendPacket: ((Err \/ (InetSocketAddress, DnsPacket)) => Task[Unit]) => Task[Err \/ (InetSocketAddress, DnsPacket) => Task[Unit]] = {
    val f = OutgoingSafeCps(makeNettyClient _) 
    val iso = CodecLenses.codecToBytes[DnsPacket].reverse 
    f.leftChoice[Err].ixmap(iso).f  
    //val safef = toSafeCps[DatagramPacket, (InetSocketAddress, DnsPacket)](f)(iso)
    //safef 
  }
  
  def makeNettyClient(incoming: DatagramPacket => Task[Unit]): Task[DatagramPacket => Task[Unit]] = {
    for {
      bootstrap <-Task.delay(new Bootstrap())
      group     = new NioEventLoopGroup()
      handler   = simpleHandler(incoming)
      _         = bootstrap.group(group).channel(classOf[NioDatagramChannel]).handler(handler)
      ch       <- Task.delay(bootstrap.bind(0).sync.channel())
     } yield {
        (d: DatagramPacket) => 
          Task.delay { 
          ch.writeAndFlush(d).sync() 
          println("done sending datagram!")
        }
      }
  } 
   
  def simpleHandler(incoming: DatagramPacket => Task[Unit]) = new SimpleChannelInboundHandler[DatagramPacket] {
    override def channelRead0(ctx: ChannelHandlerContext, packet: DatagramPacket): Unit ={
        println("\n \n \n ~~~~~~> chanenl READ \n ") 
        println(incoming(packet).attemptRunFor(10.seconds)) //TODO: can I flush here?
      }
  }
}

import scalaz.syntax.either._
import scodec.Err

object Util {

  case class OutgoingSafeCps[A](f: ((A => Task[Unit]) => Task[A => Task[Unit]])) {
    def xmap[B](fab: A => B, gba: B => A): OutgoingSafeCps[B] = {
      val bf = (btot: B => Task[Unit]) => { 
        val ret = f((a: A) => btot(fab(a)))
        ret.map(atot => (b: B) => atot(gba(b))) 
      }
      OutgoingSafeCps(bf) 
    }

    def ixmap[B](iso: Iso[A, B]): OutgoingSafeCps[B] =
      xmap(iso.get _, iso.rget _)

    def leftChoice[C]: OutgoingSafeCps[C \/ A] = { //sad, left choice should really allow for us to abstract over the type of the \/
      val caf = (catot: C \/ A => Task[Unit]) => {
        val ret = f((a: A) => catot(\/-(a)))
        ret.map(att => (ca: C \/ A) => ca match {
          case -\/(c) => Task.fail(new Throwable(c.toString)) 
          case \/-(a) => att(a)
        })
      }
      OutgoingSafeCps(caf)
    }

    //def dimap[A, B, C, D](ca: C => A, bd: B => D): OutgoingSafeCps
}
  //def toSafeCps[A, B](f: (A => Task[Unit]) => Task[A => Task[Unit]])(implicit I: Iso[Err \/ A, Err \/ B]): (Err \/ B => Task[Unit]) => Task[Err \/ B => Task[Unit]] = {
  //  (ebtot) => {
  //    val ret = f((a: A) => ebtot(I.get(a.right[Err])))
  //    
  //    val sret = ret.map(af => (eob: Err \/ B) => eob match{ 
  //      case -\/(err) => Task.fail(new Throwable("blah"))
  //      case \/-(b) => 
  //        val y = I.rget(b.right[Err]) match {
  //          case -\/(err) => Task.fail(new Throwable("Blah"))
  //          case \/-(a) => af(a)
  //        }
  //        y
  //    })
  //    sret
  //  }
  //}
}
