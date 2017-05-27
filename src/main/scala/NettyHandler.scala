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
  //import scalaz.syntax.either._
  import scodec.Err
  import Data._
  import DnsCodec._ 
  import Util._
  import java.net.InetSocketAddress
  import io.netty.channel.socket.nio.NioDatagramChannel
  import scalaz._
  
  def sendPacket: ((Err \/ (InetSocketAddress, DnsPacket)) => Task[Unit]) => Task[Err \/ (InetSocketAddress, DnsPacket) => Task[Unit]] = {
    import scalaz.syntax.profunctor._
    import scalaz.syntax.invariantFunctor._
    import scalaz.syntax.proChoice
    val f = OutgoingCps(makeNettyClient _)
    val iso = CodecLenses.codecToBytes[DnsPacket].reverse
    val xf = f.choiceRight[Err]
    val invf = Profunctor[OutgoingCps].invariantFunctor.xmapi(xf)(iso.toScalazIso)
     ???
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
import scalaz._

object Util {
  implicit def outgoingCpsProfunctor: ProChoice[OutgoingCps] = new ProChoice[OutgoingCps] {
    override def dimap[A, B, C, D](pab: OutgoingCps[A, B])(ca: C => A)(bd: B => D): OutgoingCps[C, D] = 
      pab.dimap(ca, bd)
  
    override def mapfst[A, B, C](pab: OutgoingCps[A, B])(ca: C => A): OutgoingCps[C, B] = dimap(pab)(ca)(b => b) 
  
    override def mapsnd[A, B, D](pab: OutgoingCps[A, B])(bd: B => D): OutgoingCps[A, D] = dimap(pab)((a: A) => a)( bd)
  
    def left[A, B, C](pab: OutgoingCps[A, B]): OutgoingCps[A \/ C, B \/ C] = ???

    def right[A, B, C](fa: OutgoingCps[A,B]): OutgoingCps[C \/ A, C \/ B] = fa.choiceRight[C]
  }
  
  case class OutgoingCps[A, B](f: ((B => Task[Unit]) => Task[A => Task[Unit]])) {

    def dimap[C, D](ca: C => A, bd: B => D): OutgoingCps[C, D] = {
      val ret = (dtt: D => Task[Unit]) => 
        f((b: B) => dtt(bd(b))).map(att => 
          (c: C) => att(ca(c)))
      OutgoingCps(ret) 
    }
    
    //def xmap[B](fab: A => B, gba: B => A): OutgoingSafeCps[B] = {
    //  val bf = (btot: B => Task[Unit]) => { 
    //    val ret = f((a: A) => btot(fab(a)))
    //    ret.map(atot => (b: B) => atot(gba(b))) 
    //  }
    //  OutgoingSafeCps(bf) 
    //}

    def choiceRight[C]: OutgoingCps[C \/ A, C \/ B] = { //sad, left choice should really allow for us to abstract over the type of the \/
      val caf = (catot: C \/ B => Task[Unit]) => {
        val ret = f((b: B) => catot(\/-(b)))
        ret.map(att => (ca: C \/ A) => ca match {
          case -\/(c) => Task.fail(new Throwable(c.toString)) 
          case \/-(a) => att(a)
        })
      }
      OutgoingCps(caf)
    }
}

  /*case class OutgoingSafeCps[A](f: ((A => Task[Unit]) => Task[A => Task[Unit]])) {
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
}*/

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
