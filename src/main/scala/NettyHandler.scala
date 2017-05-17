package dnsclient

import io.netty.buffer.Unpooled
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.socket.DatagramPacket
import scalaz.{\/, -\/, \/-}
import scalaz.concurrent.Task
import scala.concurrent.duration._
import java.net.InetSocketAddress
//import Util._

/*
    // Configure the client.
50          final ThreadFactory connectFactory = new DefaultThreadFactory("connect");
51          final NioEventLoopGroup connectGroup = new NioEventLoopGroup(1,
52                  connectFactory, NioUdtProvider.MESSAGE_PROVIDER);
53          try {
54              final Bootstrap boot = new Bootstrap();
55              boot.group(connectGroup)
56                      .channelFactory(NioUdtProvider.MESSAGE_CONNECTOR)
57                      .handler(new ChannelInitializer<UdtChannel>() {
58                          @Override
59                          public void initChannel(final UdtChannel ch)
60                                  throws Exception {
61                              ch.pipeline().addLast(
62                                      new LoggingHandler(LogLevel.INFO),
63                                      new MsgEchoClientHandler());
64                          }
65                      });
66              // Start the client.
67              final ChannelFuture f = boot.connect(HOST, PORT).sync();
68              // Wait until the connection is closed.
69              f.channel().closeFuture().sync();
70          } finally {
71              // Shut down the event loop to terminate all threads.
72              connectGroup.shutdownGracefully();
73          }
*/

object CreateNetty {
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
  import scalaz.Profunctor
  import scodec.Err
  import Data._
  type DGHandler = SimpleChannelInboundHandler[DatagramPacket]
  import DnsCodec._ 
  import Util._
  import java.net.InetSocketAddress

  def sendPacket(host: String, port: Int): ((Err \/ (InetSocketAddress, DnsPacket)) => Task[Unit]) => Task[Err \/ (InetSocketAddress, DnsPacket) => Task[Unit]] = {
    val f = makeNettyClient(host, port) _
    //val iso = CodecLenses.codecAtoDG[DnsPacket] 
    val iso = CodecLenses.codecToBytes[DnsPacket].reverse 
    val safef = toSafeCps[DatagramPacket, (InetSocketAddress, DnsPacket)](f)(iso)
    //import CodecLenses._
    
    safef 
  }

  //todo: instead of \/ to Task[Unit], make an iso for DT => Task ? 
  def makeNettyClient(host: String, port: Int)(incoming: DatagramPacket => Task[Unit]): Task[DatagramPacket => Task[Unit]] = {
    val connectFactory = new DefaultThreadFactory("connect")
    val connectGroup = new NioEventLoopGroup(1, connectFactory, NioUdtProvider.MESSAGE_PROVIDER)
    Task.delay {
      val bs = new Bootstrap()
      val (handler, ctxtask) = createHandler(incoming)
      bs.group(connectGroup).channelFactory(NioUdtProvider.MESSAGE_CONNECTOR).handler(new ChannelInitializer[UdtChannel]() {
        override def initChannel(ch: UdtChannel): Unit =
          ch.pipeline().addLast(new LoggingHandler(LogLevel.INFO), handler)
      })
      //start the stuff
      val f = bs.connect(host, port).sync()
      f.channel().closeFuture().sync()
      println("started")
      ctxtask.map(ctx => (dg: DatagramPacket) => Task.delay {
        ctx.write(dg)
        ctx.flush()
        ()
      })
    }.flatMap(a => a)//.onFinish(t => Task.delay(connectGroup.shutdownGracefully()).map(_ => ()))
  }
  
  def createHandler(incoming: DatagramPacket => Task[Unit]): (DGHandler, Task[ChannelHandlerContext]) = {
    var t: Task[ChannelHandlerContext] = Task.fail(new Throwable("not initialized"))
    val handler = new SimpleChannelInboundHandler[DatagramPacket] {
      override def channelActive(ctx: ChannelHandlerContext): Unit = { 
        t = Task.async(f => f(ctx.right[Throwable])) 
      }

      override def channelRead0(ctx: ChannelHandlerContext, packet: DatagramPacket): Unit =
        incoming(packet).map(_ => ctx.flush()).attemptRunFor(1.seconds) //TODO: can I flush here?
    }
    (handler, t)
  }
}

import scalaz.syntax.either._
import scodec.Err

object Util {
  def toSafeCps[A, B](f: (A => Task[Unit]) => Task[A => Task[Unit]])(implicit I: Iso[Err \/ A, Err \/ B]): (Err \/ B => Task[Unit]) => Task[Err \/ B => Task[Unit]] = {
    (ebtot) => {
      val ret = f((a: A) => ebtot(I.get(a.right[Err])))
      
      val sret = ret.map(af => (eob: Err \/ B) => eob match{ 
        case -\/(err) => Task.fail(new Throwable("blah"))
        case \/-(b) => 
          val y = I.rget(b.right[Err]) match {
            case -\/(err) => Task.fail(new Throwable("Blah"))
            case \/-(a) => af(a)
          }
          y
      })
      sret
    }
  }
}
