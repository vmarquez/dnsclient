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

  type DGHandler = SimpleChannelInboundHandler[DatagramPacket]
  import DnsCodec._ 
  //def sendPacket(host: String, port: Int): Task[DnsPacket => Task[Unit]] = {
  //  val f = CpsFunction(makeNettyClient(host, port) _)
  //  import CodecLenses._
  //  
  //  val codec = implicitly[Iso[Err \/ DnsPacket, Err \/ DatagramPacket]]      
  //  val finalfunc = f.toSafely[Err \/ DnsPacket] 
  //}

  
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
case class CpsFunction[A](f: (A => Task[Unit]) => Task[A => Task[Unit]]) {
    
    def to[B](implicit I: Iso[A, B]): CpsFunction[B] =
      CpsFunction((bf: (B => Task[Unit])) => {
        val y = f((a: A) => bf(I.get(a))) 
        y.map(bt => (b: B) => bt(I.rget(b)))
    }) 

    def toSafely[B](implicit I: Iso[A, B]): CpsFunction[Err \/ B] = 
      CpsFunction(bertf => {
        f(a => bertf(I.get(a).right[Err])).map(at => (b: Err \/ B) => b match {
          case -\/(e) => Task.fail(new Throwable(e.message))
          case \/-(b) => at(I.rget(b))
        })
      })
  }

