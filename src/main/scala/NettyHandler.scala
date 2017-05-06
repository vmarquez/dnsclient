package dnsclient

import io.netty.buffer.Unpooled
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.socket.DatagramPacket
import scalaz.\/
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
  
  def makeNettyClient[A](host: String, port: Int)(toa: DatagramPacket => \/[Throwable, A])(se: A  => Task[Unit]): Task[DatagramPacket => Task[Unit]] = {
    val connectFactory = new DefaultThreadFactory("connect")
    val connectGroup = new NioEventLoopGroup(1, connectFactory, NioUdtProvider.MESSAGE_PROVIDER)
    Task.delay {
      val bs = new Bootstrap()
      bs.group(connectGroup).channelFactory(NioUdtProvider.MESSAGE_CONNECTOR).handler(new ChannelInitializer[UdtChannel]() {
        override def initChannel(ch: UdtChannel): Unit =
          ch.pipeline().addLast(new LoggingHandler(LogLevel.INFO), makeHandler(se))
      })
      //start the stuff
      val f = bs.connect(host, port).sync()
      f.channel().closeFuture().sync()
      println("started")
    }.onFinish(t => Task.delay(connectGroup.shutdownGracefully()).map(_ => ()))
  }


  def makeHandler[A](f: (DatagramPacket => A) => Task[Unit]): SimpleChannelInboundHandler[DatagramPacket] = ??? /*new SimpleChannelInboundHandler[DatagramPacket] {
    
  }*/
}

class NettyHandler(f: DatagramPacket => \/[Throwable, (InetSocketAddress, Array[Byte])]) extends SimpleChannelInboundHandler[DatagramPacket] {
  var context: Option[ChannelHandlerContext] = None 
  
  override def channelActive(ctx: ChannelHandlerContext): Unit = context = Option(ctx)

  def channelRead0(ctx: ChannelHandlerContext, packet: DatagramPacket): Unit = {
    /*f(packet).flatMap(bytes => Task.delay {
      val npacket = new DatagramPacket(Unpooled.copiedBuffer(bytes), packet.sender())
      logger.info("writing packets to " + packet.sender.toString)
      ctx.write(npacket)
      ctx.flush()
    }).timed(2.minutes).runAsync(r => logger.debug("our response task has finished = " + r))*/
  }

  //def sendPacket(d: DatagramPacket): Task[Unit] = 

  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
    ctx.flush()
    ()
  }
}
