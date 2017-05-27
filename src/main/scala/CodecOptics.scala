package dnsclient 

object CodecLenses {
  import scodec.bits._ 
  import scodec._
  import io.netty.buffer.Unpooled
  import io.netty.channel.socket.DatagramPacket
  import java.net.InetSocketAddress
  import scodec.interop.scalaz._
  import scalaz._
  import scalaz.syntax.profunctor._
  import scalaz.syntax.std.tuple._
  import scalaz.syntax.functor._
  import scalaz.concurrent.Task

  val datagramIso: Iso[(InetSocketAddress, Array[Byte]), DatagramPacket] = Iso(t => new DatagramPacket(Unpooled.copiedBuffer(t._2), t._1), dgp => { 
    val bytebuf = dgp.content
    val bytearr = new Array[Byte](bytebuf.readableBytes())
    bytebuf.getBytes(bytebuf.readerIndex(), bytearr)
    (dgp.recipient, bytearr)
  })
  
  def codecIso[A](implicit codec: Codec[A]): Iso[Err \/ A, Err \/ BitVector] = Iso(aa => aa.flatMap(codec.encode(_).toDisjunction), bv => bv.flatMap(b => codec.decode(b).map(_.value).toDisjunction))
  
  def bvToBa: Iso[BitVector, Array[Byte]] = Iso(b => b.toByteArray, ba => ByteVector(ba).toBitVector)
  import scalaz._
  import Scalaz._
  //hmm not so efficient
  def addressAbv[A](implicit c: Codec[A]): Iso[Err \/ (InetSocketAddress, A), Err \/ (InetSocketAddress, BitVector)] = Iso.apply[Err \/ (InetSocketAddress, A), Err \/ (InetSocketAddress, BitVector)](
      err => err.flatMap(t => codecIso[A].get(t._2.right[Err]).map(a => (t._1, a))), 
      err => err.flatMap(t => codecIso[A].rget(t._2.right[Err]).map(a => (t._1, a)))
    )

  def codecToBytes[A](implicit codec: Codec[A]): Iso[Err \/ (InetSocketAddress, A),  Err \/ DatagramPacket] = {
    val niso = addressAbv[A]
    niso compose (bvToBa.first[InetSocketAddress].choiceRight[Err] compose datagramIso.choiceRight[Err])
  }
}


