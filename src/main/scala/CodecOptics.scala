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

  def datagramIso: Iso[(InetSocketAddress, Array[Byte]), DatagramPacket] = Iso(t => new DatagramPacket(Unpooled.copiedBuffer(t._2), t._1), dgp => (dgp.recipient, dgp.content.array))
  
  def codecIso[A](implicit codec: Codec[A]): Iso[Err \/ A, Err \/ BitVector] = Iso(aa => aa.flatMap(codec.encode(_).toDisjunction), bv => bv.flatMap(b => codec.decode(b).map(_.value).toDisjunction))
  
  def bvToBa: Iso[BitVector, Array[Byte]] = Iso(b => b.toByteArray, ba => ByteVector(ba).toBitVector)

  def codecToBytes[A](implicit codec: Codec[A]): Iso[Err \/ (InetSocketAddress, A),  Err \/ DatagramPacket] = {
    import scalaz._
    import Scalaz._
    val ciso = codecIso[A]
    val niso = Iso.apply[Err \/ (InetSocketAddress, A), Err \/ (InetSocketAddress, BitVector)](
      err => err.fold(e => e.left[(InetSocketAddress, BitVector)], t => ciso.get(t._2.right[Err]).map(a => (t._1, a))), 
      err => err.fold(e => e.left[(InetSocketAddress, A)], t => ciso.rget(t._2.right[Err]).map(a => (t._1, a)))
    ) 
    niso compose (bvToBa.first[InetSocketAddress].choiceRight[Err] compose datagramIso.choiceRight[Err])
  }

}


