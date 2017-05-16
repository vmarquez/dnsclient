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

  //TODO can we do functor based Isos?  
  //(Addr, A) => InetAddr, A 

  //we can make codecs go back and fourth!!! 

  def toE[A](f: A => Task[Unit]): Throwable \/ A => Task[Unit] = (e) => e match { case -\/(t) => Task.fail(t); case \/-(a) => f(a) } 
  
  implicit def codecIso[A](implicit codec: Codec[A]): Iso[Err \/ A, Err \/ BitVector] = Iso(aa => aa.flatMap(codec.encode(_).toDisjunction), bv => bv.flatMap(b => codec.decode(b).map(_.value).toDisjunction))

  implicit def bvToBa: Iso[Err \/ BitVector, Err \/ Array[Byte]] = Iso(bv => bv.map(b => b.toByteArray), ba => ba.map(b => ByteVector(b).toBitVector))

  implicit def datagramIso: Iso[(InetSocketAddress, Array[Byte]), DatagramPacket] = Iso((t) => new DatagramPacket(Unpooled.copiedBuffer(t._2), t._1), dgp => (dgp.recipient, dgp.content.array))
  
  def inetAddrAToB[A]: Iso[(InetSocketAddress, A), (InetSocketAddress, Array[Byte])] =  
   
  def codecToBa[A](implicit codec: Codec[A]): Iso[Err \/ A, Err \/ Array[Byte]] = codecIso compose bvToBa

  implicit def codecDGtoA[A](implicit codec: Codec[A]): Iso[Err \/ DatagramPacket, Err \/ (InetSocketAddress, A)] =    datagramIso
  
  //implicit def reverseIso[A, B](implicit I: Iso[A, B]): Iso[B, A] = I.reverse

  //implicit def isoTuple[A, B, C](implicit i: Iso[A, B]): Iso[(C, A), (C, B)] = Iso(ca => ca.map(a => i.get(a)), cb => cb.map(i.rget))
  implicit def isoTuple[A, B, C](implicit i: Iso[A, B]): Iso[(C, A), (C, B)] = Iso(ca => (ca._1, i.get(ca._2)), cb => (cb._1, i.rget(cb._2)))

}


