package dnsclient 


//taken from experimental scalaz branches

import scalaz._
import Scalaz._


object LensHelpers {

  import scalaz.{ProChoice, Profunctor, \/, -\/, \/-}

  case class RConst[A, B](b: B)

  final case class Forget[A, B, C](forget: B => A){ 
    def retag[D]: Forget[A, B, D] = this.asInstanceOf[Forget[A, B, D]] 
  }

  implicit def rconstProChoice: ProChoice[RConst] = new ProChoice[RConst] {
    override def dimap[A, B, C, D](pab: RConst[A, B])(f: C => A)(g: B => D): RConst[C, D] = RConst(g(pab.b))
    override def left[A, B, C](pab: RConst[A, B]): RConst[A \/ C, B \/ C] = RConst(-\/(pab.b)) 
    override def right[A, B, C](pab: RConst[A, B]): RConst[C \/ A, C \/ B] = RConst(\/-(pab.b)) 
    override def mapfst[A, B, C](fab: RConst[A,B])(f: C => A): RConst[C,B] = ???
    override def mapsnd[A, B, C](fab: RConst[A,B])(f: B => C): RConst[A,C] = ???
  }
  
  implicit def forgetProfunctor[Z]: Profunctor[({ type l[a, b] = Forget[Z, a, b]})#l] = new Profunctor[({ type l[a, b] = Forget[Z, a, b]})#l] {
    override def dimap[A, B, C, D](pab: Forget[Z, A, B])(ac: C => A)(bd: B => D): Forget[Z, C, D] = Forget((c: C) => (pab.forget(ac(c))))

    def mapfst[A, B, C](fab: Forget[Z,A,B])(f: C => A): Forget[Z,C,B] = ??? 
    
    def mapsnd[A, B, C](fab: Forget[Z,A,B])(f: B => C): Forget[Z,A,C] = ??? 

  }

  implicit def forgetProChoice[Z]: ProChoice[({ type l[a, b] = Forget[Z, a, b]})#l] = new ProChoice[({ type l[a, b] = Forget[Z, a, b]})#l] {
    override def dimap[A, B, C, D](pab: Forget[Z, A, B])(ac: C => A)(bd: B => D): Forget[Z, C, D] = Forget((c: C) => (pab.forget(ac(c))))

    override def left[A, B, C](pab: Forget[Z, A, B]): Forget[Z, (A \/ C), (B \/ C)] = ???

    override def right[A, B, C](pab: Forget[Z, A, B]): Forget[Z, (C \/ A), (C \/ B)] = ??? 
      //Forget[A, (C \/ A), (C \/ B) = 
  
    def mapfst[A, B, C](fab: Forget[Z,A,B])(f: C => A): Forget[Z,C,B] = ??? 
    
    def mapsnd[A, B, C](fab: Forget[Z,A,B])(f: B => C): Forget[Z,A,C] = ??? 

  }
}

trait Iso[S, A] { self =>
  import LensHelpers._
  def stab[P[_, _]: Profunctor]: P[A, A] => P[S, S]

  def get(s: S): A = 
    stab[Forget[A, ?, ?]](Profunctor[Forget[A, ?, ?]])(Forget[A, A, A](a => a)).forget(s)

  def rget(a: A): S =
    stab[RConst](Profunctor[RConst])(RConst[A, A](a)).b

  def compose[B](iso: Iso[A, B]): Iso[S, B] = new Iso[S, B] {
    override def stab[P[_,_]](implicit P: Profunctor[P]): P[B, B] => P[S, S] =
      iso.stab(P) andThen self.stab(P)
  }
}

object Iso {
  def apply[S, A](sa: S => A, as: A => S): Iso[S, A] = new Iso[S, A] {
    override def stab[P[_, _]: Profunctor]: P[A, A] => P[S, S] = 
      Profunctor[P].dimap(_)(sa)(as)   
  }
}

object ScodecTest {
  import scodec.bits._ 
  import scodec._
  import io.netty.buffer.Unpooled
  import io.netty.channel.socket.DatagramPacket
  import java.net.InetSocketAddress
  import scodec.interop.scalaz._

  //implicit def codecIso[A](implicit codec: Codec[A]): Iso[Attempt[BitVector], Attempt[A]] = Iso(bv => bv.flatMap(b => codec.decode(b).map(_.value)), (aa) => aa.flatMap(codec.encode))

  implicit def codecIso[A](implicit codec: Codec[A]): Iso[Err \/ BitVector, Err \/ A] = Iso(bv => bv.flatMap(b => codec.decode(b).map(_.value).toDisjunction), (aa) => aa.flatMap(codec.encode(_).toDisjunction))
 
  implicit def datagramIso: Iso[DatagramPacket, (InetSocketAddress, Array[Byte])] = Iso(dgp => (dgp.recipient, dgp.content.array), (t) => new DatagramPacket(Unpooled.copiedBuffer(t._2), t._1))

  implicit def bvToBa: Iso[Err \/ BitVector, Err \/ Array[Byte]] = Iso(bv => bv.map(b => b.toByteArray), ba => ba.map(b => ByteVector(b).toBitVector))
  

}
