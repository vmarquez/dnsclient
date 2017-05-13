package dnsclient
import scalaz.Profunctor

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

  def reverse: Iso[A, S] = Iso(rget, get)

}

object Iso {
  def apply[S, A](sa: S => A, as: A => S): Iso[S, A] = new Iso[S, A] {
    override def stab[P[_, _]: Profunctor]: P[A, A] => P[S, S] = 
      Profunctor[P].dimap(_)(sa)(as)   
  }
}

//trait TIso[S, A, C] { self =>
//  import LensHelpers._
//  def stab[P[_, _]: Strong]: P[A, A] => P[S, S]
//
//  def get(s: (S, C): (A, C) = ???
//    //stab[Forget[A, ?, ?]](Profunctor[Forget[A, ?, ?]])(Forget[A, A, A](a => a)).forget(s)
//
//  def rget(a: (A, C)): (S, C) =
//    //stab[RConst](Profunctor[RConst])(RConst[A, A](a)).b
//
//  /*def compose[B](iso: Iso[A, B]): Iso[S, B] = new Iso[S, B] {
//    override def stab[P[_,_]](implicit P: Profunctor[P]): P[B, B] => P[S, S] =
//      iso.stab(P) andThen self.stab(P)
//  }*/
//
//  /*def tupled[B]: Iso[(B, S), (B, A)] = new Iso[(B, S), (B, A)]{
//    def stab[
//  }*/
//}
//
//object TIso {
//  def apply[S, A, C](sa: S => A, as: A => S): Iso[S, A] = new Iso[S, A] {
//    override def stab[P[_, _]: Profunctor]: P[A, A] => P[S, S] = 
//      Profunctor[P].dimap(_)(sa)(as)   
//  }
//}
