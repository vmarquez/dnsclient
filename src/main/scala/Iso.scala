package dnsclient
import scalaz.{Profunctor, \/, -\/, \/-}
/*
trait FIso[F[_], S, A] { self =>
  import LensHelpers._
  def stab[P[_, _]: Profunctor]: P[A, A] => P[S, S]

  def get(s: F[S])(implicit F: Functor[F]): F[A] = 
    s.map(ss => stab[Forget[A, ?, ?]](Profunctor[Forget[A, ?, ?]])(Forget[A, A, A](a => a)).forget(ss))

  def rget(a:F[A])(implicit F: Functor[F]): F[S] =
    a.map(aa => stab[RConst](Profunctor[RConst])(RConst[A, A](aa)).b)

  def compose[B](iso: Iso[A, B]): FIso[S, B] = new FIso[F, S, B] {
    override def stab[P[_,_]](implicit P: Profunctor[P]): P[B, B] => P[S, S] =
      iso.stab(P) andThen self.stab(P)
  }

  def reverse: Iso[A, S] = FIso(rget, get)

}

object FIso {
  def apply[F[_], S, A](sa: S => A, as: A => S): FIso[F, S, A] = new FIso[F, S, A] {
    override def stab[P[_, _]: Profunctor]: P[A, A] => P[S, S] = 
      Profunctor[P].dimap(_)(sa)(as)   
  }
}*/
//make first and second functions?
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
  import scalaz.syntax.std.tuple._
  import scalaz.syntax.functor._
  import scalaz.syntax.either._
  def first[C]: Iso[(C, S), (C, A)] = Iso[(C, S), (C, A)]({ case (c, s) => (c, self.get(s)) }, { case (c, a) => (c, self.rget(a)) })  

  def choiceRight[C]: Iso[C \/ S, C \/ A] = Iso[C \/ S, C \/ A](
    { case -\/(err) => err.left[A]; case \/-(s) => self.get(s).right[C] }, 
    { case -\/(err) => err.left[S]; case \/-(a) => self.rget(a).right[C] }
  )

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

