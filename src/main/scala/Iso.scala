package dnsclient
import scalaz.{Profunctor, \/, -\/, \/-}

//make first and second functions?
trait Iso[S, A] { self =>
  import LensHelpers._
  
  def sa[P[_, _]: Profunctor]: P[A, A] => P[S, S]

  def get(s: S): A = 
    sa[Forget[A, ?, ?]](Profunctor[Forget[A, ?, ?]])(Forget[A, A, A](a => a)).forget(s)

  def rget(a: A): S =
    sa[RConst](Profunctor[RConst])(RConst[A, A](a)).b

  def compose[B](iso: Iso[A, B]): Iso[S, B] = new Iso[S, B] {
    override def sa[P[_,_]](implicit P: Profunctor[P]): P[B, B] => P[S, S] =
      iso.sa(P) andThen self.sa(P)
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
  
  def toScalazIso: scalaz.Isomorphism.Iso[Function1, S, A] = new scalaz.Isomorphism.Iso[Function1, S, A] {
    def to = get _
    def from = rget _
  }
}

object Iso {
  def apply[S, A](saf: S => A, as: A => S): Iso[S, A] = new Iso[S, A] {
    override def sa[P[_, _]: Profunctor]: P[A, A] => P[S, S] = 
      Profunctor[P].dimap(_)(saf)(as)   
  }
}

