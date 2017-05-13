package dnsclient

//taken from experimental scalaz branches
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

