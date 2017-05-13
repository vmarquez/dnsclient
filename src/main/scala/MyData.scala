package dnsclient

import scodec.{bits => _, _}
import scodec.codecs._
import scodec.bits._



object DnsLenses {
  
}

object DnsCodec {
  import scalaz.NonEmptyList 
  import scalaz.syntax.foldable1._
  
  trait DnsPacket
  case class DnsString(nel: NonEmptyList[String])  
  case class Request(transactionID: Int, name: DnsString) extends DnsPacket
  case class IPV4(a: Int, b: Int, c: Int, d: Int)
  case class Response(transactionID: Int, name: DnsString, address: IPV4) extends DnsPacket

  def ipv4: Codec[IPV4] = (uint8 :: uint8 :: uint8 :: uint8).as[IPV4]

  def dnsString = new Codec[DnsString] {

    def sizeBound = SizeBound.unknown

    def encodeStr(s: String): Attempt[BitVector] = for {
      a <- uint8.encode(s.length)
      b <- fixedSizeBytes(s.length.toLong, utf8).encode(s)
    } yield a ++ b

    override def encode(dnss: DnsString) = dnss.nel.foldMapLeft1(s => encodeStr(s)) { (acc, s) => 
      for {
        cur <- acc
        b <- encodeStr(s)
      } yield cur ++ b 
    }.map(_ ++ BitVector.lowByte)
      
    import Attempt._
    override def decode(bits0: BitVector): Attempt[DecodeResult[DnsString]] = {
      def go(acc: List[String], bits: BitVector): Attempt[DecodeResult[NonEmptyList[String]]] =
        (uint8.decode(bits), acc) match {
          case (Successful(DecodeResult(v, rem)), h :: t) if (v == 0) =>
            Successful(DecodeResult(NonEmptyList(h, t: _*), rem))
          case (Successful(DecodeResult(v, rem)), _) if (v != 0) =>
            fixedSizeBytes(v.toLong, utf8).decode(rem) match {
              case Attempt.Successful(DecodeResult(vs, rem2)) => go(vs :: acc, rem2)
              case f: Attempt.Failure => f
            }
          case (f: Failure, _) => f
          case _ => Failure(Err("no dnsstring data to process"))
        }
        go(Nil, bits0).map(_.map(nel => DnsString(nel)))
      }
    }
    
    def roundTrip[T](v: T, codec: Codec[T]) = codec.encode(v).flatMap(codec.decode).map(_.value)  
  
    roundTrip(DnsString(NonEmptyList("www", List("netbsd", "org"): _*)), dnsString)

    val dnsRequestCodec = (
    ("Transaction ID"         | uint16)               ::
    ("Response"               | constant(bin"0"))     :: 
    ("Opcode"                 | ignore(4))            ::
    ("Reserved"               | ignore(1))            ::
    ("Truncated"              | ignore(1))            ::
    ("Recursion"              | constant(bin"1"))     ::
    ("Reserved"               | ignore(3))            ::
    ("Non-authenticated data" | ignore(1))            ::
    ("Reserved"               | ignore(4))            ::
    ("Questions"              | constant(hex"00 01")) ::
    ("Answer RRs"             | ignore(16))           ::
    ("Authority RRs"          | ignore(16))           ::
    ("Additional RRs"         | ignore(16))           ::
    ("Name"                   | dnsString)            ::
    ("Type"                   | constant(hex"00 01")) ::
    ("Class"                  | constant(hex"00 01"))
  ).dropUnits.as[Request]

  def flags = (
    ("Response"               | constant(bin"1"))     :: 
    ("Opcode"                 | ignore(4))            ::
    ("Authorative"            | ignore(1))            ::
    ("Truncated"              | ignore(1))            ::
    ("Recursion desired"      | ignore(1))            ::
    ("Recursion available"    | ignore(1))            ::
    ("Reserved"               | ignore(1))            ::
    ("Answer authenticated"   | ignore(1))            :: 
    ("Non-authenticated data" | ignore(1))            ::
    ("Reply code"             | ignore(4))
  ).dropUnits
  
  def header = (
    ("Transaction ID"         | uint16)               ::
    flags :::
    ("Questions"              | constant(hex"00 01")) ::
    ("Answer RRs"             | constant(hex"00 01")) ::
    ("Authority RRs"          | ignore(16))           ::
    ("Additional RRs"         | ignore(16))           ::
    ("Name"                   | dnsString)            ::
    ("Type"                   | constant(hex"00 01")) ::
    ("Class"                  | constant(hex"00 01")) ::
    ("Name"                   | constant(hex"c0 0c"))
  ).dropUnits
    
  def dnsResponseCodec = (
    header :::
    ("Type"                   | constant(hex"00 01")) ::
    ("Class"                  | constant(hex"00 01")) ::
    ("TTL"                    | ignore(32))           ::
    ("Data Length"            | constant(hex"00 04")) ::
    ("Address"                | ipv4)
  ).dropUnits.as[Response]

}
