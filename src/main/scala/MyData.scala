package dnsclient
import Data._

object DnsCodec {
  import scalaz.NonEmptyList 
  import scalaz.syntax.foldable1._
  import scodec.{bits => _, _}
  import scodec.codecs._
  import scodec.bits._

 
  def resourceRecordCodec: Codec[ResourceRecord] = (ignore(64) :: uint32 :: ipv4).as[ResourceRecord]

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
    }.map(_ ++ BitVector.lowByte ++ hex"00 01".toBitVector ++ hex"00 01".toBitVector)
      
    import Attempt._
    override def decode(bits0: BitVector): Attempt[DecodeResult[DnsString]] = {
      def go(acc: List[String], bits: BitVector): Attempt[DecodeResult[NonEmptyList[String]]] = {
        (uint8.decode(bits), acc) match {
          case (Successful(DecodeResult(v, rem)), h :: t) if (v == 0) =>
            Successful(DecodeResult(NonEmptyList(h, t: _*), rem.splitAt(32)._2))
          case (Successful(DecodeResult(v, rem)), _) if (v != 0) =>
            fixedSizeBytes(v.toLong, utf8).decode(rem) match {
              case Attempt.Successful(DecodeResult(vs, rem2)) => 
                go(vs :: acc, rem2) 
              case f: Attempt.Failure => f
            }
          case (f: Failure, _) => f
          case _ => Failure(Err("no dnsstring data to process"))
        }
      }
        go(Nil, bits0).map(_.map(nel => DnsString(nel)))
      }
    }
    
    def roundTrip[T](v: T, codec: Codec[T]) = codec.encode(v).flatMap(codec.decode).map(_.value)  
  
    roundTrip(DnsString(NonEmptyList("www", List("netbsd", "org"): _*)), dnsString)
  def msgTypeCodec: Codec[MessageType] = mappedEnum(
    bool, 
    DnsRequest -> false,
    DnsResponse -> true)

  def dnsPacketCodec = (
    ("Transaction ID"         | uint16)               ::
    ("Response"               | msgTypeCodec)         :: 
    ("Opcode"                 | ignore(4))            ::
    ("Authorative"            | ignore(1))            ::
    ("Truncated"              | ignore(1))            ::
    ("Recursion desired"      | ignore(1))            ::
    ("Recursion available"    | ignore(1))            ::
    ("Reserved"               | ignore(1))            ::
    ("Answer authenticated"   | ignore(1))            :: 
    ("Non-authenticated data" | ignore(1))            ::
    ("Reply code"             | ignore(4))            ::
    ("Questions"              | constant(hex"00 01")) ::
    ("Answer RRs"             | ignore(16))           ::
    ("Authority RRs"          | ignore(16))           ::
    ("Additional RRs"         | ignore(16))           ::
    ("Query Name"             | dnsString)            :: //queries?
    ("Address ResourceRecords"                | vector(resourceRecordCodec))
  ).dropUnits.as[DnsPacket]
}

/*
import scalaz.NonEmptyList
import dnsclient._
import Test._

import scodec.bits._
import dnsclient.DnsCodec._ 

client(NonEmptyList("reddit", List("com"): _*)).run.run 
*/
