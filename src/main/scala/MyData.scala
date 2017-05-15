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
  
  case class Request(transactionID: Int, questions: Vector[DnsString]) extends DnsPacket

  case class IPV4(a: Int, b: Int, c: Int, d: Int)
 
  case class Response(transactionId: Int, question: DnsString, addresses: Vector[ResourceRecord]) extends DnsPacket

  case class ResourceRecord(ttl: Long, ip: IPV4)

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
    ("questions"              | vector(dnsString)) 
  ).dropUnits.as[Request]

  //case class Question(dnsString: DnsString)

  //def questionsCodec: Codec[Question] =
  //  ("question"  | (dnsString :: ("type" | constant(hex"00 01")) :: ("class" | constant(hex"00 01")))).dropUnits.as[Question]

  //def questionsCodec: Codec[Questions] =
  //  ("questions" | vector(dnsString)).as[Questions]

  //def questionCodec =
  //  vector((dnsString :: ("type" | constant(hex"00 01")) :: ("class" | constant(hex"00 01"))))

  def xflags = (
    ("Response"               | ignore(1)) :: 
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
  
  def dnsTestResponseCodec = (
    ("Transaction ID"         | uint16)               ::
    xflags :::
    ("Questions"              | constant(hex"00 01")) ::
    ("Answer RRs"             | ignore(16))           ::
    ("Authority RRs"          | ignore(16))           ::
    ("Additional RRs"         | ignore(16))           ::
    ("Query Name"                   | dnsString)             //queries?
   //("Address ResourceRecords"                | vector(resourceRecordCodec))
  ).dropUnits //.as[Response]

  def dnsResponseCodec = (
    ("Transaction ID"         | uint16)               ::
    xflags :::
    ("Questions"              | constant(hex"00 01")) ::
    ("Answer RRs"             | ignore(16))           ::
    ("Authority RRs"          | ignore(16))           ::
    ("Additional RRs"         | ignore(16))           ::
    ("Query Name"             | dnsString)            :: //queries?
    ("Address ResourceRecords"                | vector(resourceRecordCodec))
  ).dropUnits.as[Response]
}

/*
import scalaz.NonEmptyList
import dnsclient._
import Test._

import scodec.bits._
import dnsclient.DnsCodec._ 

client(NonEmptyList("reddit", List("com"): _*)).run.run 
val bits = hex"0x0672656464697403636f6d0000010001c00c000100010000000b00049765018cc00c000100010000000b00049765c18cc00c000100010000000b00049765818cc00c000100010000000b00049765418c".take(16).toBitVector 
dnsString.decode(bits)

dnsString.encode(DnsString(NonEmptyList("ab", List("org"): _*))).flatMap(DnsString.decode)


he0x75c0818000010004000000000672656464697403636f6d0000010001c00c000100010000000b00049765018cc00c000100010000000b00049765c18cc00c000100010000000b00049765818cc00c000100010000000b00049765418c"
                                                   "0x00010001c00c000100010000000b00049765018cc00c000100010000000b00049765c18cc00c000100010000000b00049765818cc00c000100010000000b00049765418c
                            0x0672656464697403636f6d0000010001c00c000100010000000b00049765018cc00c000100010000000b00049765c18cc00c000100010000000b00049765818cc00c000100010000000b00049765418c 
                                                          "0xc00c000100010000000b00049765018cc00c000100010000000b00049765c18cc00c000100010000000b00049765818cc00c000100010000000b00049765418c
*/
