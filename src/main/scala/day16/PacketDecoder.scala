package day16

object PacketDecoder {

  trait Packet{}

  case class Literal(version: Int, typeId: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, packetType: Int, subpackets: List[Packet]) extends Packet

  def toDecimal(s: String): Long = BigInt(s, 2).toString(10).toLong

  def stringToLiteral(string: String): (Literal, String) = {
    val packetVersion = Integer.parseInt(string.take(3), 2)
    val typeId = Integer.parseInt(string.slice(3, 6))
    val groups = string.drop(6).grouped(5).toList
    val endOfPacket = groups.takeWhile(_.head == '1').length + 1
    val literalInc = groups.take(endOfPacket).map(_.tail)
    val value = toDecimal(literalInc.mkString(""))
    val length = 6 + 5 * endOfPacket
    val remainder = string.drop(length)
    (Literal(packetVersion, typeId, value), remainder)
  }

  def stringToTypeOneOperator(string: String): (Operator, String) = {
    val packetVersion = Integer.parseInt(string.take(3), 2)
    val typeId = Integer.parseInt(string.slice(3, 6), 2)
    val number = Integer.parseInt(string.slice(8, 18),2)
    val subPacketBits = string.drop(18)
    val subPackets  = nStringsToPacket(subPacketBits, number)
    (Operator(packetVersion, typeId, 1, subPackets._1), subPackets._2)
  }

  def stringToTypeZeroOperator(string: String): (Operator, String) = {
    val packetVersion = Integer.parseInt(string.take(3), 2)
    val typeId = Integer.parseInt(string.slice(3, 6), 2)
    val length = Integer.parseInt(string.slice(8, 22),2)
    val subPacketBits = string.slice(22, 22 + length)
    val subPackets = getSubpackets(subPacketBits)
    val remainder = string.drop(22 + length)
    (Operator(packetVersion, typeId, 0, subPackets), remainder)
  }

  def nStringsToPacket(string: String, n: Int): (List[Packet], String) =
    if (n == 0)
      (List(), string)
    else {
      val (packet, remainder) = parsePacket(string)
      val (packets, remainder2) = nStringsToPacket(remainder, n - 1)
      (packets.prepended(packet), remainder2)
    }

  def getSubpackets(string: String): List[Packet] =
    if (string.isEmpty)
      List()
    else
      getSubpackets(parsePacket(string)._2).prepended(parsePacket(string)._1)


  def parsePacket(string: String): (Packet, String) =
    if (string.slice(3, 6) == "100")
      stringToLiteral(string)
    else if (string(6).asDigit == 1)
      stringToTypeOneOperator(string)
    else
      stringToTypeZeroOperator(string)


  def hexToBinary(string: String): String = string.map {
    case '0' => "0000"
    case '1' => "0001"
    case '2' => "0010"
    case '3' => "0011"
    case '4' => "0100"
    case '5' => "0101"
    case '6' => "0110"
    case '7' => "0111"
    case '8' => "1000"
    case '9' => "1001"
    case 'A' => "1010"
    case 'B' => "1011"
    case 'C' => "1100"
    case 'D' => "1101"
    case 'E' => "1110"
    case 'F' => "1111"
  }.mkString("")

  def parseHexPacket(string: String): Packet =
    parsePacket(hexToBinary(string))._1

  def versionNumberSum(packet: Packet): Int = packet match {
    case Literal(version, _, _) => version
    case Operator(version, _, _, subpackets) => version + subpackets.map(versionNumberSum).sum
  }

  def combineValues(typeId: Int, subvalues: List[Long]): Long = typeId match {
    case 0 => subvalues.sum
    case 1 => subvalues.product
    case 2 => subvalues.min
    case 3 => subvalues.max
    case 5 => if (subvalues.head > subvalues.last) 1 else 0
    case 6 => if (subvalues.head < subvalues.last) 1 else 0
    case 7 => if (subvalues.head == subvalues.last) 1 else 0
  }

  def getValue(packet: Packet): Long = packet match {
    case Literal(_, _, value) => value
    case Operator(_, typeId, _, subpackets) =>
      val subpacketValues = subpackets.map(getValue)
      combineValues(typeId, subpacketValues)
  }

}
