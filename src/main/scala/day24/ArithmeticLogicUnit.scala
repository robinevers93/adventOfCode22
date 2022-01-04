package day24

import scala.annotation.tailrec

object ArithmeticLogicUnit {

  type Alu =  Map[Char, Long]
  val startingAlu: Alu = Map('x' -> 0, 'y' -> 0, 'z' -> 0, 'w' -> 0)

  @tailrec
  def executeInstructions(instructions: List[String], inputStream: List[Int], result: Alu = startingAlu): Alu = instructions match {
    case Nil =>
      result
    case h :: t =>
      val (action, args) = (h.split(" ").head, h.split(" ").tail)
      action match {
        case "inp" =>
          executeInstructions(t, inputStream.tail, result.updated(args.head.head, inputStream.head.toLong))
        case "add" =>
          executeInstructions(t, inputStream,
            result.updated(args.head.head, result(args.head.head) + args(1).toLongOption.getOrElse(result(args(1).head))))
        case "mul" =>
          executeInstructions(t, inputStream,
            result.updated(args.head.head, result(args.head.head) * args(1).toLongOption.getOrElse(result(args(1).head))))
        case "div" =>
          executeInstructions(t, inputStream,
            result.updated(args.head.head, result(args.head.head) / args(1).toLongOption.getOrElse(result(args(1).head))))
        case "mod" =>
          executeInstructions(t, inputStream,
            result.updated(args.head.head, result(args.head.head) % args(1).toLongOption.getOrElse(result(args(1).head))))
        case "eql" =>
          executeInstructions(t, inputStream,
            result.updated(args.head.head, if (result(args.head.head) == args(1).toLongOption.getOrElse(result(args(1).head))) 1 else 0))
      }
  }

}
