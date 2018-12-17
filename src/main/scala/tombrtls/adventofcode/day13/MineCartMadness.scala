package tombrtls.adventofcode.day13

import tombrtls.adventofcode.{Assignment, FileHelper, Vector2}

sealed abstract class Direction {
  def toVector(): Vector2 = {
    this match {
      case Up => Vector2(0, -1)
      case Down => Vector2(0, 1)
      case Left => Vector2(-1, 0)
      case Right => Vector2(1, 0)
    }
  }

  def turnLeft = this match {
      case Up => Left
      case Left => Down
      case Down => Right
      case Right => Up
    }

  def turnRight = this match {
    case Up => Right
    case Left => Up
    case Down => Left
    case Right => Down
  }
}

case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

case class Cart(name: String, location: Vector2, direction: Direction, turnCount: Int = 0) {
  def moveOnTracks(tracks: MineTracks): Cart = {
    val nextLocation = location + direction.toVector()
    tracks.pathAt(nextLocation) match {
      case Horizontal | Vertical => {
        println(s"${name} moving FORWARD from ${location} to ${nextLocation}")
        Cart(name, nextLocation, direction, turnCount)
      }

      case LeftTurn => {
        direction match {
          case Up | Down  => {
            println(s"${name} turning LEFT from ${location} to ${nextLocation}")
            Cart(name, nextLocation, direction.turnLeft, turnCount)
          }

          case Left | Right => {
            println(s"${name} turning RIGHT from ${location} to ${nextLocation}")
            Cart(name, nextLocation, direction.turnRight, turnCount)
          }
        }
      }
      case RightTurn => {
        direction match {
          case Up | Down  => {
            println(s"${name} turning RIGHT from ${location} to ${nextLocation}")
            Cart(name, nextLocation, direction.turnRight, turnCount)
          }

          case Right | Left => {
            println(s"${name} turning LEFT from ${location} to ${nextLocation}")
            Cart(name, nextLocation, direction.turnLeft, turnCount)
          }
        }
      }
      case Intersection => {
        turnCount % 3 match {
          case 0 => {
            println(s"${name} turning LEFT from ${location} to ${nextLocation}")
            Cart(name, nextLocation, direction.turnLeft, turnCount + 1)
          }

          case 1 => {
            println(s"${name} moving FORWARD to <${location.x}, ${location.y}>")
            Cart(name, nextLocation, direction, turnCount + 1)
          }

          case 2 => {
            println(s"${name} turning RIGHT from ${location} to ${nextLocation}")
            Cart(name, nextLocation, direction.turnRight, turnCount + 1)
          }
        }
      }
      case Nothing => {
        printf("The fuck?")
        this
      }
    }
  }
}

case class MineTracks(list: Seq[Seq[Path]]) {
  def pathAt(vector: Vector2): Path = {
    list(vector.y)(vector.x)
  }
}

sealed abstract class Path
case object Horizontal extends Path
case object Vertical extends Path
case object RightTurn extends Path
case object LeftTurn extends Path
case object Intersection extends Path
case object Nothing extends Path

object MineCartMadness {
  def processInput(lines: Seq[String]) = {
    var carts = Seq[Cart]()
    val input = lines
      .zipWithIndex
      .map { case (line, y) =>
        line.zipWithIndex.map { case (char, x) =>
          char match {
            case '|' => Vertical
            case '-' => Horizontal
            case '/' => RightTurn
            case '\\' => LeftTurn
            case '+' => Intersection
            case '>' => {
              carts = carts :+ Cart(s"Cart ${carts.length}", Vector2(x, y), Right)
              Horizontal
            }
            case '<' => {
              carts = carts :+ Cart(s"Cart ${carts.length}", Vector2(x, y), Left)
              Horizontal
            }
            case '^' => {
              carts = carts :+ Cart(s"Cart ${carts.length}", Vector2(x, y), Up)
              Vertical
            }
            case 'v' => {
              carts = carts :+ Cart(s"Cart ${carts.length}", Vector2(x, y), Down)
              Vertical
            }
            case _ => Nothing
          }
        }
      }

    (MineTracks(input), carts)
  }

  def detectCollision(carts: Seq[Cart]): Option[Vector2] = {
    val allCollisions = (for (
      cart1 <- carts;
      cart2 <- carts if (cart1 != cart2 && cart1.location == cart2.location)
    ) yield (cart1.location)).toSet


    println(s"Collisions: ${allCollisions.toSeq.length}")
    allCollisions.headOption
  }

  def detectCollision(cart: Cart, otherCarts: Seq[Cart]): Option[Vector2] = {
    otherCarts
      .find { otherCart => otherCart != cart && otherCart.location == cart.location }
      .map(_.location)
  }

  implicit def ordering[A <: Vector2]: Ordering[Vector2] = new Ordering[Vector2] {
    override def compare(first: Vector2, second: Vector2): Int = {
      first.y compare second.y match {
        case 0 => first.x compare second.x
        case comparision => comparision
      }
    }
  }

  def tickUntilCollision(map: MineTracks, carts: Seq[Cart]): Vector2 = {
    def innerTick(tick: Int, remaingCarts: Seq[Cart], movedCarts: Seq[Cart]): Vector2 = {
      remaingCarts match {
        case cart :: xs => {
          val updatedCart = cart.moveOnTracks(map)
          detectCollision(updatedCart, remaingCarts ++ movedCarts) match {
            case None => innerTick(tick, xs, updatedCart +: movedCarts)
            case Some(location) => location
          }
        }
        case Nil => {
          println(s"======= Next tick ${tick} =======")
          innerTick(tick + 1, movedCarts.sortBy(_.location), Seq.empty)
        }
      }
    }

    innerTick(0, carts.sortBy(_.location), Seq.empty)
  }

  def tickUntilOneCartLeft(map: MineTracks, carts: Seq[Cart]): Vector2 = {
    def innerTick(tick: Int, remaingCarts: Seq[Cart], movedCarts: Seq[Cart]): Vector2 = {
      remaingCarts match {
        case cart :: xs => {
          val updatedCart = cart.moveOnTracks(map)
          detectCollision(updatedCart, remaingCarts ++ movedCarts) match {
            case None => innerTick(tick, xs, updatedCart +: movedCarts)
            case Some(location) => innerTick(tick, xs.filterNot(_.location == location), movedCarts.filterNot(_.location == location))
          }
        }
        case Nil => {
          if (movedCarts.length == 1) {
            movedCarts.head.location
          } else {
            println(s"======= Next tick ${tick} =======")
            innerTick(tick + 1, movedCarts.sortBy(_.location), Seq.empty)
          }
        }
      }
    }

    innerTick(0, carts.sortBy(_.location), Seq.empty)
  }

  def main(args: Array[String]): Unit = {
    var (map, carts) = processInput(FileHelper.readLines("/day13/input.txt"))

    val location = tickUntilCollision(map, carts)
    printf(s"Collision at: ${location}")

    val lastCartLocation = tickUntilOneCartLeft(map, carts)
    printf(s"One cart standing at ${lastCartLocation}")
  }
}
