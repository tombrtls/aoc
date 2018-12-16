package tombrtls.adventofcode

case class Vector2(x: Int, y: Int) {
  def plus(other: Vector2): Vector2 = {
    Vector2(x + other.x, y + other.y)
  }

  def plus(scaler: Int): Vector2 = {
    Vector2(x + scaler, y + scaler)
  }

  def minus(other: Vector2): Vector2 = {
    Vector2(x - other.x, y - other.y)
  }

  def minus(scaler: Int): Vector2 = {
    Vector2(x - scaler, y - scaler)
  }

  def mutliply(scaler: Int): Vector2 = {
    Vector2(x * scaler, y * scaler)
  }

  def + (other: Vector2): Vector2 = {
    this.plus(other)
  }

  def - (other: Vector2): Vector2 = {
    this.minus(other)
  }

  def + (scaler: Int): Vector2 = {
    this.plus(scaler)
  }

  def - (scaler: Int): Vector2 = {
    this.minus(scaler)
  }

  def * (scaler: Int): Vector2 = {
    this.mutliply(scaler)
  }
}

object Vector2 {
  val min = Vector2(Int.MinValue, Int.MinValue)
  val max = Vector2(Int.MaxValue, Int.MaxValue)
}