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

  def up = this + Vector2.Up
  def down = this + Vector2.Down
  def left = this + Vector2.Left
  def right = this + Vector2.Right
  def surrounding = Vector2.SurroundingVectors.map(this + _).toSeq

  override def toString = s"<$x, $y>"
}

object Vector2 {
  val Min = Vector2(Int.MinValue, Int.MinValue)
  val Max = Vector2(Int.MaxValue, Int.MaxValue)

  val Down = Vector2(0, 1)
  val Up = Vector2(0, -1)
  val Left = Vector2(-1, 0)
  val Right = Vector2(1, 0)
  val SurroundingVectors = Set(
    Vector2.Up,
    Vector2.Up + Vector2.Right,
    Vector2.Right,
    Vector2.Down + Vector2.Right,
    Vector2.Down,
    Vector2.Down + Vector2.Left,
    Vector2.Left,
    Vector2.Up + Vector2.Left
  )
}