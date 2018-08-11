package ca.vgorcinschi

import scala.language.implicitConversions
/*
  The walker

  The walker starts from point O, walks along OA, AB and BC. When he is in C (C will be in the upper half-plane), what is the distance CO? What is the angle tOC in positive degrees, minutes, seconds?

  Angle tOA is alpha (here 45 degrees), angle hAB is beta (here 30 degrees), angle uBC is gamma(here 60 degrees).

  Task
  function solve(a, b, c, alpha, beta, gamma) with parameters

  a, b, c: positive integers in units of distance
  alpha, beta, gamma: positive integers in degrees (positive angles are anticlockwise)

  returns an array

  -first element: distance CO (rounded to the nearest integer)
  -then angle tOC with the third following elements:
    *second element of the array: number of degrees in angle tOC (truncated positive integer)
    *third element of the array: number of minutes in angle tOC (truncated positive integer)
    *fourth element of the array: number of seconds in angle tOC (truncated positive integer)
 */
object Walker {

  case class Point(x: Double, y: Double){
    def moveLeft(by: Double) = Point(by - x, y)
    def moveRight(by: Double) = Point(by + x, y)
    def moveUp(by: Double) = Point(x, by + y)
    def moveDown(by: Double) = Point(x, by - y)
    def inverse() = Point(y, x)
  }

  implicit def roundUp(d: Double): Int = math.ceil(d).toInt

  def solve(a: Int, b: Int, c: Int, alpha: Int, beta: Int, gamma: Int): Array[Int] = {
    val pointA = pointHypotenuse(a, alpha)
    val pointB = pointHypotenuse(b, beta).inverse().moveLeft(pointA.x).moveUp(pointA.y)
    val pointC = pointHypotenuse(c, gamma).moveLeft(pointB.x).moveDown(pointB.y)
    val coHypotenuse: Int = math.sqrt(math.pow(pointC.x, 2) + math.pow(pointC.y, 2))
    val asinCOe = math.asin(math.abs(pointC.x)/coHypotenuse)
    val tOC = 180 - asinCOe.toDegrees
    coHypotenuse +: degrees(tOC)
  }

  def pointHypotenuse(coteHypotenuse: Int, angleHypotenuse: Int): Point = {
    val sinDuAngle = math.sin(math.toRadians(angleHypotenuse))
    val coteOppose = sinDuAngle * coteHypotenuse
    Point(math.sqrt(math.pow(coteHypotenuse, 2) - math.pow(coteOppose, 2)), coteOppose)
  }

  def degrees(deg: Double): Array[Int] = {
    Stream.iterate((deg.toInt, deg - deg.toInt)){
      case (_, r) =>
        val by60 = r*60
        (by60.toInt, by60 - by60.toInt)
    }.map(_._1).take(3).toArray
  }
}