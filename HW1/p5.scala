import scala.math

trait Shape {
  def area(): Double
  def perimeter(): Double
  override def toString: String =
    "Area: " + area() + "\n" +
    "Perimeter: " + perimeter()
}

class Circle(r:Double) extends Shape{
  override def area(): Double = math.Pi*math.pow(r,2)
  override def perimeter(): Double = 2*math.Pi*r
}

class Rectangle(l:Double,w:Double) extends Shape{
  override def area(): Double = l*w
  override def perimeter(): Double = 2*l+2*w
}

object p5 {
  def main(args: Array[String]) {
    val c=new Circle(4)
    val r=new Rectangle(3,5)
    println(c)
    println(r)
    object anonShape extends Shape{
      val l=3
      val w=5
      override def area(): Double = l*w
      override def perimeter(): Double = 2*l+2*w
    }
    println(anonShape)
  }
}