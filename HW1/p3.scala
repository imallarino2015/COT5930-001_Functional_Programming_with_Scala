object p3{
  @annotation.tailrec
  def trueForAll(bools: List[Boolean]): Boolean = {
    if(bools.isEmpty){
      true
    } else if(!bools(0)){
      false
    } else {
      trueForAll(bools.take(0)++bools.drop(1))
    }
  }

  def countWithProperty[T](xs: List[T], p: T => Boolean): Int = {
    @annotation.tailrec
    def recCountWithProperty(xs: List[T], p: T => Boolean,idx:Int=0,count:Int=0): Int = {
      if(idx>=xs.length) {
        count
      } else if(p(xs(idx))) {
        recCountWithProperty(xs,p,idx+1,count+1)
      } else {
        recCountWithProperty(xs, p, idx+1, count)
      }
    }
    recCountWithProperty(xs,p)
  }

  def scalarProduct(x: Array[Double], y: Array[Double]) : Double={
    @annotation.tailrec
    def recScalarProduct(x: Array[Double], y: Array[Double],i:Int=0,j:Int=0, total:Double=0): Double={
      if(i>=x.length || j>=y.length) {
        total
      } else {
        recScalarProduct(x,y,i+1,j+1,total+x(i)*y(j))
      }
    }
    recScalarProduct(x,y)
  }

  def countCommonElements[T](xs: Array[T], ys: Array[T], p: (T, T) => Boolean): Int={
    @annotation.tailrec
    def recCountCommonElements[T](xs: Array[T], ys: Array[T], p: (T, T) => Boolean,i:Int=0,j:Int=0,count:Int=0): Int={
      if(j>=ys.length) {
        count
      } else if(i>=xs.length) {
        recCountCommonElements(xs, ys, p, 0, j+1, count)
      } else if(p(xs(i),ys(j))) {
        recCountCommonElements(xs, ys, p, i+1, j, count+1)
      } else {
        recCountCommonElements(xs, ys, p, i+1, j, count)
      }
    }
    recCountCommonElements(xs,ys,p)
  }

  def main(args: Array[String]): Unit = {
    var bl:List[Boolean]=List(true,true,true,true,true)
    println(trueForAll(bl))
    bl=List(true,false,true,true,true)
    println(trueForAll(bl))

    val word = "queueing"
    val vowelsList = "aeiou".toList
    println(countWithProperty(word.toList, (x: Char) => vowelsList.contains(x)))

    val x:Array[Double] = Array(1,2,-3,4,5)
    val y:Array[Double] = Array(5,-4,3,-2,1)
    println(scalarProduct(x, y))

    val xs = Array("cwm","fjord","veg","balks","nth","pyx","quiz")
    val ys = Array("cwm","fjord","bank","glyphs","vext","quiz")
    println(countCommonElements(xs, ys, (x: String, y: String) => x.toUpperCase == y.toUpperCase))
  }
}