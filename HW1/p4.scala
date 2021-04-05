object p4{
  def main(args: Array[String]): Unit = {
    def partial2[A,B,C,D](a:A,f:(A,B,C)=>D):(B,C)=>D={
      (b:B,c:C) => f(a, b, c)
    }
    def formatStudentInfo(isGraduate: Boolean, year: Int, name: String):String = {
      val grad = if (isGraduate) "graduate" else "undergraduate"
      "%s student %s enrolled in year %d".format(grad, name, year)
    }
    def partialFormatStudentInfo(year:Int,name:String):String={
      partial2(true,formatStudentInfo)(year,name)
    }
    val year=2019
    val name="John"
    println(partialFormatStudentInfo(year,name))

    def htmlTag(elem: String, inner: String) = "<%s>%s</%s>".format(elem, inner, elem)
    val mkBold=(inner:String)=>htmlTag("B", inner)
    val mkItalic=(inner:String)=>htmlTag("I", inner)

    def compose[A,B,C](f: B => C, g: A => B)={
      a:A=>f(g(a))
    }

    val mkBoldItalic1=(inner:String)=>mkItalic.compose(mkBold)(inner)
    println(mkBoldItalic1("..."))
    val mkBoldItalic2=(inner:String)=>mkBold.andThen(mkItalic)(inner)
    println(mkBoldItalic2("..."))
    val mkBoldItalic3=(inner:String)=>compose(mkItalic,mkBold)(inner)
    println(mkBoldItalic3("..."))

    def curry2[A,B,C,D](f:(A,B,C)=>D)={
      (a:A)=>(b:B,c:C)=>f(a,b,c)
    }
    val mkStdInfo=curry2(formatStudentInfo)
    println(mkStdInfo(true)(year,name))

    val formatUndergradStdInfo=mkStdInfo(false)
    println(formatUndergradStdInfo(year,name))
  }
}