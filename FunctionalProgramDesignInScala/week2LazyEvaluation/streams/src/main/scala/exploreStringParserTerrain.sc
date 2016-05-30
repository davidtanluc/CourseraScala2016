val level =
  """------
    |--ST--
    |--oo--
    |--oo--
    |------""".stripMargin
    val level2 =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
   val level3 =
       """ST
          |oo
          |oo""".stripMargin.replaceAll("\n"," ")
//https://www.safaribooksonline.com/library/view/scala-cookbook/9781449340292/ch01s03.html
val speech = """Four score
                |and
               |seven years ago""".stripMargin
//////http://stackoverflow.com/questions/16503387/how-to-capture-inner-matched-value-in-indexwhere-vector-expression
val v = Vector(Vector(1, 2, 3),
               Vector(4, 5, 6),
               Vector(7, 8, 9))
val num = 4
v.map(_ indexOf num).zipWithIndex
//// row = Y ;; col ;; X
val Some((posY, posX)) = v.map(_ indexOf num).zipWithIndex.find(_._1 > -1)
//http://blog.bruchez.name/2011/10/scala-partial-functions-without-phd.html
//partial function with lift
val pets = List("cats","dogs","lizards")

pets lift(0)
pets lift(5)


val xs = List(5, 4, 2, 3, 1)
val kk1 =xs.zip(xs.tail)

kk1.indexWhere { case (x, y) => x < y }

