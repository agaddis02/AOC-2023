enum Color:
  case Red, Green, Blue
case class Cube (quantity: Int, color: Color)
case class Bag (var cubes: List[Cube])

// part 1 preloaded bag
val PreloadedBag = Bag(
  List(
    Cube(14, Color.Blue),
    Cube(13, Color.Green),
    Cube(12, Color.Red)
    )
  )

@main def hello: Unit =
  val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day2" / "p1.txt"
  val lines: Seq[String] = os.read.lines(path)
  
  var p1 = 0
  var p2 = 0
  for (line <- lines) {
    p1 += part1(line)
    p2 += part2(line)
  }
  println(s"Part 1: $p1")
  println(s"Part 2: $p2")


val part2 = (line: String) => {
    val game = line.split(":")(0).split(" ")(1).toInt
    val sets = line.split(":")(1).split(";")
    var setValues: List[Cube] = List[Cube]()
    var counter = 0
    for (set <- sets) {
      counter += 1
      println(s"Set: ${set.toString()}")
      val cubes = set.split(",")
      var tempBag = PreloadedBag.copy()
      println(s"Temp Bag: ${tempBag.toString()}")
      val listCubes: List[Cube] = List[Cube]()
      // println(s"Cubes: ${cubes.toString()}")
      for (cube <- cubes) {
        val cleanedCube = cube.trim()
        println(s"Cube: ${cube.toString()}")
        // print(s"Cube split: ${cube.split(" ")}")
        val quantity = cleanedCube.split(" ")(0).toInt
        println(s"Quantity: ${quantity.toString()}")
        val color = cleanedCube.split(" ")(1) match {
          case "red" => Color.Red
          case "green" => Color.Green
          case "blue" => Color.Blue
        }
        val currCube = Cube(quantity, color)
        println(s"Curr Cube: ${currCube.toString()}")
        setValues = setValues.appendedAll(List(currCube))
      }
      // setValues :+ listCubes

    }
    var maxValuesByColor: Map[Color, Int] = Map[Color, Int]()
    println(s"Set Values: ${setValues.toString()}")

    setValues.foreach( cube => {
      println(s"Cube: ${cube.toString()}")
      val color = cube.color
      val quantity = cube.quantity
      val currMax = maxValuesByColor.get(color)
      if (currMax.isEmpty) {
        maxValuesByColor = maxValuesByColor + (color -> quantity)
      } else {
        if (quantity > currMax.get) {
          maxValuesByColor = maxValuesByColor + (color -> quantity)
        }
      }
    })

    maxValuesByColor.foreach( (color, quantity) => {
      println(s"Color: $color, Quantity: $quantity")
    })

    val result = maxValuesByColor.values.foldLeft(1)(_ * _)
    println(s"Result: $result")

    result
}

val part1 = (line: String) => {
    val game = line.split(":")(0).split(" ")(1).toInt
    val sets = line.split(":")(1).split(";")

    var currentBag = Bag(List[Cube]())
    var brokes: List[Boolean] = List[Boolean]()
    for (set <- sets) {
      println(s"Set: ${set.toString()}")
      val cubes = set.split(",")
      var tempBag = PreloadedBag.copy()
      println(s"Temp Bag: ${tempBag.toString()}")
      // println(s"Cubes: ${cubes.toString()}")
      for (cube <- cubes) {
        val cleanedCube = cube.trim()
        println(s"Cube: ${cube.toString()}")
        // print(s"Cube split: ${cube.split(" ")}")
        val quantity = cleanedCube.split(" ")(0).toInt
        println(s"Quantity: ${quantity.toString()}")
        val color = cleanedCube.split(" ")(1) match {
          case "red" => Color.Red
          case "green" => Color.Green
          case "blue" => Color.Blue
        }
        println(s"Color and Quantity: ${color.toString()}, ${quantity.toString()}")
        var currCube = Cube(quantity, color)
        val correspondingCube = tempBag.cubes.find(cube => cube.color == currCube.color)
        println(s"Current Cube: ${currCube.toString()}")
        println(s"Corresponding Cube: ${correspondingCube.toString()}")
        val index = tempBag.cubes.indexOf(correspondingCube.get)
        val newQuantity = tempBag.cubes(index).quantity - currCube.quantity
        var newCube = Cube(newQuantity, currCube.color)
        println(s"New Cube: ${newCube.toString()}")
        tempBag.cubes = tempBag.cubes.updated(index, newCube)
        currentBag = tempBag.copy()
      }

      val broke = currentBag.cubes.map(cube => cube.quantity >= 0)
      brokes = brokes ++ broke
    }

    if (brokes.contains(false)) {
      println(s"Game $game broke!")
      0
    } else {
      println(s"Game $game passed!")
      game
    }
}