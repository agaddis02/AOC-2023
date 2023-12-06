package day5

case class Rule(destinationRangeStart: Long, sourceRangeStart: Long, rangeLength: Long)

// case class CategoryConverter(rules: List[Rule])
// Simplified CategoryConverter
case class CategoryConverter(rules: List[Rule]) {
    def convert(value: Long): Long = {
        rules.find(rule => rule.sourceRangeStart <= value && value < rule.sourceRangeStart + rule.rangeLength) match {
            case Some(rule) => rule.destinationRangeStart + (value - rule.sourceRangeStart)
            case None => value
        }
    }
}


val checkIsCharacterRegex = "(?=[a-zA-Z]|$)"
val defaultValue = (key: Long) => key

@main def Part1: Unit =
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day5" / "p1.txt"
    val input: String = os.read(path)

    lazy val seedNumbers: List[Long] = input.split("seeds: ")(1).split(checkIsCharacterRegex, 2)(0).trim().split("\\s+").map(_.toLong).toList
    // val seedNumbers = seedNumbersList.map
    println("Doing mapping")
    lazy val seedToSoilMapping = (parseRulesToCategoryConvertor(input, "seed-to-soil map:"))
    lazy val soilToFertilizerMapping = (parseRulesToCategoryConvertor(input, "soil-to-fertilizer map:"))
    lazy val fertilizerToWaterMapping = (parseRulesToCategoryConvertor(input, "fertilizer-to-water map:"))
    lazy val waterToLightMapping = (parseRulesToCategoryConvertor(input, "water-to-light map:"))
    lazy val lightToTemperatureMapping = (parseRulesToCategoryConvertor(input, "light-to-temperature map:"))
    lazy val temperatureToHumidityMapping = (parseRulesToCategoryConvertor(input, "temperature-to-humidity map:"))
    lazy val humidityToLocationMapping = (parseRulesToCategoryConvertor(input, "humidity-to-location map:"))
    println("Finished mapping")
    var minLocation = 1000000000000000000L
    // var locations: List[Long] = List()
    for (seed <- seedNumbers) {
        // println(s"Seed $seed")
        val soil = seedToSoilMapping.convert(seed)
        // println(s"Seed $seed, soil $soil")
        val fertilizer = soilToFertilizerMapping.convert(soil)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer")
        val water = fertilizerToWaterMapping.convert(fertilizer)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water")
        val light = waterToLightMapping.convert(water)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light")
        val temperature = lightToTemperatureMapping.convert(light)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light, temperature $temperature")
        val humidity = temperatureToHumidityMapping.convert(temperature)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light, temperature $temperature, humidity $humidity")
        val location = humidityToLocationMapping.convert(humidity)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light, temperature $temperature, humidity $humidity, location $location")
        // locations = locations :+ location
        if (location < minLocation) {
            minLocation = location
        }
    }

    println(s"Lowest Location is ${minLocation}")

@main def Part2: Unit =
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day5" / "p1.txt"
    val input: String = os.read(path)

    lazy val seedNumbers: Iterable[Long] = generateSeedNumbers(input)
    // val seedNumbers = seedNumbersList.map
    println("Doing mapping")
    lazy val seedToSoilMapping = (parseRulesToCategoryConvertor(input, "seed-to-soil map:"))
    lazy val soilToFertilizerMapping = (parseRulesToCategoryConvertor(input, "soil-to-fertilizer map:"))
    lazy val fertilizerToWaterMapping = (parseRulesToCategoryConvertor(input, "fertilizer-to-water map:"))
    lazy val waterToLightMapping = (parseRulesToCategoryConvertor(input, "water-to-light map:"))
    lazy val lightToTemperatureMapping = (parseRulesToCategoryConvertor(input, "light-to-temperature map:"))
    lazy val temperatureToHumidityMapping = (parseRulesToCategoryConvertor(input, "temperature-to-humidity map:"))
    lazy val humidityToLocationMapping = (parseRulesToCategoryConvertor(input, "humidity-to-location map:"))
    println("Finished mapping")
    var minLocation = 1000000000000000000L
    // var locations: List[Long] = List()
    for (seed <- seedNumbers) {
        // println(s"Seed $seed")
        val soil = seedToSoilMapping.convert(seed)
        // println(s"Seed $seed, soil $soil")
        val fertilizer = soilToFertilizerMapping.convert(soil)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer")
        val water = fertilizerToWaterMapping.convert(fertilizer)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water")
        val light = waterToLightMapping.convert(water)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light")
        val temperature = lightToTemperatureMapping.convert(light)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light, temperature $temperature")
        val humidity = temperatureToHumidityMapping.convert(temperature)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light, temperature $temperature, humidity $humidity")
        val location = humidityToLocationMapping.convert(humidity)
        // println(s"Seed $seed, soil $soil, fertilizer $fertilizer, water $water, light $light, temperature $temperature, humidity $humidity, location $location")
        // locations = locations :+ location
        if (location < minLocation) {
            minLocation = location
        }
    }

    println(s"Lowest Location is ${minLocation}")


def parseRulesToCategoryConvertor(input: String, label: String): CategoryConverter = {
    return CategoryConverter(input.split(label)(1).split(checkIsCharacterRegex, 2)(0).trim().linesIterator.map( line => {
        val nums = line.split("\\s").toList
        Rule(nums(0).toLong, nums(1).toLong, nums(2).toLong)
    }).toList)
}

def generateSeedNumbers(input: String): Iterable[Long] = new Iterable[Long] {
  override def iterator: Iterator[Long] = {
    val seedsPart = input.split("seeds: ")(1).split(checkIsCharacterRegex, 2)(0).trim()
    val seedRanges = seedsPart.split("\\s+").map(_.toLong).grouped(2)

    seedRanges.flatMap {
      case Array(start, length) => start until (start + length)
      case _ => Iterator.empty
    }.iterator
  }
}

def generateConversionMaping(convertor: CategoryConverter): Map[Long, Long] = {

    var mapped: Map[Long, Long] = Map()

    for (rule <- convertor.rules) {
        val destinations = rule.destinationRangeStart until rule.destinationRangeStart + rule.rangeLength
        val sources = rule.sourceRangeStart until rule.sourceRangeStart + rule.rangeLength
        val map: Map[Long, Long] = sources.zip(destinations).toMap

        mapped = mapped ++ map
    }
    return mapped

}



