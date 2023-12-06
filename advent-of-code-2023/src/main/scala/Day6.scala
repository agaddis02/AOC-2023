package day6



case class Race(durationInMilliseconds: Int, distanceInMilimeters: Int)
case class LongRace(durationInMilliseconds: Long, distanceInMilimeters: Long)

@main def Part1: Unit =
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day6" / "p1.txt"
    val input: String = os.read(path)
    val durations: List[Int] = input.split("Time:")(1).split("D")(0).trim().split("\\s+").map(_.toInt).toList
    val distances: List[Int] = input.split("Distance:")(1).trim().split("\\s+").map(_.toInt).toList
    val races: List[Race] = durations.zip(distances).map((duration, distance) => Race(duration, distance))

    // remaing time = hold time - duration
    // total distance traveled = remaining time * hold time
    println(races) 
    var numberOfWaysToBeatRecord: Array[Int] = Array()
    for (race <- races) {

        val holdTimes = (0 until race.durationInMilliseconds + 1)
        // println(holdTimes.toList)
        val waysToWin = holdTimes.map(hold => {
            val remaingTime = race.durationInMilliseconds - hold
            val totalDistanceTraveled = remaingTime * hold
            if (totalDistanceTraveled > race.distanceInMilimeters) {
                hold
            } else {
                -1
            }
            
        }).toList

        println(waysToWin)
        numberOfWaysToBeatRecord =  numberOfWaysToBeatRecord :+ waysToWin.count( num => num != -1)
    }
    
    val output = numberOfWaysToBeatRecord.foldLeft(1)(_ * _)
    println(s"Part 1: $output")

@main def Part2: Unit =
    val path: os.Path = os.root / "Users" / "agadd1" / "Documents" / "Adam" / "GitHub" / "AOC-2023" / "inputs" / "day6" / "p1.txt"
    val input: String = os.read(path)
    lazy val duration: Long = input.split("Time:")(1).split("D")(0).trim().split("\\s+").foldLeft("")(_+_).toLong
    lazy val distance: Long = input.split("Distance:")(1).trim().split("\\s+").foldLeft("")(_+_).toLong
    val race: LongRace = LongRace(duration, distance)

    // // remaing time = hold time - duration
    // // total distance traveled = remaining time * hold time
    println(race) 
    // var numberOfWaysToBeatRecord: Array[Int] = Array()

    lazy val holdTimes = (0L to race.durationInMilliseconds + 1L)
    // println(holdTimes.toList)
    lazy val waysToWin = holdTimes.map(hold => {
        val remaingTime = race.durationInMilliseconds - hold
        val totalDistanceTraveled = remaingTime * hold
        if (totalDistanceTraveled > race.distanceInMilimeters) {
            hold
        } else {
            -1
        }
        
    }).toIterable

    // println(waysToWin)
    val output = waysToWin.count( num => num != -1)

    
    // val output = numberOfWaysToBeatRecord.foldLeft(1)(_ * _)
    println(s"Part 2: $output")

