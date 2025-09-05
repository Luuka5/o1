package o1.rainfall
import scala.util.control.Breaks._

// What goes here is described in Chapter 7.1.

def averageRainfall(nums: Vector[Int]): Option[Int] =
  var sum = 0
  var count = 0
  breakable {
    nums.foreach {
      case(num) => {
        if num == 999999 then {
          break
        }
        if num >= 0 then {
          sum += num
          count += 1
        }
      }
    }
  }

  if count == 0 then {
    None
  } else {
    Some(sum / count)
  }

def averageRainfallFromStrings(strs: Vector[String]): Option[Int] =
  val nums = strs.flatMap{
    str => str.toIntOption
  }

  averageRainfall(nums)