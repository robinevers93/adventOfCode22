package day7

import day7.NoSpaceOnDevice
import day7.NoSpaceOnDevice.Directory
import org.scalatest.wordspec.AnyWordSpec

class NoSpaceOnDeviceTest extends AnyWordSpec {

  private val testInput: List[String] = general.ReadInTestData.getPuzzleInputAsList("src/test/scala/day7/sampleData.txt")("\n")
  private val input: List[String] = general.ReadInTestData.getPuzzleInputAsList("src/test/scala/day7/input.txt")("\n")

  val testDirectories: List[Directory] = NoSpaceOnDevice.readDirectories(testInput)
  val testDirectorySizes: List[Int] = testDirectories.map(NoSpaceOnDevice.directorySize(_, testDirectories))

  val directories: List[Directory] = NoSpaceOnDevice.readDirectories(input)
  val directorySizes: List[Int] = directories.map(NoSpaceOnDevice.directorySize(_, directories))

  "Reading in directories" should {

    "find all directories" in {
      assert(testDirectories.map(_.name).toSet == Set("/", "d", "e", "a"))
    }

    "calculate the correct size for each directory" in {
      val directoryToSize: Map[String, Int] =
        testDirectories.map(directory => (directory.name, NoSpaceOnDevice.directorySize(directory, testDirectories))).toMap

      assert(directoryToSize("/") == 48381165)
      assert(directoryToSize("d") == 24933642)
      assert(directoryToSize("a") == 94853)
      assert(directoryToSize("e") == 584)
    }
  }

  "Finding the sum of all small directories" should {

    "find the correct answer for the test data" in {
      val smallDirectorySizes = testDirectorySizes.filter(_ <= 100000)
      assert(smallDirectorySizes.sum == 95437)
    }

    "print the answer to the real data" in {
      val smallDirectorySizes = directorySizes.filter(_ <= 100000)
      println(smallDirectorySizes.sum)
    }
  }

  "Finding the smallest directory to delete to free up enough disk space" should {
    val totalDiskSpace = 70000000
    val requiredUnused = 30000000

    "find the correct answer for the test data" in {
      val totalUsed = NoSpaceOnDevice.directorySize(testDirectories.find(_.name == "/").get, testDirectories)
      val totalUnused = totalDiskSpace - totalUsed
      val needToFreeUp = requiredUnused - totalUnused

      val sizeOfDirectoryToDelete = testDirectorySizes.sorted.find(_ >= needToFreeUp)
      assert(sizeOfDirectoryToDelete.contains(24933642))

    }

    "print the answer to the real data" in {
      val totalUsed = NoSpaceOnDevice.directorySize(directories.find(_.name == "/").get, directories)
      val totalUnused = totalDiskSpace - totalUsed
      val needToFreeUp = requiredUnused - totalUnused

      val sizeOfDirectoryToDelete = directorySizes.sorted.find(_ >= needToFreeUp)
      println(sizeOfDirectoryToDelete)
    }
  }

}
