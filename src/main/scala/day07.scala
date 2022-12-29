import scala.collection.mutable
import scala.io.Source

abstract sealed class FileSystem {
  def size():Long
}
case class File(name: String, size: Long) extends FileSystem
case class Folder(name: String, parent: Folder, var files: mutable.HashMap[String, FileSystem]) extends FileSystem {
  def size = files.values.map(_.size).sum
}
object day07 extends App {

  private def findMatchingFolder(parent: Folder, condition: (Folder => Boolean)): List[Folder] = {

    val smallSubFolders = parent.files.values.map(_ match {
      case _: File => List[Folder]()
      case subFolder: Folder => findMatchingFolder(subFolder, condition)
    }).fold(List[Folder]())((agg, next) => agg++next)

    if (condition(parent)) {
      smallSubFolders :+ parent
    } else {
      smallSubFolders
    }
  }

  private val instructions = Source.fromURI(getClass.getResource("day07.txt").toURI)
    .getLines().toList

  val rootDir = Folder("/", null, mutable.HashMap[String, FileSystem]())
  var currentDir = rootDir

  instructions.drop(1).foreach(instr => {
    if (instr.startsWith("$ cd")) {
      instr.substring(5) match {
        case ".." => currentDir = currentDir.parent
        case folderName: String => currentDir = currentDir.files.get(folderName).get.asInstanceOf[Folder]
      }
    } else if (instr.startsWith("$ ls")) {
      // skip
    } else {
      val lsOutputSplit = instr.split(" ")
      lsOutputSplit(0) match {
        case "dir" => {
          currentDir.files.put(lsOutputSplit(1), Folder(lsOutputSplit(1), currentDir, mutable.HashMap()))
        }
        case size: String => currentDir.files.put(lsOutputSplit(1), File(lsOutputSplit(1), lsOutputSplit(0).toLong))
      }
    }
  })

  println(s"Day7 Part1 ${findMatchingFolder(rootDir, f => f.size <= 100000).map(_.size).sum}")

  val usedDiskSpace = rootDir.size
  val requiredDeletion = 30000000 - (70000000 -usedDiskSpace)

  if (requiredDeletion > 0) {
    println(s"Day7 Part2 ${findMatchingFolder(rootDir, f => true).map(_.size).sorted.find(size => size > requiredDeletion).getOrElse(-1)}")
  } else {
    println(s"Day7 Part2 No deletion required")
  }



}
