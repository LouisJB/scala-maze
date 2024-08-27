/**
 * Scala 2.13+ Maze 0.1 21-08-2024 Louis Botterill
 */
package maze

import scala.util.Random

import scala.swing.Panel
import scala.swing.Reactor
import scala.swing.Applet

import javax.swing.JFrame
import javax.swing.WindowConstants
import java.awt.Graphics
import java.awt.Color

object Direction extends Enumeration {
  type Direction = Value
    val North, South, East, West = Value

  val allDirections = North :: South :: East :: West :: Nil
  
  // find the inverse direction for a given direction
  def getInv(dir: Direction) = dir match {
    case North => South
    case South => North
    case East => West
    case West => East
  }
}

object Breadcrumb extends Enumeration {
  type Breadcrumb = Value
    val Forward, Backward, Clear = Value
}

import Direction._
import Breadcrumb._

object MazeModel {
  val HEIGHT = 10
  val WIDTH = 10
}
object MazeConsts {
  val frameBorder = 25
  val frameHeight = 1000 - frameBorder
  val frameWidth = frameHeight

  val delayTimeMs = 4000
  val solveDelayMs = 400
  val delayStopMs = 2000
}
import MazeModel._
import MazeConsts._
import Audio._
object BasicSound {
  def beep() = java.awt.Toolkit.getDefaultToolkit().beep()
}
case class Sounds(audioSynth: AudioSynth) {
  def beep() =
    audioSynth.tone(600, 25)
  def blip() =
    audioSynth.blip(400, 800, 20, 80)
  def sweepUp() =
    audioSynth.sweep(200, 2000, 5, 2000)
  def sweepDown() =
    audioSynth.sweep(4000, 200, -10, 4000)
}

class MazeModel(sounds: Sounds) {
  import sounds._
  var cells = Array.tabulate(WIDTH, HEIGHT)((i, j) => Cell(i, j))

  // set everything as not visited and with no trail
  def clearVisited() =
    cells.foreach(_.foreach(c => {
        c.visited = false
        c.trail = Clear
      }
    ))

  // generate maze
  def generateMaze(update: => Unit): Unit = {
    val exit = new Point(WIDTH - 1, HEIGHT - 1)
    val start = new Point(0, 0)

    // 1. Start at a particular cell, call it the "exit"
    // 2. Mark the current cell as visited, and get a list of all its neighbors.
    //      For each neighbor, starting with a randomly selected neighbor:
    //   1. If that neighbor hasn't been visited,
    //        remove the wall between this cell and that neighbor,
    //        and then recurse with that neighbor as the current cell.

    // cell 0,0 is the start, open the north wall to highlight this
    val startCell = cells(0)(0)
    startCell.clear(North)

    // cell width-1,height-1 is the exit, open the south wall to highlight this
    val endCell = cells(exit.x)(exit.y)
    endCell.clear(South)

    // recursively process the next cell
    def doNextCell(c: Cell) : Unit = {
      c.visited = true
      c.trail = Forward

      c.getRndDirections().foreach(dir => {
        getCell(c, dir) match {
          case Some(n) =>
            if (n.visited != true) {
              n.visited = true
              c.clear(dir)
              n.clear(getInv(dir))
              n.trail = Forward
              update
              doNextCell(n)
              n.trail = Clear
              update
            }
          case None =>
        }
      })
    }
    doNextCell(startCell)
  }

  def solveMaze(update :  => Unit) =
    solveMazeBFS(update)

  // find the maze solution using dfs from the start node until the end
  // node is located
  def solveMazeDFS(update: => Unit): Unit = {
    val start = new Point(0, 0)
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    val startCell = cells(0)(0)
    startCell.clear(North)

    def doNextCell(c: Cell): Unit = {
      c.visited = true
      c.trail = Forward

      c.getDirections().foreach(dir => {
        getCell(c, dir) match {
          case Some(n) => if (n.visited != true) {
            n.visited = true
            n.pi = c  // set predecessor node
            n.trail = Forward
            update
            beep()
            if (n.i == exit.x && n.j == exit.y) throw new Exception("Done")
            Thread.sleep(solveDelayMs)
            doNextCell(n)
            blip()
            n.trail = Backward
            update
          }
          case _ =>
        }
      })
      Thread.sleep(solveDelayMs)
    }
    doNextCell(startCell)
  }

  // find the maze solution using dfs from the start node until the end node is located
  def solveMazeBFS(update: => Unit): Unit = {
    val q = new collection.mutable.Queue[Cell]

    val start = new Point(0, 0)
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    val startCell = cells(0)(0)
    startCell.clear(North)

    q.enqueue(startCell)
    var gen = 0

    while (!q.isEmpty) {
      val c = q.dequeue()
      c.gen = q.size

      if (c.visited != true) {
        c.visited = true
        c.trail = Forward

        update

        if (c.i == exit.x && c.j == exit.y) throw new Exception("Done")

        c.getDirections().foreach(dir => {
          getCell(c, dir) match {
            case Some(n) if (n.visited != true) => {
              n.pi = c
              n.gen = c.gen
              q.enqueue(n)
              gen = gen + 1
            }
            case _ =>
          }
        })

        Thread.sleep(solveDelayMs) // add a little delay so we can watch the bfs explore and find the solution
      }
    }
  }

  // from the exit recurse over the trail of predecessors,
  // marking with a forward trail
  def showSolution(update: => Unit): Unit = {
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    println(">>showSolution")

    // Stream.iterate(cells(exit.n)(exit.y))(_.pi).takeWhile(_ != null)
    LazyList.iterate(cells(exit.x)(exit.y))(_.pi).takeWhile(_ != null).foreach {
      n => {
        n.trail = Forward
        update
      }
    }

    println("<<showSolution")
  }

  // get the cell if possible based on current cell and the given direction
  def getCell(c: Cell, dir: Direction): Option[Cell] = dir match {
    case North  => if (c.j > 0) Some(cells(c.i)(c.j-1)) else None
    case South  => if (c.j < HEIGHT  - 1) Some(cells(c.i)(c.j+1)) else None
    case East   => if (c.i < WIDTH - 1) Some(cells(c.i+1)(c.j)) else None
    case West   => if (c.i > 0) Some(cells(c.i-1)(c.j)) else None
  }
}

// a scala.swing.Panel, override paint(Graphics2D) to paint each of the cells
class MazePanel(m: MazeModel) extends Panel {
  override def paint(g: java.awt.Graphics2D) =
    m.cells.foreach(_.foreach(_.draw(g)))
}

class MazeMainPanel extends Applet {
  import MazeConsts._
  //import BasicSound._

  object ui extends UI with Reactor {
    import Audio._
    import AudioConsts._
    val audioSynth = AudioSynth.mkAudioSynth(sampleRate, bitDepth)
    val sounds = Sounds(audioSynth)
    import sounds._
    val m = new MazeModel(sounds)
    lazy val mp = new MazePanel(m)

    def init() =
      contents = mp

    override def start() : Unit = {
      println("Starting...")
      def update =
        this.repaint()

      println("Generating Maze")
      m.generateMaze(update)
      beep()
      Thread.sleep(delayTimeMs)

      m.clearVisited()

      println("Solving Maze using DFS")
      try {
        m.solveMazeDFS(update)
      }
      catch {
        case e : Exception =>
      }

      beep()
      Thread.sleep(delayTimeMs)

      m.clearVisited()
      m.showSolution(update)
      mp.repaint()

      sweepUp()
      Thread.sleep(delayTimeMs)

      m.clearVisited()

      println("Solving Maze using BFS")
      try {
        m.solveMazeBFS(update)
      }
      catch {
        case e : Exception =>
      }

      beep()
      Thread.sleep(delayTimeMs)

      m.clearVisited()
      m.showSolution(update)
      mp.repaint()

      sweepUp()
      Thread.sleep(delayTimeMs)

      m.clearVisited()
      this.repaint()

      sweepDown()
      Thread.sleep(delayTimeMs)

      println("Ending...")
      audioSynth.stop()
    }
  }
}

// standard Java app of the maze using JFrame
object Maze {
  def main(args: Array[String]): Unit = {
    val sizeDims = new java.awt.Dimension(frameWidth + frameBorder, frameHeight + frameBorder)
    val mazeMainPanel = new MazeMainPanel()
    mazeMainPanel.setMinimumSize(new java.awt.Dimension(sizeDims))
    val frame = new JFrame()
    frame.setBounds(0, 0, sizeDims.width, sizeDims.height)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setTitle("Simple Maze Demo v0.1")
    frame.add(mazeMainPanel)
    frame.setLocation(0, 0)
    frame.setVisible(true)
    mazeMainPanel.init
    mazeMainPanel.start
  }
}

object Cell {
  val size = frameWidth / WIDTH // each cell is square
  val cellBorder = 10
  val origin = Cell(0, 0)
}

// represent a Cell of the maze
case class Cell(i: Int, j: Int) {
  import Cell._
  var north : Boolean = true
  var south : Boolean = true
  var east : Boolean = true
  var west : Boolean = true
  var visited : Boolean = false
  var pi : Cell = null  // predecessor cell
  var trail : Breadcrumb = Clear
  var gen : Int = 0

  private val x = i * size
  private val y = j * size

  def draw(g: Graphics): Unit = {

    def fillCell() = 
      g.fillRect(x + cellBorder, y + cellBorder, size - cellBorder * 2, size - cellBorder * 2)
    
    trail match {
      case Forward => {
        val c = getColour(gen)
        g.setColor(c)
        fillCell()
      }
      case Backward => {
        g.setColor(Color.BLUE)
        fillCell()
      }
      case _ => // leave blank/clear
    }

    g.setColor(Color.BLACK)

    if (north) g.drawLine(x, y, x + size, y)
    if (south) g.drawLine(x, y + size, x + size, y + size)
    if (east) g.drawLine(x + size, y, x + size, y + size)
    if (west) g.drawLine(x, y, x, y + size)
  }

  def clear(dir : Direction) = dir match {
    case North => north = false
    case South => south = false
    case East => east = false
    case West => west = false
  }

  def getDirections(): List[Direction] = {
    var d = List[Direction]()

    if (!north) d = North :: d
    if (!south) d = South :: d
    if (!east) d = East :: d
    if (!west) d = West :: d

    d
  }

  def getRndDirections() : List[Direction] = {
    implicit val r = Rand.rand
    Utils.shuffle(allDirections)
  }

  def getColour(n : Int) = n match {
    case 0 => Color.RED
    case 1 => Color.BLUE
    case 2 => Color.CYAN
    case 3 => Color.YELLOW
    case 4 => Color.PINK
    case 5 => Color.ORANGE
    case 6 => Color.MAGENTA
    case _ => Color.GREEN
  }
}

// represent a 2d point
case class Point(x: Int, y: Int)

// init a random generator singleton
object Rand {
  val rand: Random = new Random()
}

object Utils {
  // scala 2.13+
  def shuffle[T](xs: List[T])(implicit r: Random) =
    xs.zip(LazyList.continually(r.nextDouble())).sortWith(_._2 < _._2).map(_._1)

  def permute[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => List(Nil)
    case _ =>
      xs.flatMap(x => permute(xs.filter(_ != x)).map(x :: _))
  }

  // make a stream of n, f(n), f(f(n)), etc.
  // in Haskell this is called "iterate".  it ought to be in the standard library
  // as Stream.iterate.  "unfold" should be more general, but nonetheless I'm
  // going to call this unfold for the moment...
  def unfold[T](x:T)(f:T=>T): LazyList[T] =
    LazyList.cons(x,unfold(f(x))(f))

  def iterate[T](x:T)(f:T=>T): LazyList[T] =
    LazyList.cons(x,iterate(f(x))(f))
}
