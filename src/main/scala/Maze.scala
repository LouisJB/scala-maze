/**
 * Scala 2.13+ Maze 0.1 21-08-2024 Louis Botterill
 */
package maze

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.Rectangle
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import scala.swing.BorderPanel
import scala.swing.Frame
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.event.KeyPressed
import scala.util.Random


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
  val frameHeight = 800 - frameBorder
  val frameWidth = frameHeight

  val delayTimeMs = 4000
  val createDelayMs = 50
  val solveDelayMs = 100
  val delayStopMs = 2000
}
import MazeModel._
import MazeConsts._
import Audio._
object BasicSound {
  def beep() = java.awt.Toolkit.getDefaultToolkit().beep()
}
case class Sounds(audioSynth: Option[AudioSynth]) {
  def beep() =
    audioSynth.foreach(_.sine(600, 25))
  def blip() =
    audioSynth.foreach(_.blip(200, 400, 20, 200))
  def sweepUp() =
    audioSynth.foreach(_.sweep(200, 2000, 5, 2000))
  def sweepDown() =
    audioSynth.foreach(_.sweep(3000, 300, -10, 2000))
  def blipSweep1() =
    audioSynth.foreach(_.blipSweep(500, 2500, 100, 100, 4, 2000))
  def blipSweep2() =
    audioSynth.foreach(_.blipSweep(500, 2500, 200, 100, 4, 5000))
  def silence(lenMs: Int) = // blocks for duration playing no audio to avoid audio stall
    audioSynth.foreach(_.silence(lenMs))
}

class MazeModel(sounds: Sounds) {
  import sounds._
  var cells = Array.tabulate(WIDTH, HEIGHT)((i, j) => Cell(i, j))

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
    def doNextCell(c: Cell): Unit = {
      c.visited = true
      c.trail = Forward

      c.getRndDirections().foreach { dir =>
        getCell(c, dir).foreach { n =>
          if (n.visited != true) {
            n.visited = true
            c.clear(dir)
            n.clear(getInv(dir))
            n.trail = Forward
            update
            Thread.sleep(createDelayMs)
            doNextCell(n)
            n.trail = Clear
            update
          }
        }
      }
    }
    doNextCell(startCell)
  }

  def solveMaze(update: => Unit) =
    solveMazeBFS(update)

  // find the maze solution using dfs from the start node until the end
  // node is located
  def solveMazeDFS(update: => Unit): Unit = {
    val start = new Point(0, 0)
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    val startCell = cells(0)(0)
    startCell.clear(North)
    try {
      def doNextCell(c: Cell): Unit = {
        c.visited = true
        c.trail = Forward

        c.getDirections().foreach { dir =>
          getCell(c, dir).foreach { n =>
            if (n.visited != true) {
              n.visited = true
              n.pi = Some(c)  // set predecessor node
              n.trail = Forward
              update
              beep()
              if (n.i == exit.x && n.j == exit.y) throw new Exception("Done")
              silence(solveDelayMs)
              doNextCell(n)
              n.trail = Backward
              update
              blip()
            }
          }
        }
        silence(solveDelayMs)
      }
      doNextCell(startCell)
    }
    catch {
      case e : Exception =>
    }
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
    try {
      while (!q.isEmpty) {
        val c = q.dequeue()
        c.gen = q.size

        if (c.visited != true) {
          c.visited = true
          c.trail = Forward

          update
          beep()

          if (c.i == exit.x && c.j == exit.y) throw new Exception("Done")

          c.getDirections().foreach { dir =>
            getCell(c, dir).foreach { n =>
              if (n.visited != true) {
                n.pi = Some(c)
                n.gen = c.gen
                q.enqueue(n)
                gen = gen + 1
              }
            }
          }

          silence(solveDelayMs) // add a little delay so we can watch the bfs explore and find the solution
        }
      }
    }
    catch {
      case e : Exception =>
    }
  }

  // from the exit recurse over the trail of predecessors,
  // marking with a forward trail
  def showSolution(update: => Unit): Unit = {
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    println(">>showSolution")

    // Stream.iterate(cells(exit.n)(exit.y))(_.pi).takeWhile(_ != null)
    val cell: Option[Cell] = Some(cells(exit.x)(exit.y))
    LazyList.iterate(cell)(_.flatMap(_.pi)).takeWhile(_ != None).foreach {
      case Some(n) =>
        n.trail = Forward
        update
      case _ =>
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

  def mapCells(f: Cell => Cell) = 
    cells.foreach(_.foreach(f))

  // set everything as not visited and with no trail
  def clearVisited() = mapCells { cell =>
    cell.visited = false
    cell.trail = Clear
    cell
  }
  def clearAll() = mapCells { cell =>
    cell.clearAll()
    cell
  }
}

// a scala.swing.Panel, override paint(Graphics2D) to paint each of the cells
class MazePanel(mm: MazeModel) extends Panel {
  override def paintComponent(g: java.awt.Graphics2D) = {
    super.paintComponent(g)
    mm.mapCells(c => {c.draw(g); c })
  }
}

class Maze(noSound: Boolean) {
  import MazeConsts._
  import Audio._
  import AudioConsts._

  val sounds = {
    val maybeAudioSynth = if (noSound) None else
      Some(AudioSynth.mkAudioSynth(defaultSampleRate, defaultBitDepth))
    Sounds(maybeAudioSynth)
  }
  import sounds._
  val mazeModel = new MazeModel(sounds)
  lazy val mazePanel = new MazePanel(mazeModel)

  def update =
    mazePanel.repaint()

  def run(): Unit = {
    println("Starting...")
    while (true) {
      blip()
      mazeModel.clearAll()
      println("Generating Maze")
      mazeModel.generateMaze(update)
      beep()
      silence(delayTimeMs)

      mazeModel.clearVisited()

      println("Solving Maze using DFS")
      mazeModel.solveMazeDFS(update)

      blipSweep1()
      silence(delayTimeMs)

      mazeModel.clearVisited()
      mazeModel.showSolution(update)
      update

      sweepUp()
      silence(delayTimeMs)

      mazeModel.clearVisited()

      println("Solving Maze using BFS")
      mazeModel.solveMazeBFS(update)

      blipSweep1()
      silence(delayTimeMs)

      mazeModel.clearVisited()
      mazeModel.showSolution(update)
      update

      sweepDown()
      silence(delayTimeMs)

      mazeModel.clearVisited()
      update

      blipSweep2()
      silence(delayTimeMs)
    }
    println("Ending...")
    audioSynth.foreach(_.stop())
  }
}

// standard Java app of the maze using JFrame
object Maze {
  def main(args: Array[String]): Unit = {
    val noSound = if (args.length > 0)
	    args(0).trim.toLowerCase == "nosound"
    else false
    val sizeDims = new java.awt.Dimension(frameWidth + frameBorder, frameHeight + frameBorder)
    val maze = new Maze(noSound)
    val frame = new MainFrame()
    frame.preferredSize = new java.awt.Dimension(sizeDims)
    frame.bounds = new Rectangle(0, 0, sizeDims.width, sizeDims.height)
    frame.title = "Simple Maze Demo v0.1"
    maze.mazePanel.background_= = new Color(124, 255, 64)
    frame.contents = new BorderPanel { add(maze.mazePanel, BorderPanel.Position.Center) }
    frame.peer.addKeyListener(mkKeyListener(frame))
    frame.pack().centerOnScreen()
    frame.visible = true
    maze.run()
    frame.dispose()
  }

  // old way to do it but did not scala-swing has a wrapper for it yet
  private def mkKeyListener(frame: Frame) = new KeyListener() {
    @Override
    def keyPressed(args: KeyEvent): Unit = {
      val key = args.getKeyCode()
      // hand X and Q as exit/quit
      if ((key == KeyEvent.VK_X) || (key == KeyEvent.VK_Q)) {
        frame.dispose()
        System.exit(0)
      }
    }
    @Override
    def keyReleased(arg: KeyEvent): Unit = {}
    @Override
    def keyTyped(arg: KeyEvent): Unit = {}
  }
}

object Cell {
  val cellBorder = 10
  val origin = Cell(0, 0)
}

// represent a Cell of the maze
case class Cell(i: Int, j: Int) {
  import Cell._
  import scala.collection.mutable.Set
  private val dirs: Set[Direction] = Set.empty ++= allDirections
  private def north = dirs.contains(North)
  private def south = dirs.contains(South)
  private def east = dirs.contains(East)
  private def west = dirs.contains(West)

  var visited: Boolean = false
  var pi: Option[Cell] = None  // predecessor cell
  var trail: Breadcrumb = Clear
  var gen: Int = 0

  def draw(g: Graphics): Unit = {
    val bounds = g.getClipBounds()
    val size = Math.min(bounds.width / WIDTH, bounds.height / HEIGHT)
    val x = bounds.x + i * size
    val y = bounds.y + j * size

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

  def clear(dir : Direction) = dirs -= dir
  def clearAll() = {
    dirs ++= allDirections
    visited = false
    trail = Clear
    gen = 0
    pi = None
  }

  def getDirections(): List[Direction] =
    (allDirections.toSet -- dirs).toList

  def getRndDirections() : List[Direction] = {
    implicit val r = Rand.rand
    Utils.shuffle(allDirections)
  }

  def getColour(n : Int) = n match {
    case 0 => Color.RED
    case 1 => Color.ORANGE
    case 2 => Color.PINK
    case 3 => Color.YELLOW
    case 4 => Color.GREEN.darker()
    case 5 => Color.CYAN
    case 6 => Color.BLUE
    case _ => Color.MAGENTA
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
