import edu.mit.csail.sdg.alloy4.A4Reporter
import edu.mit.csail.sdg.alloy4.ErrorWarning
import edu.mit.csail.sdg.ast.Command
import edu.mit.csail.sdg.parser.CompUtil
import edu.mit.csail.sdg.translator.A4Options
import edu.mit.csail.sdg.translator.A4Solution
import edu.mit.csail.sdg.translator.TranslateAlloyToKodkod
import scala.concurrent.duration._
import scala.io.Source
import java.nio.file.{ Files, Path, Paths }
import java.io.{ File, PrintWriter, PrintStream }
import sys.process._
import collection.JavaConverters._
import scopt.OParser
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
case class Config(
  outdir: File = new File ("/tmp/output"),
  alloyfile: File = new File ("."),
)
object Convert extends App {
  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("convert"),
      head("convert", "0.1"),
      arg[File]("<file>")
        .required ()
        .action((x, c) => c.copy(alloyfile = x))
        .text("Alloy program"),
    )
  }
  val config = OParser.parse(parser1, args, Config()) get
  val reporter: A4Reporter = new A4Reporter {
    // For example, here we choose to display each "warning" by printing it to System.out
    override def warning(msg: ErrorWarning) = println(s"Warning: $msg")
    override def solve(pvars: Int, tvars: Int, clauses: Int) = if (tvars == 0) { 
       println("zero vars")
       sys.exit(1)
     } else println(s"$tvars vars")
  }
  val dir = config.outdir.toPath
  val world = CompUtil.parseEverything_fromFile(reporter, null, config.alloyfile.toString)
  def get_starter_command () : Command = {
    world
      .getAllCommands ()
      .asScala
      .find (_.label.equals("features"))
      .get
  }
  val options = new A4Options ()
  val cmd = get_starter_command ()
  println (s"The command is: $cmd")
  run_command (cmd, Some (1.minute.fromNow))
  def run_command (cmd: Command, d: Option[Deadline] = None) = {
    println(s"Running $cmd")
    var ans = TranslateAlloyToKodkod.execute_command(reporter, world.getAllReachableSigs (), cmd, options)
    //println(ans)
    var hashes : Set[String] = Set()
    while (ans.satisfiable () && d.map (_.hasTimeLeft ()).getOrElse (true)) {
      val tmp_file = dir.resolve("tmp.xml")
      ans.writeXML(tmp_file.toString ())
      val md5out = (s"md5sum ${tmp_file.toString ()}" !!).split(" ")(0)
      if (!hashes.contains (md5out)) {
	println(ans)
        hashes = hashes + md5out
	} else {
        println(s"REPEAT: $ans")
	sys.exit(42)
      }
      ans = ans.next ()
    }
  }
}
// Local Variables:
