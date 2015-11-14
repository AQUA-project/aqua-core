import Chisel._

class FrontEngineIO extends Bundle
class FrontBackIO extends Bundle
class FrontMemoryIO extends Bundle
class BackEngineIO extends Bundle
class BackMemoryIO extends Bundle
class CoreMemoryIO extends Bundle
class AXIMasterIO extends Bundle

class Frontend extends Module {

  val io = new Bundle {
    val exec = new FrontEngineIO
    val back = new FrontBackIO
    val mem = new FrontMemoryIO
  }

}

class Engine extends Module {

  val io = new Bundle {
    val front = new FrontEngineIO().flip
    val back = new BackEngineIO().flip
  }

}

class Backend extends Module {

  val io = new Bundle {
    val exec = new BackEngineIO
    val front = new FrontBackIO().flip
    val mem = new BackMemoryIO
  }
 
}

class Core extends Module {

  val io = new Bundle {
    val mem = new CoreMemoryIO
  }

  val front = Module(new Frontend)
  val engine = Module(new Engine)
  val back = Module(new Backend)

  engine.io.front <> front.io.exec
  engine.io.back <> back.io.exec

  front.io.back <> back.io.front
}

class CPU extends Module {

  val io = new Bundle {
    val mem = new AXIMasterIO()
  }

  val core1 = Module(new Core)
  val core2 = Module(new Core)

}

class CPUTests(c: CPU) extends Tester(c) {
  step(1)
}

object CPU {
  def main(args: Array[String]) {
    chiselMainTest(args, () => Module(new CPU)) { c =>
      new CPUTests(c)
    }
  }
}
