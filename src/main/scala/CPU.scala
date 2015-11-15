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

  // Includes: icache, BTB, predictor, decoder, renamer, tlb

}

class RFEntry extends Bundle {
  val valid = Bool()
  val data = UInt(width = 32)
}

class IQEntry extends Bundle {
  val busy = Bool()
  val func = UInt(width = 4)
  val rd = UInt(width = 6)
  val rs1 = UInt(width = 6)
  val rs2 = UInt(width = 6)
  val ready1 = Bool()
  val ready2 = Bool()
}

class ReadReq extends Bundle {
  val func = UInt(width = 4)
  val rd = UInt(width = 6)
  val rs1 = UInt(width = 6)
  val rs2 = UInt(width = 6)
}

class ALUReq extends Bundle {
  val func = UInt(width = 4)
  val rd = UInt(width = 6)
  val rv1 = UInt(width = 32)
  val rv2 = UInt(width = 32)
}

package object Implicits {

  implicit def toReadReq(iqe: IQEntry): ReadReq = {
    val rq = new ReadReq
    rq.func := iqe.func
    rq.rd  := iqe.rd
    rq.rs1 := iqe.rs1
    rq.rs2 := iqe.rs2
    rq
  }

  implicit class RichVec[T <: Data](val self: Vec[T]) extends AnyVal {
    def select(p: T => Bool) = PriorityMux(self map { e => p(e) -> e })
  }

}

import Implicits._

class ALU extends Module

class Engine extends Module {

  val io = new Bundle {
    val front = new FrontEngineIO().flip
    val back = new BackEngineIO().flip
  }

  // Includes: centralized RS, functional units

  val NR_IQ = 20
  val NR_RF = 64

  val alu0 = Module(new ALU)

  // 1. Issue Queue

  val iq = Reg(Vec(NR_IQ, new IQEntry))
  val rq0 = Reg(new ReadReq)

  for (i <- 0 until NR_IQ) {
    when (iq(i).busy) {
      when (alu0.io.resp.valid && alu0.io.resp.rd === iq(i).rs1) {
        iq(i).ready1 := Bool(true)
      }
      when (alu0.io.resp.valid && alu0.io.resp.rd === iq(i).rs2) {
        iq(i).ready2 := Bool(true)
      }
    }
  }

  rq0 := iq select { e => e.busy && e.ready1 && e.ready2 }        // Moore style logic; no direct flow from instruction retirement


  // 2. Register Read

  val rf = Reg(Vec(NR_RF, new RFEntry))
  val freq0 = Reg(new ALUReq)

  freq0.func := rq0.func
  freq0.rd := rq0.rd
  freq0.rv1 := rf(rq0.rs1)
  freq0.rv2 := rf(rq0.rs2)


  // 3. Functional Units

  alu0.io.req := freq0

}

class Backend extends Module {

  val io = new Bundle {
    val exec = new BackEngineIO
    val front = new FrontBackIO().flip
    val mem = new BackMemoryIO
  }

  // Includes: ROB, store buffer, dcache, tlb
 
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
