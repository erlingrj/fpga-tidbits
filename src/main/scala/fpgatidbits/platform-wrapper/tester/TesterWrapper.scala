package fpgatidbits.PlatformWrapper


import chisel3._
import chisel3.util._
//import chisel3.iotesters._
import fpgatidbits.axi._
import fpgatidbits.dma._
import fpgatidbits.regfile._
import java.nio.file.{Files, Paths}
import java.nio.ByteBuffer
import java.io.FileOutputStream
import chiseltest._

import fpgatidbits.MainObj.{fileCopy, fileCopyBulk}
import fpgatidbits.TidbitsMakeUtils._

// testing infrastructure for GenericAccelerator
// providing something like a virtual platform that can be used for testing the
// accelerator in Chisel simulation. provides "main memory" simulation and a
// convenient way of setting up the control/status registers for setting up
// the accelerator.

object TesterWrapperParams extends PlatformWrapperParams {

  val driverTargetDir = "test"
  val platformName = "Tester"
  val memAddrBits = 48
  val memDataBits = 64
  val memIDBits = 32
  val memMetaBits = 1
  val numMemPorts = 0 // not really, just taken from the accelerator
  val sameIDInOrder = true
  val typicalMemLatencyCycles = 16
  val burstBeats = 8
  val coherentMem = false
}


class TesterMemoryWrapper(p: PlatformWrapperParams, val numMemPorts: Int) extends MultiIOModule {

  val mrp = p.toMemReqParams()

  val accio = IO(new Bundle {
    val memPort = Vec(numMemPorts, new GenericMemorySlavePort(mrp))
  })

  //  accio.memPort.map { m =>
  //      m.memWrDat.ready := false.B
  //      m.memWrReq.ready := false.B
  //      m.memWrRsp.valid := false.B
  //      m.memWrRsp.bits := DontCare
  //      m.memRdReq.ready := false.B
  //      m.memRdRsp.valid := false.B
  //      m.memRdRsp.bits := DontCare
  //    }
  //
  //
  //
  val verio = IO(new Bundle {
    val memAddr = Input(UInt(p.memAddrBits.W))
    val memWriteEn = Input(Bool())
    val memWriteData = Input(UInt(p.memDataBits.W))
    val memReadData = Output(UInt(p.memDataBits.W))
  })

  val memWords = 64 * 1024 * 1024
  val memAddrBits = log2Ceil(memWords)
  val memUnitBytes = (p.memDataBits/8).U
  val mem = SyncReadMem(memWords, UInt(p.memDataBits.W))

  // testbench memory access
  // Typically 64 bits data words in memory. The address is shifted log2(nBytes)
  //  Which is normally 3 bits. This must be to "simulate" byte-addressing?
  //  So memAddr is byte-addresses? So 0->8 means accessing first word. And then do the filtering in the StreamReader?

  def addrToWord(x: UInt) = {x >> (log2Ceil(p.memDataBits/8))}
  val memWord = addrToWord(verio.memAddr)
  verio.memReadData := mem.read(memWord)

  when (verio.memWriteEn) {mem.write(memWord, verio.memWriteData)}

  def addLatency[T <: Data](n: Int, prod: DecoupledIO[T]): DecoupledIO[T] = {
    if(n == 1) {
      return Queue(prod, 2)
    } else {
      return addLatency(n-1, Queue(prod, 2))
    }
  }

  // accelerator memory access ports
  // one FSM per port, rather simple, but supports bursts
  for(i <- 0 until numMemPorts) {
    // reads
    val sWaitRd :: sRead :: Nil = Enum(2)
    val regStateRead = RegInit(sWaitRd)
    val s1_regReadRequest = Reg(Valid(new GenericMemoryRequest(mrp)))
    val s2_regReadRequest = Reg(Valid(new GenericMemoryRequest(mrp)))
    val s3_regMemRead = Reg(Valid(new GenericMemoryResponse(mrp)))

    val s1_wireMemAddr = WireInit(0.U(p.memDataBits.W))
    val regTransactionsLeft = RegInit(0.U(32.W))

    val accmp = accio.memPort(i)
    val accRdReq = addLatency(15, accmp.memRdReq)
    val accRdRsp = accmp.memRdRsp
    val memRead = WireInit(mem(addrToWord(s1_wireMemAddr)))

    accRdRsp.bits := s3_regMemRead.bits
    accRdRsp.valid := s3_regMemRead.valid
    accRdReq.ready := false.B

    switch(regStateRead) {
      is(sWaitRd) {
        accRdReq.ready := true.B
        when (accRdReq.valid) {
          s1_regReadRequest.bits := accRdReq.bits
          s1_regReadRequest.valid := true.B
          regStateRead := sRead
          regTransactionsLeft := 2.U
          s2_regReadRequest.valid := false.B
          s3_regMemRead.valid := false.B
        }
      }

      is(sRead) {
        when (accRdRsp.ready) {
          // s1
          when(s1_regReadRequest.bits.numBytes > memUnitBytes) {
            s1_regReadRequest.bits.numBytes := s1_regReadRequest.bits.numBytes - memUnitBytes
            s1_regReadRequest.bits.addr := s1_regReadRequest.bits.addr + memUnitBytes
            s1_regReadRequest.valid := true.B
          }.otherwise {
            when (regTransactionsLeft === 0.U) {
              regStateRead := sWaitRd
            }.otherwise {
              regTransactionsLeft := regTransactionsLeft - 1.U
              s1_regReadRequest.valid := false.B
            }
          }
          // Mem read
          s1_wireMemAddr := s1_regReadRequest.bits.addr

          // s2
          s2_regReadRequest := s1_regReadRequest

          // s3
          s3_regMemRead.valid := s2_regReadRequest.valid
          s3_regMemRead.bits.metaData := s2_regReadRequest.bits.metaData
          s3_regMemRead.bits.channelID := s2_regReadRequest.bits.channelID
          s3_regMemRead.bits.isWrite := false.B
          s3_regMemRead.bits.isLast := s2_regReadRequest.bits.numBytes === memUnitBytes
          s3_regMemRead.bits.readData := memRead

        }.otherwise {
          // If stall
          s1_wireMemAddr := s2_regReadRequest.bits.addr
        }
      }
    }

    // writes
    val sWaitWr :: sWrite :: Nil = Enum(2)
    val regStateWrite = RegInit(sWaitWr)
    val regWriteRequest = RegInit(GenericMemoryRequest(mrp))
    // write data queue to avoid deadlocks (state machine expects rspQ and data
    // available simultaneously)
    val wrDatQ = Module(new Queue(UInt(p.memDataBits.W), 16)).io
    wrDatQ.enq <> accmp.memWrDat

    // queue on write response port (to avoid combinational loops)
    val wrRspQ = Module(new Queue(new GenericMemoryResponse(mrp), 16)).io
    wrRspQ.deq <> accmp.memWrRsp

    val accWrReq = addLatency(10, accmp.memWrReq)

    accWrReq.ready := false.B
    wrDatQ.deq.ready := false.B
    wrRspQ.enq.valid := false.B
    wrRspQ.enq.bits.driveDefaults()
    wrRspQ.enq.bits.channelID := regWriteRequest.channelID

    switch(regStateWrite) {
      is(sWaitWr) {
        accWrReq.ready := true.B
        when(accWrReq.valid) {
          regWriteRequest := accWrReq.bits
          regStateWrite := sWrite
        }
      }

      is(sWrite) {
        when(regWriteRequest.numBytes === 0.U) {regStateWrite := sWaitWr}
          .otherwise {
            when(wrRspQ.enq.ready && wrDatQ.deq.valid) {
              when(regWriteRequest.numBytes === memUnitBytes) {
                wrRspQ.enq.valid := true.B
              }
              wrDatQ.deq.ready := true.B
              mem(addrToWord(regWriteRequest.addr)) := wrDatQ.deq.bits
              regWriteRequest.numBytes := regWriteRequest.numBytes - memUnitBytes
              regWriteRequest.addr := regWriteRequest.addr + (memUnitBytes)
            }
          }
      }
    }
  }
}



class TesterWrapper(instFxn: PlatformWrapperParams => GenericAccelerator, targetDir: String)
  extends PlatformWrapper(TesterWrapperParams, instFxn) {
  override def desiredName  = "TesterWrapper"


  val platformDriverFiles = baseDriverFiles ++ Array[String](
    "platform-tester.cpp", "testerdriver.hpp"
  )

  val memWords = 64 * 1024 * 1024
  val mrp = p.toMemReqParams()
  val memAddrBits = log2Ceil(memWords)
  val memUnitBytes = (p.memDataBits/8).U
  val io = IO(new Bundle {
    // register file access
    val regFileIF = new RegFileSlaveIF(regAddrBits, p.csrDataBits)
    // memory access for the testbench
    val memAddr = Input(UInt(p.memAddrBits.W))
    val memWriteEn = Input(Bool())
    val memWriteData = Input(UInt(p.memDataBits.W))
    val memReadData = Output(UInt(p.memDataBits.W))
  })
  val accio = accel.io

  // expose regfile interface for testbench
  io.regFileIF <> regFile.extIF

  val memory = Module(new TesterMemoryWrapper(p, accel.numMemPorts))
  memory.verio.memAddr := io.memAddr
  memory.verio.memWriteEn := io.memWriteEn
  memory.verio.memWriteData := io.memWriteData
  io.memReadData := memory.verio.memReadData


  for (i <- 0 until accel.numMemPorts) {
    accio.memPort(i).memRdRsp <> memory.accio.memPort(i).memRdRsp
    accio.memPort(i).memRdReq <> memory.accio.memPort(i).memRdReq
    accio.memPort(i).memWrDat <> memory.accio.memPort(i).memWrDat
    accio.memPort(i).memWrReq <> memory.accio.memPort(i).memWrReq
    accio.memPort(i).memWrRsp <> memory.accio.memPort(i).memWrRsp
  }

}

// Erlingrj: Experimental, use implicits to get read/write to CSR
//  and mem functionality into TesterWrapper
object GenericAccelImplicits {
  implicit class GenericAccelTesterDriver(c: TesterWrapper) {
    val regFile = c.io.regFileIF
    def nameToRegIdx(str: String): Int  = {
      c.regFileMap(str).last
    }

    def writeReg(regName: String, value: UInt) = {
      val idx = nameToRegIdx(regName)
      regFile.cmd.bits.regID.poke(idx.U)
      regFile.cmd.bits.read.poke(false.B)
      regFile.cmd.bits.write.poke(true.B)
      regFile.cmd.bits.writeData.poke(value)
      regFile.cmd.valid.poke(true.B)
      c.clock.step()
      regFile.cmd.valid.poke(false.B)
      c.clock.step(5) // allow the command to propagate and take effect
    }

    def readReg(regName: String): UInt = {
      val idx = nameToRegIdx(regName)
      regFile.cmd.bits.regID.poke(idx.U)
      regFile.cmd.bits.read.poke(true.B)
      regFile.cmd.bits.write.poke(false.B)
      regFile.cmd.valid.poke(true.B)
      c.clock.step()
      regFile.cmd.valid.poke(false.B)
      regFile.readData.bits.peek
    }

    def expectReg(regName: String, value: UInt): Unit = {
      val idx = nameToRegIdx(regName)
      regFile.cmd.bits.regID.poke(idx.U)
      regFile.cmd.bits.read.poke(true.B)
      regFile.cmd.bits.write.poke(false.B)
      regFile.cmd.valid.poke(true.B)
      c.clock.step()
      regFile.cmd.valid.poke(false.B)
      regFile.readData.bits.expect(value)
    }

    def writeMem(addr: BigInt, value: UInt): Unit = {
      c.io.memAddr.poke(addr.U)
      c.io.memWriteData.poke(value)
      c.io.memWriteEn.poke(true.B)
      c.clock.step()
      c.io.memWriteEn.poke(false.B)
    }

    def readMem(addr: BigInt): UInt = {
      c.io.memAddr.poke(addr.U)
      c.io.memWriteEn.poke(false.B)
      c.clock.step()
      c.io.memReadData
    }

    def expectMem(addr: BigInt, value: UInt) = {
      c.io.memAddr.poke(addr.U)
      c.io.memWriteEn.poke(false.B)
      c.clock.step()
      c.io.memReadData.expect(value)
    }

    def arrayToMem(startAddr: BigInt, arr: Seq[UInt]): Unit = {
      var addr = startAddr
      for (value <- arr) {
        writeMem(addr, value)
        addr = addr + 8 //TODO: Support variable memory data width
      }
    }


  }
}




//class GenericAccelTester(c: TesterWrapper) {
//  // TODO add functions for initializing memory
//  val memUnitBytes = c.memUnitBytes.litValue()
//  val regFile = c.io.regFileIF
//  def nameToRegInd(regName: String): Int = {
//    return c.regFileMap(regName)(0).toInt
//  }
//  type HookFxn = () => Unit
//  var hooks = scala.collection.mutable.Map[String, HookFxn]()
//
//  override def step(n: Int) = {
//    for((n,f) <- hooks) {f()}
//    c.clock.step(n)
//  }
//
//  def printAllRegs() = {
//    val ks = c.regFileMap.keys
//    var regVals = scala.collection.mutable.Map[String, BigInt]()
//    for(k <- ks) {
//      regVals(k) = readReg(k)
//    }
//    for(k <- ks) {
//      println(k + " : " + regVals(k).toString)
//    }
//  }
//
//  def readReg(regName: String): BigInt = {
//    val ind = nameToRegInd(regName)
//    poke(regFile.cmd.bits.regID, ind)
//    poke(regFile.cmd.bits.read, 1)
//    poke(regFile.cmd.bits.write, 0)
//    poke(regFile.cmd.bits.writeData, 0)
//    poke(regFile.cmd.valid, 1)
//    step(1)
//    poke(regFile.cmd.valid, 0)
//    return peek(regFile.readData.bits)
//  }
//
//  def expectReg(regName: String, value: BigInt): Boolean = {
//    return expect(readReg(regName)==value, regName)
//  }
//
//  def writeReg(regName: String, value: BigInt) = {
//    val ind = nameToRegInd(regName)
//    poke(regFile.cmd.bits.regID, ind)
//    poke(regFile.cmd.bits.read, 0)
//    poke(regFile.cmd.bits.write, 1)
//    poke(regFile.cmd.bits.writeData, value)
//    poke(regFile.cmd.valid, 1)
//    step(1)
//    poke(regFile.cmd.valid, 0)
//    step(5) // allow the command to propagate and take effect
//  }
//
//  def readMem(addr: BigInt): BigInt = {
//    poke(c.io.memAddr, addr)
//    return peek(c.io.memReadData)
//  }
//
//  def expectMem(addr: BigInt, value: BigInt): Boolean = {
//    return expect(readMem(addr) == value, "Mem: "+addr.toString)
//  }
//
//  def writeMem(addr: BigInt, value: BigInt) = {
//    poke(c.io.memAddr, addr)
//    poke(c.io.memWriteEn, 1)
//    poke(c.io.memWriteData, value)
//    step(1)
//    poke(c.io.memWriteEn, 0)
//  }
//
//  // read file and write into memory, starting at <baseAddr>
//  def fileToMem(fileName: String, baseAddr: BigInt) = {
//    var buf = Files.readAllBytes(Paths.get(fileName))
//    println("Loading "+fileName+" to baseAddr "+baseAddr.toString)
//    arrayToMem(buf, baseAddr)
//  }
//
//  def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
//
//  // TODO not sure if this is the correct way to handle endianness --
//  // expect problems:
//  // every <memory-width> byte group is reversed while being written
//  def arrayToMem(buf: Array[Byte], baseAddr: BigInt) = {
//    if(baseAddr % memUnitBytes != 0) {
//      println("fileToMem: base addr must be multiple of mem unit width")
//      System.exit(-1)
//    }
//    var i: Int = 0
//    for(b <- buf.grouped(c.p.memDataBits/8)) {
//      val w : BigInt = new BigInt(new java.math.BigInteger(b.reverse))
//
//      //println("Read: " + valueOf(w.toByteArray))
//      //println("Read: " +i.toString+ "=" + valueOf(b))
//      writeMem(baseAddr+i*memUnitBytes, w)
//      i += 1
//    }
//  }
//
//  def memToFile(fileName: String, baseAddr: BigInt, wordCount: Int) = {
//    val fout = new FileOutputStream(fileName)
//    for(i <- 0 until wordCount) {
//      var ba = readMem(baseAddr+i*memUnitBytes).toByteArray
//      // the BigInt.toByteArray occasionally returns too many bytes,
//      // not sure why
//      if (ba.size > memUnitBytes) { ba = ba.takeRight(memUnitBytes.toInt) }
//      // BigInt.toByteArray returns the min # of bytes needed, pad to
//      // cover all bytes read from memory by adding zeroes
//      while(ba.size < memUnitBytes) {
//        ba = ba ++ Array[Byte](0)
//      }
//      ba = ba.reverse
//      fout.write(ba)
//    }
//    fout.close()
//  }
//
//  // let the accelerator do internal init (such as writing to the regfile)
//  step(10)
//  // TODO launch the default test, as defined by the accelerator
//}

class VerilatedTesterWrapper(instFxn: PlatformWrapperParams => GenericAccelerator, targetDir: String)
  extends TesterWrapper(instFxn, targetDir) {
  override val platformDriverFiles = baseDriverFiles ++ Array[String](
    "platform-verilatedtester.cpp", "verilatedtesterdriver.hpp"
  )

  // Generate the RegFile driver
  generateRegDriver(targetDir)



  // Copy over the other needed files
  val verilogBlackBoxFiles = Seq("Q_srl.v", "DualPortBRAM.v")
  val scriptFiles = Seq("verilator-build.sh")
  val driverFiles = Seq("wrapperregdriver.h", "platform-verilatedtester.cpp",
    "platform.h", "verilatedtesterdriver.hpp")

  val resRoot = Paths.get("./fpga-tidbits/src/main/resources").toAbsolutePath
  // copy blackbox verilog, scripts, driver and SW support files
  fileCopyBulk(s"$resRoot/verilog/", targetDir, verilogBlackBoxFiles)
  fileCopyBulk(s"$resRoot/script/", targetDir, scriptFiles)
  fileCopyBulk(s"$resRoot/cpp/platform-wrapper-regdriver/", targetDir,
    driverFiles)
}

