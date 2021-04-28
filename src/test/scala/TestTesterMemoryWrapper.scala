package PlatformWrapper

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.stage.PrintFullStackTraceAnnotation
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma.GenericMemorySlavePort


class TestTesterMemoryWrapper extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation, PrintFullStackTraceAnnotation)
    def initClocks(c: TesterMemoryWrapper ): Unit = {
        c.accio.memPort(0).memRdRsp.initSink().setSinkClock(c.clock)
        c.accio.memPort(0).memRdReq.initSource().setSourceClock(c.clock)
        c.accio.memPort(0).memWrRsp.initSink().setSinkClock(c.clock)
        c.accio.memPort(0).memWrReq.initSource().setSourceClock(c.clock)
        c.accio.memPort(0).memWrDat.initSource().setSourceClock(c.clock)
    }

  def writeMem(c: TesterMemoryWrapper, addr: Int, numBytes: Int, data: Seq[Int]) = {

    fork {

      c.accio.memPort(0).memWrReq.enqueue(chiselTypeOf(c.accio.memPort(0).memWrReq).bits.Lit(
        _.channelID -> 0.U,
        _.metaData -> 0.U,
        _.numBytes -> numBytes.U,
        _.addr -> addr.U,
        _.isWrite -> true.B,
      ))
      c.accio.memPort(0).memWrDat.enqueueSeq(data.map(_.U))
    }.fork {

      c.accio.memPort(0).memWrRsp.expectDequeue(
        chiselTypeOf(c.accio.memPort(0).memWrRsp).bits.Lit(
          _.readData-> 0.U,
          _.metaData-> 0.U,
          _.channelID -> 0.U,
          _.isWrite -> false.B,
          _.isLast -> false.B
        )
      )
    }.join()

  }

  def expectRead(c: TesterMemoryWrapper, addr: Int, numBytes: Int, data: Seq[Int]) = {
    c.accio.memPort(0).memRdReq.enqueue(chiselTypeOf(c.accio.memPort(0).memRdReq).bits.Lit(
      _.addr -> addr.U,
      _.metaData -> 0.U,
      _.channelID -> 0.U,
      _.numBytes -> numBytes.U,
      _.isWrite -> false.B
    ))
    c.accio.memPort(0).memRdRsp.expectDequeueSeq(data.zipWithIndex.map(d => {
      chiselTypeOf(c.accio.memPort(0).memRdRsp).bits.Lit(
        _.readData -> d._1.U,
        _.isLast ->  (d._2 == data.size-1).B,
        _.channelID -> 0.U,
        _.isWrite -> false.B,
        _.metaData -> 0.U
      )}))
  }

  behavior of "TesterMemoryWrapper"

  it should "Initialize correctly" in {
    test(new TesterMemoryWrapper(TesterWrapperParams, numMemPorts = 1)) { c =>
      c.accio.memPort(0).memRdRsp.valid.expect(false.B)
    }
  }


  it should "support simple mem read write" in {

    test(new TesterMemoryWrapper(TesterWrapperParams, numMemPorts = 1)) { c =>
      initClocks(c)

      val data = Seq.tabulate(8)(i => i)
      writeMem(c, addr = 0, numBytes = 64, data = data)
      println("data written")
      expectRead(c, addr = 0, numBytes = 64, data = data)

    }
  }

  it should "support reads with intermediate blocking" in {

    test(new TesterMemoryWrapper(TesterWrapperParams, numMemPorts = 1)) { c =>
      initClocks(c)

      val data = Seq.tabulate(32)(i => i)
      for (i <- 0 until 4) {
        val d  = Seq.tabulate(8)(j =>i*8 + j)
        writeMem(c, addr = i*64, numBytes = 64, data = d)
      }
      for (i <- 0 until 32) {
        if (i < 16) {
          expectRead(c, addr = i*8, numBytes = 8, data = Seq(data(i)))
        } else {
          if (i % 2 == 0) {
            c.clock.step(2)
          }
          expectRead(c, addr = i*8, numBytes = 8, data = Seq(data(i)))
        }
      }
    }
  }
  it should "work with stalling" in {

    test(new TesterMemoryWrapper(TesterWrapperParams, numMemPorts = 1)) { c =>
      initClocks(c)

      val data = Seq.tabulate(8)(i => i)
      writeMem(c, addr = 0, numBytes = 64, data = data)
      c.accio.memPort(0).memRdReq.enqueue(chiselTypeOf(c.accio.memPort(0).memRdReq).bits.Lit(
        _.addr -> 0.U,
        _.metaData -> 0.U,
        _.channelID -> 0.U,
        _.numBytes -> 64.U,
        _.isWrite -> false.B
      )
      )
      val dsplit = data.splitAt(4)



      c.accio.memPort(0).memRdRsp.expectDequeueSeq(dsplit._1.zipWithIndex.map(d => {
        chiselTypeOf(c.accio.memPort(0).memRdRsp).bits.Lit(
          _.readData -> d._1.U,
          _.isLast ->  (d._2 == data.size-1).B,
          _.channelID -> 0.U,
          _.isWrite -> false.B,
          _.metaData -> 0.U
        )}))

      c.clock.step(4)

      c.accio.memPort(0).memRdRsp.expectDequeueSeq(dsplit._2.zipWithIndex.map(d => {
        chiselTypeOf(c.accio.memPort(0).memRdRsp).bits.Lit(
          _.readData -> d._1.U,
          _.isLast ->  (d._2 == data.size-1).B,
          _.channelID -> 0.U,
          _.isWrite -> false.B,
          _.metaData -> 0.U
        )}))

      }
    }
}
