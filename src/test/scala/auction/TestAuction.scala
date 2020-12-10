package fpgatidbits.Accelerators

import org.scalatest._
import chiseltest._
import chisel3._

import fpgatidbits.Accelerators._


class TestDataDistributor extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }

  behavior of "DataDistributor"

  it should "Initialize read/valid interfaces correctly" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.peOut.map(_.valid.expect(false.B))
      c.mem.valid.poke(false.B)

      c.peOut.map(_.ready.poke(false.B))
      c.mem.ready.expect(false.B)
    }
  }

  it should "Pass simple data through" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.peOut(0).ready.poke(true.B)
      c.mem.valid.poke(true.B)
      c.mem.bits.poke(69.U)

      c.peOut(0).valid.expect(true.B)
      c.peOut(0).bits.expect(69.U)
    }
  }

  it should "Pass a stream of data out correctly" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }

      for (i <- 0 until 100) {
        println(s"i=$i cnt=${c.cnt.peek}")
        c.peOut.zipWithIndex.map({ case (io, idx) =>
          if (idx == i%4) {
            io.expectDequeueNow((i.U))
          }
        })
      }
    }
  }

  it should "Pass a stream of data out correctly" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }

      for (i <- 0 until 100) {
        println(s"i=$i cnt=${c.cnt.peek}")
        c.peOut.zipWithIndex.map({ case (io, idx) =>
          if (idx == i%4) {
            io.expectDequeueNow((i.U))
          }
        })
      }
    }
  }


}