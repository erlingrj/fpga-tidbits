package fpgatidbits.Accelerators

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._


import fpgatidbits.Accelerators._




class TestSearchTask extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }

  behavior of "SearchTask"
  it should "Initialize correctly" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.ready.expect(true.B)
      c.io.resultOut.valid.expect(false.B)
    }
  }
  it should "Find highest value" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)


      fork {
        c.io.benefitIn.enqueueSeq(Seq(1.U, 3.U,5.U,4.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
          .bits.Lit(_.winner -> 2.U, _.bid -> 1.U)
        )
      }.join()
    }
  }

  it should "find highest value in stream" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)

      val inputStream =
        Seq(
          10.U, 0.U, 3.U, 4.U,
          2.U, 3.U, 10.U, 2.U,
          69.U, 10.U, 3.U, 42.U
        )
      fork {
        c.io.benefitIn.enqueueSeq(inputStream)
      }.fork {
        c.io.resultOut.expectDequeueSeq(Seq(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 6.U),
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 2.U, _.bid -> 7.U),
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 27.U)
        )
        )
      }.join()
    }
  }
  // TODO: Add test on negative input
  // TODO: Add test on tied best input
  // TODO: Add test on negative second best
  // TODO: Add test on all negative
}


class TestProcessingElement extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }

  behavior of "ProcessingElement"
  it should "Initialize correctly" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      // c.regState.expect(c.sIdle)
      // c.regReward.expect(0.U)
      // c.regPrice.expect(0.U)
      // c.regBenefit.expect(0.U)

      c.io.benefitOut.valid.expect(false.B)
      c.io.rewardIn.ready.expect(false.B)
      c.io.priceIn.ready.expect(false.B)
    }
  }
  it should "Calculate single benefit" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.priceIn.enqueue(4.U)
      }.fork {
        c.io.rewardIn.enqueue(8.U)
      }.fork {
        c.io.benefitOut.expectDequeue(4.U)
      }.join()
    }
  }

  it should "Handle negative benefit" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.priceIn.enqueue(8.U)
      }.fork {
        c.io.rewardIn.enqueue(4.U)
      }.fork {
        c.io.benefitOut.expectDequeue("hffff_fffc".U)
      }.join()
    }
  }


  it should "Handle a stream of input data" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      c.io.priceIn.bits.poke(100.U)
      c.io.priceIn.valid.poke(true.B)
      fork {
        c.io.rewardIn.enqueueSeq(Seq.tabulate(100)(i => (i+100).U))
      }.fork {
        c.io.benefitOut.expectDequeueSeq(Seq.tabulate(100)(i => (i.U)))
      }.join()
    }
  }

  it should "handle stalling on output interface" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      val price = 45
      c.io.priceIn.bits.poke(price.U)
      c.io.priceIn.valid.poke(true.B)
      val it = 100
      val rewardIn = Seq.tabulate(it)(i => (45+i))

      fork {
        c.io.rewardIn.enqueueSeq(rewardIn.map(_.U))
      }.fork {
        for (i <- 0 until it) {
          if (i%4 == 0) {
            c.clock.step(50)
          }
          c.io.benefitOut.expectDequeue((rewardIn(i)-price).U)
        }
      }.join()
    }
  }

  it should "Handle stalling on input interface" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      val it = 100
      val priceIn = 69
      val rewardIn = Seq.tabulate(it)(i => (i+69))
      c.io.priceIn.valid.poke(true.B)
      c.io.priceIn.bits.poke(priceIn.U)

     fork {
       for (i <- 0 until it) {
         if (i%4 == 0) {
           c.clock.step(50)
         }
         c.io.rewardIn.enqueue(rewardIn(i).U)
       }
      }.fork {
        c.io.benefitOut.expectDequeueSeq(rewardIn.map( a => (a - 69).U))
      }.join()
    }
  }
}


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

  it should "Pass a stream of data out correctly 2" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }.fork {
        c.peOut(0).expectDequeueSeq(Seq.tabulate(25)(idx => (idx*4).U))
      }.fork {
        c.peOut(1).expectDequeueSeq(Seq.tabulate(25)(idx => (1+(idx*4)).U))
      }.fork {
        c.peOut(2).expectDequeueSeq(Seq.tabulate(25)(idx => (2+(idx*4)).U))
      }.fork {
        c.peOut(3).expectDequeueSeq(Seq.tabulate(25)(idx => (3+(idx*4)).U))
      }.join()
    }
  }

  it should "Pass a stream of data out correctly with stalls" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }.fork {
        c.peOut(0).expectDequeueSeq(Seq.tabulate(25)(idx => (idx*4).U))
      }.fork {
        c.peOut(1).expectDequeueSeq(Seq.tabulate(25)(idx => (1+(idx*4)).U))
      }.fork {
        for (i <- 0 until 25) {
          if (i % 4 == 0) c.clock.step(100)
          c.peOut(2).expectDequeue((2+(i*4)).U)
        }
      }.fork {
        c.peOut(3).expectDequeueSeq(Seq.tabulate(25)(idx => (3+(idx*4)).U))
      }.join()
    }
  }
}