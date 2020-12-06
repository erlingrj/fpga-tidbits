package fpgatidbits.Accelerators

import org.scalatest._
import chiseltest._
import chisel3._

import fpgatidbits.Accelerators._


class TestAuction extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }
  behavior of "AuctionController"
  it should "do something" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      println("Hello ChiselTest!")
    }
  }



  behavior of "SearchTask"
  it should "do something" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      println("Hello ChiselTest!")
    }
  }

  behavior of "DataDistributor"
  it should "do something" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      println("Hello ChiselTest!")
    }
  }

  behavior of "ProcessingElement"
  it should "do something" in {
    test(new ProcessingElement(AuctionTestParams)) { c =>
      println("Hello ChiselTest!")
    }
  }
}