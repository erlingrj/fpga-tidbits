package fpgatidbits.Testbenches

import chisel3._
import chisel3.util._
import org.scalatest._
import chiseltest._

import fpgatidbits.PlatformWrapper.TesterWrapper

class TestGatherTest extends FlatSpec with ChiselScalatestTester with Matchers {

  val accel = {p => new TestGather(p)}
  behavior of "TestGather"
  it should "pass" in {
    test(new TesterWrapper(accel, ".dump")) {
      dut =>
        println("Hello")
    }
  }
}
