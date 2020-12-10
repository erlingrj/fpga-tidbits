package fpgatidbits.Accelerators

import chisel3._
import chisel3.util._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.streams._

trait AuctionParams {
  def nProcessingElements : Int
  def datSz: Int
}

// Dynamic run-time parameters
class AuctionRunTimeParams extends Bundle {
  val nRows = UInt(32.W)
  val nCols = UInt(32.W)
}

// read and sum a contiguous stream of 32-bit uints from main memory
class Auction(p: PlatformWrapperParams, ap: AuctionParams) extends GenericAccelerator(p) {
  val numMemPorts = 1
  val io = IO(new GenericAcceleratorIF(numMemPorts, p) {
    val start = Input(Bool())
    val finished = Output(Bool())
    val baseAddr = Input(UInt(64.W))
    val nRows = Input(UInt(32.W))
    val nCols = Input(UInt(32.W))
    val byteCount = Input(UInt(32.W))
    val sum = Output(UInt(32.W))
    val cycleCount = Output(UInt(32.W))
  })
  io.signature := makeDefaultSignature()
  plugMemWritePort(0)

  val rdP = new StreamReaderParams(
    streamWidth = 32, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true
  )


  val reader = Module(new StreamReader(rdP)).io
  val red = Module(new StreamReducer(32, 0, {_+_})).io

  reader.start := io.start
  reader.baseAddr := io.baseAddr
  reader.byteCount := io.nRows * io.nCols


  // Added by erlingrj because chisel3 complains they are not initialized
  //  when inspecting verilog output of chisel2 synthesis they are commented out of the
  //  module interface of StreamReader, how?
  reader.doInit := false.B
  reader.initCount := 0.U

  red.start := io.start
  red.byteCount := io.byteCount

  io.sum := red.reduced
  io.finished := red.finished

  reader.req <> io.memPort(0).memRdReq
  io.memPort(0).memRdRsp <> reader.rsp

  reader.out <> red.streamIn

  val regCycleCount = RegInit(0.U(32.W))
  io.cycleCount := regCycleCount
  when(!io.start) {regCycleCount := 0.U}
    .elsewhen(io.start & !io.finished) {regCycleCount := regCycleCount + 1.U}
}



// DataDistributor connects to the memory stream and distributes the values to the PEs
class DataDistributor(ap: AuctionParams) extends MultiIOModule {
  val mem = IO(Flipped(Decoupled(UInt(ap.datSz.W))))
  val peOut = IO(Vec(ap.nProcessingElements, Decoupled(UInt(ap.datSz.W))))

  val cnt  = IO(Output(UInt(log2Ceil(ap.nProcessingElements).W)))

  val regCount = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))
  cnt := regCount
  // Initialize the output to 0s
  peOut.map({ (out: DecoupledIO[UInt]) =>
    out.valid := false.B
    out.bits := DontCare
  })

  // Connect the memory stream to the right PE
  mem <> peOut(regCount)

  when (mem.fire === true.B) {
    when (regCount === (ap.nProcessingElements-1).U ) {
      regCount := 0.U
    }.otherwise {
      regCount := regCount + 1.U
    }
  }.otherwise {
    regCount := regCount
  }
}

// ProcessingElements do the processing (subtraction) and calculates the net benefit
class ProessingElementIO(ap: AuctionParams) extends Bundle {
  val rewardIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val priceIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val benefitOut = Decoupled(UInt(ap.datSz.W))
}

class ProcessingElement(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new ProessingElementIO(ap))

  val sIdle :: sProcess :: sFinished :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regReward = RegInit(0.U(ap.datSz.W))
  val regPrice = RegInit(0.U(ap.datSz.W))
  val regBenefit = RegInit(0.U(ap.datSz.W))

  // Drive signals to default
  io.rewardIn.ready := false.B
  io.benefitOut.valid := false.B
  io.benefitOut.bits := 0.U
  io.priceIn.ready := false.B


  switch (regState) {
    is (sIdle) {
      // Idle state. We wait for valid input on both rewardIn and priceIn
      when(io.rewardIn.valid && io.priceIn.valid) {
        io.rewardIn.ready := true.B
        io.priceIn.ready := true.B
        regReward := io.rewardIn.bits
        regPrice := io.priceIn.bits
        regState := sProcess
      }

    }
    is (sProcess) {
      // We do calculation (subtraction) beware that we might get negative reward so check msb later
      regBenefit := regReward - regPrice
      regState := sFinished
    }
    is (sFinished) {
      // Expose result
      io.benefitOut.valid := true.B
      io.benefitOut.bits := regBenefit
      when (io.benefitOut.fire) {
        regState := sIdle
      }
    }
  }
}

// The serach tasks takes in one net benefit at the time and calculates the
// total highest, its index and its bid which is passed along to next node
class SearchTaskResult(private val ap: AuctionParams) extends Bundle {
  val winner = UInt(log2Ceil(ap.nProcessingElements).W)
  val bid = UInt(ap.datSz.W)
}

class SearchTaskIO(ap: AuctionParams) extends Bundle {
  val benefitIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val resultOut = Decoupled(new SearchTaskResult(ap))

  def driveDefaults(): Unit = {
    benefitIn.ready := false.B
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
}

class SearchTask(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new SearchTaskIO(ap))
  val regCurrentBest = RegInit(0.U)
  val regCurrentNextBest = RegInit(0.U)
  val regCount = RegInit(0.U)
  val regCurrentBestIdx = RegInit(0.U)

  val sProcess :: sFinished :: Nil = Enum(2)
  val regState = RegInit(sProcess)

  // Drive interface signals to default
  io.driveDefaults


  switch (regState) {

    is (sProcess) {
      io.benefitIn.ready := true.B
      io.resultOut.valid := false.B
      io.resultOut.bits := DontCare
      when (io.benefitIn.fire) {
        when(io.benefitIn.bits > regCurrentBest) {
          regCurrentBest := io.benefitIn.bits
          regCurrentNextBest := regCurrentBest
          regCurrentBestIdx := regCount
        }
        .otherwise
        {
          when(io.benefitIn.bits > regCurrentNextBest) {
            regCurrentNextBest := io.benefitIn.bits
          }
        }

        // Increment count
        when(regCount === (ap.nProcessingElements - 1).U) {
          regCount := 0.U
          regState := sFinished
        }. otherwise {
          regCount := regCount + 1.U
        }
      }
    }
    is (sFinished) {
      io.benefitIn.ready := false.B
      io.resultOut.valid := true.B
      io.resultOut.bits.winner := regCurrentBestIdx
      io.resultOut.bits.bid := regCurrentBest - regCurrentNextBest

      when (io.resultOut.fire) {
        regState := sProcess
      }
    }
  }
}

// AuctionController does a couple of things
// 1. It keeps a list over the assignments
// 2. It keeps a list over the prices
// 3. It requests the next column from StreamReader
// 4. Input the correct prices to the PEs
// 5. Take the search result and update assignments and unassigned

class StreamReaderControlSignals extends Bundle {
  val start = Input(Bool())
  val active = Output(Bool())
  val finished = Output(Bool())
  val error = Output(Bool())
  val baseAddr = Input(UInt(64.W)) // TODO: Make generic
  val byteCount = Input(UInt(32.W))


}

class AuctionControllerIO(ap: AuctionParams) extends Bundle {
  val searchResultIn = Flipped(Decoupled(new SearchTaskResult(ap)))
  val streamReaderCtrlSignals = Flipped(new StreamReaderControlSignals)
  val pricesOut = Vec(ap.nProcessingElements, Decoupled(UInt(ap.datSz.W)))
  val start = Input(Bool())
  val finished = Output(Bool())
  val baseAddress = Input(UInt(64.W))
  val nRows = Input(UInt(32.W))
  val nCols = Input(UInt(32.W))


  def driveDefaults() = {
    finished := false.B
    pricesOut.map({case (out) =>
      out.valid := false.B
      out.bits := 0.U
    })
    searchResultIn.ready := false.B
    streamReaderCtrlSignals.start := false.B
    streamReaderCtrlSignals.byteCount := 0.U
    streamReaderCtrlSignals.baseAddr := 0.U
  }
}


class AuctionController(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new AuctionControllerIO(ap))
  io.driveDefaults

  val regAssignments = RegInit(VecInit(Seq.fill(ap.nProcessingElements){ap.nProcessingElements.U}))
  val qUnassigned = Module(new Queue(UInt(), ap.nProcessingElements)).io
  qUnassigned.deq.ready := false.B
  qUnassigned.enq.valid := false.B
  qUnassigned.enq.bits := 0.U

  val regCurrentAgent= RegInit(0.U)

  val regPrices = RegInit(VecInit(Seq.fill(ap.nProcessingElements){0.U(ap.datSz.W)}))
  // Connect prices to the PEs
  (io.pricesOut zip regPrices).map({
    case (io, reg) =>
      io.valid := true.B
      io.bits := reg
  })



  val sSetup :: sIdle :: sReading :: sFinished :: Nil = Enum(4)
  val regState = RegInit(sSetup)

  val cnt = RegInit(0.U)
  switch (regState) {

    is (sSetup) {
      // Setup the unassigned queue
      qUnassigned.enq.valid := true.B
      qUnassigned.enq.bits := cnt
      when (cnt === (ap.nProcessingElements - 1).U) {
        cnt := 0.U
        regState := sIdle
      }.otherwise {
        cnt := cnt + 1.U
      }

    }
    is (sIdle) {
      when (io.start) {
        // Dequeue an element
        when (qUnassigned.deq.valid) {
          val unassigned = WireInit(qUnassigned.deq.bits)
          regCurrentAgent := unassigned

          qUnassigned.deq.ready := true.B
          assert(qUnassigned.deq.valid)

          // Start the streamReader
          io.streamReaderCtrlSignals.start := true.B
          io.streamReaderCtrlSignals.baseAddr := io.baseAddress + unassigned*ap.nProcessingElements.U
          io.streamReaderCtrlSignals.byteCount := io.nCols

          regState := sReading
        }.otherwise {
          // We have no unassigned agents. => We are finished
          io.finished := true.B
          regState := sSetup
        }

      }

    }
    is (sReading) {
      when (io.streamReaderCtrlSignals.finished) {
        regState := sFinished
      }
    }
    is (sFinished) {
      io.searchResultIn.ready := true.B

      when(io.searchResultIn.fire) {
        // Update unassigned and assigned and prices
        val obj = io.searchResultIn.bits.winner
        val bid = io.searchResultIn.bits.bid
        when (bid.asSInt >= 0.S) {
          qUnassigned.enq.bits := regAssignments(obj)
          qUnassigned.enq.valid := true.B
          assert(qUnassigned.enq.ready)

          regAssignments(obj) := regCurrentAgent
          regPrices(obj) := bid
        }
        regState := sIdle
      }
    }

  }
}