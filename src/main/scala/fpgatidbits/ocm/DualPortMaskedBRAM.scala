package fpgatidbits.ocm

import chisel3._
import chisel3.util._

// creates a BRAM of desired size, which supports partial writes at "unit"
// granularity. which parts will be written is determined by the writeMask.
// internally, this is accomplished by instantiating a number of standard
// dual-port BRAMs of width = unit, and routing the write/read data accordingly

class DualPortMaskedBRAM(addrBits: Int, dataBits: Int, unit: Int = 8)
extends Module {
  val numBanks = dataBits/unit
  val io = new DualPortMaskedBRAMIO(addrBits, dataBits, numBanks)
  val banksExt = VecInit(Seq.fill(numBanks) {
    Module(new DualPortBRAM(addrBits, unit)).io
  })
  val banks = WireInit(VecInit(Seq.fill(numBanks) {
    new DualPortBRAMIO(addrBits, unit)
  }))

  (banksExt zip banks).map({
    case (ext, int) =>
      ext.clk := clock
      ext.a.connect(int.ports(0))
      ext.b.connect(int.ports(1))
  })



  // assign zero to readData to enable partial assignment in loop
  for(p <- 0 until 2) {
    io.ports(p).rsp.readData := 0.U
  }

  for(i <- 0 until numBanks) {
    for(p <- 0 until 2) {
      // base request data goes to all banks
      banks(i).ports(p).req.addr := io.ports(p).req.addr
      // each bank gets one byte of data
      val bankWrData = io.ports(p).req.writeData((i+1)*unit-1, i*unit)
      banks(i).ports(p).req.writeData := bankWrData
      // each bank's write enable is computed separately
      val bankWrEn = io.ports(p).req.writeEn & io.ports(p).req.writeMask(i)
      banks(i).ports(p).req.writeEn := bankWrEn
      // use partial assignment to concatenate read data
      io.ports(p).rsp.readData((i+1)*unit-1, i*unit) := banks(i).ports(p).rsp.readData
    }
  }
}

class OCMMaskedRequest(writeWidth: Int, addrWidth: Int, maskWidth: Int)
extends OCMRequest(writeWidth, addrWidth) {
  if(writeWidth % maskWidth != 0)
    throw new Exception("Mask-writable BRAM needs data width % mask width = 0")
  val writeMask = VecInit(Seq.fill(maskWidth) {Bool()})

  override def cloneType: this.type =
    new OCMMaskedRequest(writeWidth, addrWidth, maskWidth).asInstanceOf[this.type]
}

class OCMMaskedSlaveIF(dataWidth: Int, addrWidth: Int, maskWidth: Int)
extends Bundle {
  val req = Input(new OCMMaskedRequest(dataWidth, addrWidth, maskWidth))
  val rsp = Output(new OCMResponse(dataWidth))

  override def cloneType: this.type =
    new OCMMaskedSlaveIF(dataWidth, addrWidth, maskWidth).asInstanceOf[this.type]
}

class DualPortMaskedBRAMIO(addrBits: Int, dataBits: Int, maskBits: Int)
extends Bundle {
  val ports = Vec(2, new OCMMaskedSlaveIF(dataBits, addrBits, maskBits))

  override def cloneType: this.type =
    new DualPortMaskedBRAMIO(addrBits, dataBits, maskBits).asInstanceOf[this.type]
}
