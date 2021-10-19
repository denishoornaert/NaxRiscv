package vooxriscv.frontend

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import vooxriscv.interfaces.JumpService
import vooxriscv.pipeline._
import vooxriscv.utilities.Plugin
import vooxriscv.Global

import scala.collection.mutable.ArrayBuffer



class PcPlugin(resetVector : BigInt = 0x80000000l) extends Plugin with JumpService{
  case class JumpInfo(interface :  Flow[UInt], priority : Int)
  val jumpInfos = ArrayBuffer[JumpInfo]()
  override def createJumpInterface(priority : Int = 0): Flow[UInt] = {
    val interface = Flow(UInt(32 bits))
    jumpInfos += JumpInfo(interface, priority)
    interface
  }

  val setup = create early new Area{
    val pipeline = getService(classOf[FrontendPlugin])
    pipeline.lock.retain()
  }

  val logic = create late{
    import vooxriscv.frontend.Frontend._
    val stage = setup.pipeline.getStage(0)
    val pipeline = setup.pipeline.getPipeline()
    import stage._

    val jump = new Area {
      val sortedByStage = jumpInfos.sortWith(_.priority > _.priority)
      val valids = sortedByStage.map(_.interface.valid)
      val pcs = sortedByStage.map(_.interface.payload)

      val pcLoad = Flow(FETCH_PC_VIRTUAL)
      pcLoad.valid := jumpInfos.map(_.interface.valid).orR
      if(valids.nonEmpty) {
        pcLoad.payload := MuxOH(OHMasking.first(valids.asBits), pcs)
      } else {
        pcLoad.payload.assignDontCare()
      }
    }

    val fetchPc = new Area{
      //PC calculation without Jump
      val output = Stream(UInt(32 bits))
      val pcReg = Reg(UInt(32 bits)) init(resetVector) addAttribute(Verilator.public)
      val correction = False
      val correctionReg = RegInit(False) setWhen(correction) clearWhen(output.fire)
      val corrected = correction || correctionReg
      val pcRegPropagate = False
      val booted = RegNext(True) init (False)
      val inc = RegInit(False) clearWhen(correction || pcRegPropagate) setWhen(output.fire) clearWhen(!output.valid && output.ready)
      val pc = pcReg + (inc ## B"00").asUInt


      val flushed = False

      if(RVC) when(inc) {
        pc(1) := False
      }

      when(jump.pcLoad.valid) {
        correction := True
        pc := jump.pcLoad.payload
        flushed := True
      }

      when(booted && (output.ready || correction || pcRegPropagate)){
        pcReg := pc
      }

      pc(0) := False
      if(!RVC) pc(1) := False

      val fetcherHalt = False
      output.valid := !fetcherHalt && booted
      output.payload := pc
    }

    fetchPc.output.ready := stage.isReady
    stage.valid := fetchPc.output.valid
    stage(FETCH_PC_VIRTUAL) := fetchPc.output.payload

    setup.pipeline.lock.release()
  }
}
