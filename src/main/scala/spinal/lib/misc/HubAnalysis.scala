package spinal.lib.misc

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent._

class HubAnalysis extends Component  {
    val hub = new Hub(HubGen.basicConfig(
        probeCount = 2,
        downPendingMax = 8,
        dataWidth = 32,
        addressWidth = 32,
        prioWidth = 8,
        masterPerChannel = 8,

        wayCount = 1, // 2
        setCount = 1, // 256
        lineSize = 32 // 64
    ))
    
    val io = new Bundle {
        val up = hub.io.up simPublic()
        val down = hub.io.down simPublic()
    }
}

object HubAnalysisSim extends App {
    val sc = SimConfig
    sc.normalOptimisation
    sc.withFstWave
    sc.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC)).includeSimulation)
    sc.addSimulatorFlag("--threads 24")

    sc.compile(new HubAnalysis()).doSim { hub =>
        hub.clockDomain.forkStimulus(period = 10)
        hub.io.up.a.valid #= false
        hub.io.up.b.ready #= true
        hub.io.up.c.valid #= false
        hub.io.up.d.ready #= true
        hub.io.up.e.valid #= false
        hub.io.up.a.payload.opcode #= Opcode.A.ACQUIRE_BLOCK
        hub.io.up.a.payload.param #= 0
        hub.io.up.a.payload.prio #= 0
        hub.io.up.a.payload.size #= 6
        hub.io.up.a.payload.source #= 4
        hub.io.up.a.payload.mask #= 0xf
        hub.io.up.a.payload.corrupt #= false
        hub.io.up.a.payload.data #= 0
        
        hub.clockDomain.waitSamplingWhere(hub.io.up.a.ready.toBoolean)
        for( idx <- 1 to 5){
            hub.io.up.a.payload.address #= 1000 * idx
            hub.io.up.a.valid #= true
            hub.clockDomain.waitSampling(1)
            hub.io.up.a.valid #= false
            hub.clockDomain.waitSamplingWhere(1000)(hub.io.up.a.ready.toBoolean)
        }
    }
}