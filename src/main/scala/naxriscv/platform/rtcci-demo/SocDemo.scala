package naxriscv.platform.rtccidemo

import naxriscv.lsu.DataCachePlugin
import naxriscv.platform.{RvlsBackend, TilelinkNaxRiscvFiber}
import riscv.model.Model
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.StreamPipe
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.misc.TilelinkClintFiber
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA

// SocDemo is a little SoC made only for simulation purposes.
class SocDemo(cpuCount : Int, asic : Boolean = false, xlen : Int = 32) extends Component {
  // Create a few NaxRiscv cpu
  val naxes = for(hartId <- 0 until cpuCount) yield new TilelinkNaxRiscvFiber().setCoherentConfig(hartId, asic = asic, xlen = xlen)

  // As NaxRiscv may emit memory request to some unmapped memory space, we need to catch those with TransactionFilter
  val memFilter, ioFilter = new fabric.TransferFilter()
  for(nax <- naxes) {
    memFilter.up << List(nax.iBus, nax.dBus)
    ioFilter.up << List(nax.pBus)
  }

  // Creates tilelink Hub to connect cores
  val hub = new HubFiber()
  hub.up << memFilter.down
  hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
  hub.down.forceDataWidth(64) // TODO: better way to set the width? Getting value from cores' bus?
  var nonCoherent = hub.down

  // Tilelink to AXi4 conversion
  val bridge = new tilelink.fabric.Axi4Bridge
  bridge.up.forceDataWidth(64) // TODO: better way to get the width? Getting value from cores' bus?
  bridge.down.addTag(PMA.MAIN)
  bridge.up at SizeMapping(0x80000000l, 0x80000000l) of nonCoherent
  val mBus = Fiber build master(bridge.down)

  // Handle all the IO / Peripheral things
  val peripheral = new Area {
    val bus = Node()
    bus at (0x10000000l, 0x10000000l)  of (ioFilter.down)

    val clint = new TilelinkClintFiber()
    clint.node at 0x10000 of bus

    val plic = new TilelinkPlicFiber()
    plic.node at 0xC00000l of bus

    for(nax <- naxes) {
      nax.bind(clint)
      nax.bind(plic)
    }

    val emulated = new tilelink.fabric.SlaveBus(
      M2sSupport(
        addressWidth = 28,
        dataWidth = 32,
        transfers = M2sTransfers(
          get = SizeRange(4),
          putFull = SizeRange(4)
        )
      )
    )
    emulated.node << bus

    val custom = Fiber build new Area{
      val mei,sei = in Bool()
      naxes.foreach{ hart =>
        hart.getIntMachineExternal() setWhen mei
        hart.getIntSupervisorExternal() setWhen sei
      }
    }
  }
}

object SocDemo extends App{
  SpinalVerilog(new SocDemo(2))
}
