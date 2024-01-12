package naxriscv.platform.rtccidemo

import naxriscv.fetch.FetchCachePlugin
import naxriscv.lsu.DataCachePlugin
import naxriscv.platform.{FileBackend, NaxriscvProbe, NaxriscvTilelinkProbe, PeripheralEmulator, RvlsBackend}
import spinal.core._
import spinal.core.fiber.Fiber
import spinal.core.sim._
import spinal.lib.bus.amba4.axi.sim.{Axi4MemorySim, AxiMemorySimConfig}
import spinal.lib.bus.tilelink.ScopeFiber
import spinal.lib.bus.tilelink.sim.{Checker, MemoryAgent}
import spinal.lib.misc.Elf
import spinal.lib.misc.test.{DualSimTracer, MultithreadedFunSuite, MultithreadedTester}

import java.io.File
import java.lang
import scala.collection.mutable.ArrayBuffer




/*
Multi core SoC simulation.

Setup :
- You need Verilator installed
- Unless you add --no-rvls, you need to compile rvls. See ext/rvls/README.md

Parameters :
--trace : Enable wave capture
--dual-sim : Enable dual lock step simulation to only trace the 50000 cycles before failure
--naxCount INT : Number of NaxRiscv cores
--no-rvls : Disable rvls, so you don't need to compile it, but the NaxRiscv behaviour will not be checked.
--load-bin HEX,STRING : Load at address the given file. ex : 80000000,fw_jump.bin
--load-elf STRING : Load the given elf file. If both pass/fail symbole are defined, they will end the simulation once reached

The following commands are for the sbt terminal (that you can enter using "sbt" in the terminal)

Run baremetal example :
naxriscv.platform.tilelinkdemo.SocSim                                      \
--load-elf ext/NaxSoftware/baremetal/dhrystone/build/rv32ima/dhrystone.elf

Boot dual core linux example :
naxriscv.platform.tilelinkdemo.SocSim                                      \
--load-bin 80000000,ext/NaxSoftware/buildroot/images/rv32ima/fw_jump.bin   \
--load-bin 80F80000,ext/NaxSoftware/buildroot/images/rv32ima/linux.dtb  \
--load-bin 80400000,ext/NaxSoftware/buildroot/images/rv32ima/Image         \
--load-bin 81000000,ext/NaxSoftware/buildroot/images/rv32ima/rootfs.cpio   \
--nax-count 2
 */

object SocSim extends App {
  var dualSim = false // Double simulation, one ahead of the other which will trigger wave capture of the second simulation when it fail
  var traceIt = false
  var withRvls = true
  var asic = false
  var naxCount = 1
  var xlen = 32
  var coreMaxTransactions = 4
  var memoryResponseTime = 0
  val bins = ArrayBuffer[(Long, String)]()
  val elfs = ArrayBuffer[String]()

  assert(new scopt.OptionParser[Unit]("NaxRiscv") {
    help("help").text("prints this usage text")
    opt[Int]("xlen") action { (v, c) => xlen = v }
    opt[Unit]("dual-sim") action { (v, c) => dualSim = true }
    opt[Unit]("trace") action { (v, c) => traceIt = true }
    opt[Unit]("no-rvls") action { (v, c) => withRvls = false }
    opt[Unit]("asic") action { (v, c) => asic = true }
    opt[Int]("nax-count") action { (v, c) => naxCount = v }
    opt[Int]("core-max-trans") action { (v, c) => coreMaxTransactions = v }
    opt[Int]("mem-resp-time") action { (v, c) => memoryResponseTime = v }
    opt[Seq[String]]("load-bin") unbounded() action { (v, c) => bins += (lang.Long.parseLong(v(0), 16) -> v(1)) }
    opt[String]("load-elf") unbounded() action { (v, c) => elfs += v }
  }.parse(args, Unit).nonEmpty)

  val memoryMaxTransactions = naxCount*coreMaxTransactions

  val sc = SimConfig
  sc.normalOptimisation
  sc.withFstWave
  sc.withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC)).includeSimulation)
  sc.addSimulatorFlag("--threads 24")

  // Tweek the toplevel a bit
  class SocDemoSim(cpuCount : Int) extends SocDemo(cpuCount, coreMaxTransactions, asic = asic, xlen = xlen){
    setDefinitionName("SocDemo")
    // You can for instance override cache parameters of the CPU caches like that :
    naxes.flatMap(_.plugins).foreach{
      case p : FetchCachePlugin => //p.cacheSize = 2048
      case p : DataCachePlugin =>  //p.cacheSize = 2048
      case _ =>
    }

    // Here we add a scope peripheral, which can count the number of cycle that a given signal is high
    // See ext/NaxSoftware/baremetal/socdemo for software usages
    val scope = new ScopeFiber(){
      up at 0x04000000 of peripheral.bus
      lock.retain()

      val filler = Fiber build new Area {
        for ((nax, i) <- naxes.zipWithIndex) nax.plugins.foreach {
          case p: FetchCachePlugin => add(p.logic.refill.fire, i * 0x80 + 0x000)
          case p: DataCachePlugin => {
            add(p.logic.cache.refill.push.fire, i * 0x80 + 0x010)
            add(p.logic.cache.writeback.push.fire, i * 0x80 + 0x014)
          }
          case _ =>
        }
        lock.release()
      }
    }

  }
  val compiled = sc.compile(new SocDemoSim(cpuCount = naxCount))

  // How we want to run the test
  dualSim match {
    case true => DualSimTracer.withCb(compiled, window = 50000*10, seed = 2)(testIt)
    case false => compiled.doSimUntilVoid(name = s"test", seed = 2){dut => disableSimWave(); testIt(dut, f => if(traceIt) f)}
  }

  // Testbench code
  def testIt(dut : SocDemoSim, onTrace : (=> Unit) => Unit = cb => {}): Unit = {
    val cd = dut.clockDomain
    cd.forkStimulus(10 ns)
    // cd.forkSimSpeedPrinter(1.0)

    // Connect the few peripherals
    val memConfig = AxiMemorySimConfig(
      maxOutstandingReads = memoryMaxTransactions,
      maxOutstandingWrites = memoryMaxTransactions,
      readResponseDelay = memoryResponseTime,
      writeResponseDelay = memoryResponseTime
    )
    val ma = new Axi4MemorySim(dut.bridge.down, cd, memConfig)
    ma.start() // must start all threads in simulation
    val pa = new PeripheralEmulator(dut.peripheral.emulated.node.bus, dut.peripheral.custom.mei, dut.peripheral.custom.sei, cd)

    // Rvls will check that the CPUs are doing things right
    val rvls = withRvls generate new RvlsBackend(new File(compiled.compiledPath, currentTestName))
    if(withRvls) {
      rvls.spinalSimFlusher(10 * 10000)
      rvls.spinalSimTime(10000)
    }

    // Collect traces from the CPUs behaviour
    val naxes = dut.naxes.map(nax => new NaxriscvTilelinkProbe(nax, nax.getHartId()))
    if(withRvls) naxes.foreach(_.add(rvls))

    // Things to enable when we want to collect traces
    onTrace{
      enableSimWave()
      if(withRvls) rvls.debug()

      val tracerFile = new FileBackend(new File(new File(compiled.compiledPath, currentTestName), "tracer.log"))
      tracerFile.spinalSimFlusher(10 * 10000)
      tracerFile.spinalSimTime(10000)
      naxes.foreach { hart =>
        hart.add(tracerFile)
        val r = hart.backends.reverse
        hart.backends.clear()
        hart.backends ++= r
      }
    }

    // Load the binaries
    for((offset, file) <- bins){
      ma.memory.loadBinary(offset - 0x80000000l, file)
      if(withRvls) rvls.loadBin(offset, new File(file))
    }

    // load elfs
    for (file <- elfs) {
      val elf = new Elf(new File(file), xlen)
      elf.load(ma.memory, 0x80000000l)
      if(withRvls) rvls.loadElf(0, elf.f)

      if(elf.getELFSymbol("pass") != null && elf.getELFSymbol("fail") != null) {
        val passSymbol = elf.getSymbolAddress("pass")
        val failSymbol = elf.getSymbolAddress("fail")
        naxes.foreach { nax =>
          nax.commitsCallbacks += { (hartId, pc) =>
            if (pc == passSymbol) delayed(1) {
              dut.naxes.foreach { nax =>
                println(s"Hart ${nax.getHartId()}")
                nax.plugins.foreach {
                  case p: FetchCachePlugin => println("- i$ refill = " + p.logic.refill.pushCounter.toLong)
                  case p: DataCachePlugin => println("- d$ refill = " + p.logic.cache.refill.pushCounter.toLong)
                  case _ =>
                }
              }
              ma.stop()
              simSuccess()
            }
            if (pc == failSymbol) delayed(1) {
              ma.stop()
              simFailure("Software reach the fail symbole :(")
            }
          }
        }
      }
    }

    println("Sim starting <3")
  }
}
