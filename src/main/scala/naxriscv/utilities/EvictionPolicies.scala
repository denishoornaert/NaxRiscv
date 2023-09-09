// See README.md for license details.

package naxriscv.utilities

import scala.collection.mutable.ArrayBuffer

import spinal.core._
import spinal.lib._


abstract class EvictionPolicy(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends Area {

  // Note: Is a policy specific hyper-parameter. It must be a scala Boolean in order to be evaluated during elaboration of the design.
  val usesGlobalState = false

  // indicates the amount of banks used by the policy. Whenever the state is global, 'banks' must be set to 1
  val bankRange = log2Up(banks)-1 downto 0 
  val indexRange = log2Up(linePerWay)-1 downto log2Up(banks)
  
  // Placeholder
  val stateWidth = 1

  // Mock Areas placed here for interfacing
  val read = new Area{
    val load = new Area{
      val cmd = Flow(UInt(log2Up(linePerWay) bits))
      val rsp = U(0, stateWidth bits)
      KeepAttribute(rsp)
    }
    val store = new Area{
      val cmd = Flow(UInt(log2Up(linePerWay) bits))
      val rsp = U(0, stateWidth bits)
      KeepAttribute(rsp)
    }
  }

  // Mock Areas placed here for interfacing
  val write = new Area{
    val load = new Area{
      val valid = Bool()
      val address = UInt(log2Up(linePerWay) bits)
      val state = UInt(stateWidth bits)

      // May not be needed
      valid := False
      address.assignDontCare()
      state.assignDontCare()
      
      // May not be needed
      val cached = new Area {
        val valid = Reg(Bool())
        val address = Reg(UInt(log2Up(linePerWay) bits))
        val state = Reg(UInt(stateWidth bits))
      }
    }

    val store = new Area{
      val valid = Bool()
      val address = UInt(log2Up(linePerWay) bits)
      val state = UInt(stateWidth bits)

      // May not be needed
      valid := False
      address.assignDontCare()
      state.assignDontCare()
      
      // May not be needed
      val cached = new Area {
        val valid = Reg(Bool())
        val address = Reg(UInt(log2Up(linePerWay) bits))
        val state = Reg(UInt(stateWidth bits))
      }
    }
  }

  def get_valid_write_hit() : Bool

  def get_next_state_hit(state: UInt, touch_way: UInt) : UInt

  def get_valid_write_miss() : Bool

  def get_next_state_miss(state: UInt, touch_way: UInt) : UInt

  def get_valid_write_invalidate() : Bool
  
  def get_invalidate_state(state: UInt, touch_way: UInt) : UInt

  def get_cached_valid(address: UInt) : Bool

  def get_cached_state(address: UInt) : UInt

  def get_reset_state() : UInt

  def get_replace_way(state: UInt) : UInt

}


case class RandomFreeCounter(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends EvictionPolicy(ways, linePerWay, tagsReadAsync, banks) {

  override val stateWidth = 6

  val counter = CounterFreeRun(log2Up(ways))

  override val usesGlobalState = true

  // State does not change upon hits
  override def get_valid_write_hit() : Bool = {
    return False
  }

  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return U(0, stateWidth bits) 
  }

  // State does not change upon hits
  override def get_valid_write_miss() : Bool = {
    return True
  }

  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_valid_write_invalidate() : Bool = {
    return False
  }
  
  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_cached_valid(address: UInt) : Bool = {
    return False
  }

  override def get_cached_state(address: UInt) : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_reset_state() : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_replace_way(state: UInt) : UInt = {
    return counter.value
  }

}


case class RandomLFSR(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends EvictionPolicy(ways, linePerWay, tagsReadAsync, banks) {

  override val stateWidth = 6

  // From "Table of Linear Feedback Shift Register" by Roy Ward and Tim Molteno
  // LFSR-4 is always chosen unless only LFSR-2 is available
  val taps = Map(
     2 -> Seq(         2,  1),
     3 -> Seq(         3,  2),
     4 -> Seq(         4,  3),
     5 -> Seq( 5,  4,  3,  2),
     6 -> Seq( 6,  5,  3,  2),
     7 -> Seq( 7,  6,  5,  4),
     8 -> Seq( 8,  6,  5,  4),
     9 -> Seq( 9,  8,  6,  5),
    10 -> Seq(10,  9,  7,  6),
    11 -> Seq(11, 10,  9,  7),
    12 -> Seq(12, 11,  8,  6),
    13 -> Seq(13, 12, 10,  9),
    14 -> Seq(14, 13, 11,  9),
    15 -> Seq(15, 14, 13, 11),
    16 -> Seq(16, 14, 13, 11)
  )

  val counter = Reg(UInt(stateWidth bits)) init(1)

  override val usesGlobalState = true

  // State does not chnage upon hits
  override def get_valid_write_hit() : Bool = {
    return False
  }

  // State does not change upon hits (could be a constant value...)
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return state
  }

  // State must be updated upon misses
  override def get_valid_write_miss() : Bool = {
    return True
  }

  // STate must be updated upon misses
  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    val newState = UInt(stateWidth bits)
    for (i <- 0 until stateWidth-1) {
      if (taps(stateWidth).contains(i+1))
        newState(i) := state(i+1) ^ state(0)
      else
        newState(i) := state(i+1)
    }
    newState(stateWidth-1) := state(0)
    return newState
  }

  override def get_valid_write_invalidate() : Bool = {
    return False
  }
  
  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return state
  }

  override def get_cached_valid(address: UInt) : Bool = {
    return write.load.cached.valid | write.store.cached.valid
  }

  override def get_cached_state(address: UInt) : UInt = {
    return counter
  }

  override def get_reset_state() : UInt = {
    return U(1, stateWidth bits)
  }

  override def get_replace_way(state: UInt) : UInt = {
    return state(log2Up(ways)-1 downto 0)
  }

  // Storing
  when (write.load.valid) {
    counter := write.load.state
  }
  .elsewhen (write.store.valid) {
    counter := write.store.state
  }

  // Caching
  write.load.cached.valid := write.load.valid
  write.store.cached.valid := write.store.valid
  
  override val read.load.rsp = counter
  override val read.store.rsp = counter
}


abstract class LRULogic(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends EvictionPolicy(ways, linePerWay, tagsReadAsync, banks) {

  override val stateWidth = ((ways*ways)-ways)/2

  def zero_mask_indices(index: Int) : Array[Int] = {
    var i = stateWidth-1
    val indices = Array.tabulate(ways)(n => Array.tabulate(n)(i => 0))
    for (row <- ways-1 to 0 by -1) {
      for (col <- 0 until indices(row).size) {
        indices(row)(col) = i
        i -= 1
      }
    }
    return indices(index)
  }
  
  def one_mask_indices(index: Int) : Array[Int] = {
    var i = stateWidth-1
    val indices = Array.tabulate(ways)(n => Array.tabulate(ways-n-1)(i => 0))
    for (col <- 0 until ways-1) {
      for (row <- ways-2-col to 0 by -1) {
        indices(row)(col) = i
        i -= 1
      }
    }
    return indices(index)
  }
 
  // Upgrades the order encoding based on the way touched
  def upgrade_order_encoding(state: UInt, touch_way: UInt) : UInt = {
    // Generate candidate states
    val candidate_next_states = Array.tabulate(ways)(n => Bits(stateWidth bits))
    for (way <- 0 until ways) {
      var unchanged_indices = ArrayBuffer.tabulate(stateWidth)(n => n)
      unchanged_indices --= one_mask_indices(way)
      unchanged_indices --= zero_mask_indices(way)
      for (index <- unchanged_indices)
        candidate_next_states(way)(index) := state(index)
      for (index <- one_mask_indices(way))
        candidate_next_states(way)(index) := True
      for (index <- zero_mask_indices(way))
        candidate_next_states(way)(index) := False
    }
    return touch_way.muxList(for (n <- 0 until ways) yield (n, candidate_next_states(n).asUInt))
  }

  // Downgrades the order encoding based on the way touched
  def downgrade_order_encoding(state: UInt, touch_way: UInt) : UInt = {
    // Generate candidate states
    val candidate_next_states = Array.tabulate(ways)(n => Bits(stateWidth bits))
    for (way <- 0 until ways) {
      var unchanged_indices = ArrayBuffer.tabulate(stateWidth)(n => n)
      unchanged_indices --= one_mask_indices(way)
      unchanged_indices --= zero_mask_indices(way)
      for (index <- unchanged_indices)
        candidate_next_states(way)(index) := state(index)
      for (index <- one_mask_indices(way))
        candidate_next_states(way)(index) := False
      for (index <- zero_mask_indices(way))
        candidate_next_states(way)(index) := True
    }
    return touch_way.muxList(for (n <- 0 until ways) yield (n, candidate_next_states(n).asUInt))
  }

  // Generates logical evaluation on order encoding to tell if the way is the least recently used
  def is_least_recently_used(state: UInt, zero_indices: Array[Int], one_indices: Array[Int]) : Bool = {
    // Concatenate all bits of 'state' indicated by 'one_indices'
    val must_be_ones  = Cat(Seq.tabulate(one_indices.size)(i => state(one_indices(i))))
    // Evaluate whether all the bits in 'state' that should be set to 1 are set to 1
    val must_be_ones_are_one = must_be_ones.andR
    // Concatenate all bits of 'state' indicated by 'zero_indices'
    val must_be_zeros = Cat(Seq.tabulate(zero_indices.size)(i => state(zero_indices(i))))
    // Evaluate whether all the bits in 'state' that should be set to 0 are set to 0 (i.e., all negated and reduced and)
    val must_be_zeros_are_zero = (~must_be_zeros).andR
    // checks that the bits in state that should be set to one and to zero are correctly set
    return must_be_ones_are_one & must_be_zeros_are_zero
  }

  // Compare state with mask for each state to determine the LRU way
  def get_least_recently_used(state: UInt) : UInt = {
    val least_recently_used = Bits(ways bits)
    for (way <- 0 until ways)
      least_recently_used(way) := is_least_recently_used(state, one_mask_indices(way), zero_mask_indices(way))
    return OHToUInt(least_recently_used)
  }

  override val usesGlobalState = false

  override def get_cached_valid(address: UInt) : Bool = {
    val load_cache_hit = (write.load.cached.valid & (write.load.cached.address === address)) 
    val store_cache_hit = (write.store.cached.valid & (write.store.cached.address === address))
    return load_cache_hit | store_cache_hit
  }

  override def get_cached_state(address: UInt) : UInt = {
    val load_cache_hit = (write.load.cached.valid & (write.load.cached.address === address)) 
    val store_cache_hit = (write.store.cached.valid & (write.store.cached.address === address))
    return Mux(load_cache_hit,
      write.load.cached.state,
      Mux(store_cache_hit,
        write.store.cached.state,
        U(0, stateWidth bits)
      )
    )
  }

//  override val write.load.state = UInt(stateWidth bits)
//  override val write.load.cached.state = Reg(UInt(stateWidth bits))
//  override val write.store.state = UInt(stateWidth bits)
//  override val write.store.cached.state = Reg(UInt(stateWidth bits))

  override val write = new Area{
    val load = new Area{
      val valid = Bool()
      val address = UInt(log2Up(linePerWay) bits)
      val state = UInt(stateWidth bits)

      valid := False
      address.assignDontCare()
      state.assignDontCare()

      val cached = new Area {
        val valid = Reg(Bool())
        val address = Reg(UInt(log2Up(linePerWay) bits))
        val state = Reg(UInt(stateWidth bits))
      }
    }

    val store = new Area{
      val valid = Bool()
      val address = UInt(log2Up(linePerWay) bits)
      val state = UInt(stateWidth bits)

      valid := False
      address.assignDontCare()
      state.assignDontCare()
      
      val cached = new Area {
        val valid = Reg(Bool())
        val address = Reg(UInt(log2Up(linePerWay) bits))
        val state = Reg(UInt(stateWidth bits))
      }
    }
  }

  // Storing state
  val mem = for(id <- 0 until banks) yield new Area {
    val bank = Mem.fill(linePerWay/banks)(UInt(stateWidth bits))
    val addrload = write.load.address(bankRange)
    val addrstore = write.store.address(bankRange)
    val selload = (addrload === id)
    val selstore = (addrstore === id)
    bank.write(
      Mux(write.load.valid & selload, write.load.address(indexRange), write.store.address(indexRange)),
      Mux(write.load.valid & selload, write.load.state  , write.store.state  ),
      (write.load.valid & selload) | (write.store.valid & selstore)
    )
  }

  // Caching
  //// Load
  write.load.cached.valid := write.load.valid
  when (write.load.valid) {
    write.load.cached.address := write.load.address
    write.load.cached.state := write.load.state
  }
  .otherwise {
    write.load.cached.address := 0
    write.load.cached.state := 0
  }
  //// Store
  write.store.cached.valid := write.store.valid
  when(write.store.valid) {
    write.store.cached.address := write.store.address
    write.store.cached.state := write.store.state
  }
  .otherwise {
    write.store.cached.address := 0
    write.store.cached.state := 0
  }

//  override val read.load.rsp = read.load.cmd.payload(bankRange).muxList(for (b <- 0 until banks) yield (b, if (tagsReadAsync) mem(b).bank.readAsync(read.load.cmd.payload(indexRange)) else mem(b).bank.readSync(read.load.cmd.payload(indexRange), read.load.cmd.valid)))
//  override val read.store.rsp = read.store.cmd.payload(bankRange).muxList(for (b <- 0 until banks) yield (b, if (tagsReadAsync) mem(b).bank.readAsync(read.store.cmd.payload(indexRange)) else mem(b).bank.readSync(read.store.cmd.payload(indexRange), read.store.cmd.valid)))


  override val read = new Area{
    val load = new Area{
      val cmd = Flow(UInt(log2Up(linePerWay) bits))
      val rsp = cmd.payload(bankRange).muxList(for (b <- 0 until banks) yield (b, if (tagsReadAsync) mem(b).bank.readAsync(cmd.payload(indexRange)) else mem(b).bank.readSync(cmd.payload(indexRange), cmd.valid)))
      KeepAttribute(rsp)
    }
    val store = new Area{
      val cmd = Flow(UInt(log2Up(linePerWay) bits))
      val rsp = cmd.payload(bankRange).muxList(for (b <- 0 until banks) yield (b, if (tagsReadAsync) mem(b).bank.readAsync(cmd.payload(indexRange)) else mem(b).bank.readSync(cmd.payload(indexRange), cmd.valid)))
      KeepAttribute(rsp)
    }
  }

}


case class LRU(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends LRULogic(ways, linePerWay, tagsReadAsync, banks) {

  // State changes upon hits
  override def get_valid_write_hit() : Bool = {
    return True
  }

  // Returns the next updated state upon touching a way
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way) 
  }

  // State changes upon misses
  override def get_valid_write_miss() : Bool = {
    return True
  }

  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way)
  }

  override def get_valid_write_invalidate() : Bool = {
    return True
  }
  
  // Returns the next updated state with indicated way being invalidated
  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return downgrade_order_encoding(state, touch_way)
  }

  // Returns the encoding of state for reset mode
  override def get_reset_state() : UInt = {
    return U(0, stateWidth bits)
  }

  // Compare state with mask for each state to determine the LRU way
  override def get_replace_way(state: UInt) : UInt = {
    return get_least_recently_used(state)
  }

}


case class FIFO(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends LRULogic(ways, linePerWay, tagsReadAsync, banks) {

  // State does not change upon hits
  override def get_valid_write_hit() : Bool = {
    return False
  }

  // Returns the next updates state upon touching a way
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return state
  }

  // State changes upon misses
  override def get_valid_write_miss() : Bool = {
    return True
  }

  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way)
  }

  override def get_valid_write_invalidate() : Bool = {
    return False
  }
  
  // Returns the next updated state with indicated way being invalidated
  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return downgrade_order_encoding(state, touch_way)
  }

  // Returns the encoding of state for reset mode
  override def get_reset_state() : UInt = {
    return U(0, stateWidth bits)
  }

  // Compare state with mask for each state to determine the LRU way
  override def get_replace_way(state: UInt) : UInt = {
    return get_least_recently_used(state)
  }

}


case class LIP(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) extends LRULogic(ways, linePerWay, tagsReadAsync, banks) {

  // State changes upon hits
  override def get_valid_write_hit() : Bool = {
    return True
  }

  // Returns the next updated state upon touching a way iff the touched way is also the least recently used way
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return Mux(touch_way === get_least_recently_used(state), upgrade_order_encoding(state, touch_way), state)
  }

  // State changes upon misses
  override def get_valid_write_miss() : Bool = {
    return True
  }

  // Make sure that teh selected way is at the lru place
  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return downgrade_order_encoding(state, touch_way)
  }

  override def get_valid_write_invalidate() : Bool = {
    return True
  }
  
  // Returns the next updated state with indicated way being invalidated
  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return downgrade_order_encoding(state, touch_way)
  }

  // Returns the encoding of state for reset mode
  override def get_reset_state() : UInt = {
    return U(0, stateWidth bits)
  }

  // Compare state with mask for each state to determine the LRU way
  override def get_replace_way(state: UInt) : UInt = {
    return get_least_recently_used(state)
  }

}


case class BIP(ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int, denominator: Int) extends LRULogic(ways, linePerWay, tagsReadAsync, banks) {

  override val stateWidth = 6

  // Find the relevant range to compare to in the random number register
  assert(List(1, 2, 4, 8, 16, 32, 64).contains(denominator), "BIP does not support 1/"+denominator+" probability. `denominator` must be included in [1, 2, 4, 8, 16, 32, 64]")
  val relevantRange = log2Up(denominator)-1 downto 0

  // Random number register
  val random_number = CounterFreeRun(stateWidth)

  // State changes upon hits
  override def get_valid_write_hit() : Bool = {
    return True
  }

  // Returns the next updated state upon touching a way iff the touched way is also the least recently used way
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way) 
  }

  // State changes upon misses
  override def get_valid_write_miss() : Bool = {
    return True
  }

  // Make sure that teh selected way is at the lru place
  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return Mux(random_number(relevantRange) === 0, upgrade_order_encoding(state, touch_way), downgrade_order_encoding(state, touch_way))
  }

  override def get_valid_write_invalidate() : Bool = {
    return True
  }
  
  // Returns the next updated state with indicated way being invalidated
  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return downgrade_order_encoding(state, touch_way)
  }

  // Returns the encoding of state for reset mode
  override def get_reset_state() : UInt = {
    return U(0, stateWidth bits)
  }

  // Compare state with mask for each state to determine the LRU way
  override def get_replace_way(state: UInt) : UInt = {
    return get_least_recently_used(state)
  }

}

object EvictionPolicy {
  
  def make(policy: String, ways: Int, linePerWay: Int, tagsReadAsync: Boolean, banks: Int) : EvictionPolicy = {
    val supportedPolicies = List("RandomFreeCounter", "RandomLFSR", "LRU", "FIFO", "LIP")
    assert(supportedPolicies.contains(policy), "Specified eviction policy ("+policy+") not available!")
    val res = policy match {
      case "RandomFreeCounter" => new RandomFreeCounter(ways, linePerWay, tagsReadAsync, banks)
      case "RandomLFSR"        =>        new RandomLFSR(ways, linePerWay, tagsReadAsync, banks)
      case "LRU"               =>               new LRU(ways, linePerWay, tagsReadAsync, banks)
      case "FIFO"              =>              new FIFO(ways, linePerWay, tagsReadAsync, banks)
      case "LIP"               =>               new LIP(ways, linePerWay, tagsReadAsync, banks)
      case "BIP"               =>               new BIP(ways, linePerWay, tagsReadAsync, banks, 16)
    }
    return res
  }

}
