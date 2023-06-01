// See README.md for license details.

package naxriscv.lsu

import scala.collection.mutable.ArrayBuffer

import spinal.core._
import spinal.lib._

// TODO: Include meta state memory in classes.

abstract class EvictionPolicy(ways: Int, state_width: Int) extends Area {

  def victim() : UInt

  def store_lookup(address: UInt) : Unit

  def load_lookup(address: UInt) : Unit

  def on_miss(address: UInt) : Unit
  
  def on_hit(address: UInt, selection: UInt) : Unit
  
  def on_invalidate(address: UInt, selection: UInt) : Unit

  def on_reset(address: UInt) : Unit

}

case class RandomFreeCounter(ways: Int, state_width: Int) extends EvictionPolicy(ways, state_width) {

  val counter = CounterFreeRun(ways)

  override def victim() : UInt = {
    return counter.value
  }

  override def store_lookup(address: UInt) : Unit = {
  }

  override def load_lookup(address: UInt) : Unit = {
  }
  
  override def on_miss(address: UInt) : Unit = {
  }
  
  override def on_hit(address: UInt, selection: UInt) : Unit = {
  }
  
  override def on_invalidate(address: UInt, selection: UInt) : Unit = {
  }

  override def on_reset(address: UInt) : Unit = {
  }

}

case class RandomLFSR(ways: Int, state_width: Int) extends EvictionPolicy(ways, state_width) {

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

  val counter = Reg(UInt(ways bits)) init(1)

  // Compare state with mask for each state to determine the LRU way
  override def victim() : UInt = {
    for (i <- 0 until ways-1) {
      if (taps(ways).contains(i+1))
        counter(i) := counter(i+1) ^ counter(0)
      else
        counter(i) := counter(i+1)
    }
    counter(ways-1) := counter(0)
    return counter(log2Up(ways)-1 downto 0)
  }

  override def store_lookup(address: UInt) : Unit = {
  }

  override def load_lookup(address: UInt) : Unit = {
  }

  override def on_miss(address: UInt) : Unit = {
  }
  
  override def on_hit(address: UInt, selection: UInt) : Unit = {
  }
  
  override def on_invalidate(address: UInt, selection: UInt) : Unit = {
  }

  override def on_reset(address: UInt) : Unit = {
  }

}

abstract class LRULogic(ways: Int, state_width: Int) extends EvictionPolicy(ways, state_width) {
 
  case class Meta() extends Bundle{
    val state = UInt(metaWidth bits)
  }

  def zero_mask_indices(index: Int) : Array[Int] = {
    var i = state_width-1
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
    var i = state_width-1
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
    val candidate_next_states = Array.tabulate(ways)(n => Bits(state_width bits))
    for (way <- 0 until ways) {
      var unchanged_indices = ArrayBuffer.tabulate(state_width)(n => n)
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
    val candidate_next_states = Array.tabulate(ways)(n => Bits(state_width bits))
    for (way <- 0 until ways) {
      var unchanged_indices = ArrayBuffer.tabulate(state_width)(n => n)
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

  val port = new Area{
    val valid = Bool()
    val address = UInt(log2Up(linePerWay) bits)
    val meta = Meta()

    valid := False
    address.assignDontCare()
    meta.assignDontCare()
  }

  val mem = Mem.fill(linePerWay)(Meta())
  mem.write(port.address, port.meta, port.valid)
  
  val loadRead = new Area{
    val cmd = Flow(mem.addressType)
    val rsp = if(tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
    KeepAttribute(rsp)
  }
  
  val storeRead = new Area{
    val cmd = Flow(mem.addressType)
    val rsp = if(tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
    KeepAttribute(rsp)
  }

}


case class LRU(ways: Int, state_width: Int) extends LRULogic(ways, state_width) {

  override def victim() : UInt = {
    return get_least_recently_used(SET_META.state)
  }

  override def store_lookup(condition: Bool, address: UInt) : UInt = {
    storeRead.cmd.valid := condition
    storeRead.cmd.payload := address
    return storeRead.rsp
  }

  override def load_lookup(address: UInt) : Unit = {
  }

  override def on_miss(address: UInt) : Unit = {
    // write at set's state an updated version of the state where the way indicated by 'WAYS_HITS' is touched
    val lru = get_least_recently_used(SET_META.state)
    port.valid := True
    port.address := address
    port.meta.state := upgrade_order_encoding(SET_META.state, lru)
  }
  
  override def on_hit(address: UInt, selection: UInt) : Unit = {
    // write at set's state an updated version of the state where the way indicated by 'WAYS_HITS' is touched
    port.valid := True
    port.address := address
    port.meta.state := upgrade_order_encoding(SET_META.state, selection)
  }
  
  override def on_invalidate(address: UInt, selection: UInt) : Unit = {
    // write at set's state an updated version of the state where the way indicated by 'WAYS_HITS' is touched
    port.valid := True
    port.address := address
    port.meta.state := downgrade_order_encoding(SET_META.state, selection)
  }

  override def on_reset(address: UInt) : Unit = {
    port.valid := True
    port.address := address
    port.meta.state := U(0, state_width bits)
  }

}


case class FIFO(ways: Int, state_width: Int) extends LRULogic(ways, state_width) {

  override def victim() : UInt = {
    return get_least_recently_used(SET_META.state)
  }

  override def store_lookup(address: UInt) : Unit = {
  }

  override def load_lookup(address: UInt) : Unit = {
  }

  override def on_miss(address: UInt) : Unit = {
    // write at set's state an updated version of the state where the way indicated by 'WAYS_HITS' is touched
    val lru = get_least_recently_used(SET_META.state)
    port.valid := True
    port.address := address
    port.meta.state := upgrade_order_encoding(SET_META.state, lru)
  }
  
  override def on_hit(address: UInt, selection: UInt) : Unit = {
    // write at set's state an updated version of the state where the way indicated by 'WAYS_HITS' is touched
    port.valid := True
    port.address := address
    port.meta.state := SET_META.state
  }
  
  override def on_invalidate(address: UInt, selection: UInt) : Unit = {
    // write at set's state an updated version of the state where the way indicated by 'WAYS_HITS' is touched
    port.valid := True
    port.address := address
    port.meta.state := downgrade_order_encoding(SET_META.state, selection)
  }

  override def on_reset(address: UInt) : Unit = {
    port.valid := True
    port.address := address
    port.meta.state := U(0, state_width bits)
  }

}
