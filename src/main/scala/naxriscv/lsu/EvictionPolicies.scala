// See README.md for license details.

package naxriscv.lsu

import scala.collection.mutable.ArrayBuffer

import spinal.core._
import spinal.lib._


abstract class EvictionPolicy(way: Int, linePerWay: Int, tagsReadAsync: Boolean) extends Area {

  val write = new Area{}

  val read = new Area{}

  def get_next_state_hit(state: UInt, touch_way: UInt) : UInt

  def get_next_state_miss(state: UInt, touch_way: UInt) : UInt

  def get_invalidate_state(state: UInt, touch_way: UInt) : UInt

  def get_reset_state() : UInt

  def get_replace_way(state: UInt) : UInt

}


case class RandomFreeCounter(ways: Int, linePerWay: Int, tagsReadAsync: Boolean) extends EvictionPolicy(ways, linePerWay, tagsReadAsync) {

  val stateWidth = 0

  val counter = CounterFreeRun(log2Up(ways))

  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return U(0, stateWidth bits) 
  }

  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_reset_state() : UInt = {
    return U(0, stateWidth bits)
  }

  override def get_replace_way(state: UInt) : UInt = {
    return counter.value
  }

  override val write = new Area{
    val valid = Bool()
    val address = UInt(log2Up(linePerWay) bits)
    val state = UInt(stateWidth bits)

    valid := False
    address.assignDontCare()
    state.assignDontCare()
  }

  override val read = new Area{
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

}


case class RandomLFSR(ways: Int, linePerWay: Int, tagsReadAsync: Boolean) extends EvictionPolicy(ways, linePerWay, tagsReadAsync) {

  val stateWidth = 6

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

  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return state
  }

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

  override def get_invalidate_state(state: UInt, touch_way: UInt) : UInt = {
    return state
  }

  override def get_reset_state() : UInt = {
    return U(1, stateWidth bits)
  }

  override def get_replace_way(state: UInt) : UInt = {
    return state(log2Up(ways)-1 downto 0)
  }

  override val write = new Area{
    val valid = Bool()
    val address = UInt(log2Up(linePerWay) bits)
    val state = UInt(stateWidth bits)

    valid := False
    address.assignDontCare()
    state.assignDontCare()

    when (valid) {
      counter := state
    }
  }

  override val read = new Area{
    val load = new Area{
      val cmd = Flow(UInt(log2Up(linePerWay) bits))
      val rsp = counter
      KeepAttribute(rsp)
    }
    val store = new Area{
      val cmd = Flow(UInt(log2Up(linePerWay) bits))
      val rsp = counter
      KeepAttribute(rsp)
    }
  }

}


abstract class LRULogic(ways: Int, linePerWay: Int, tagsReadAsync: Boolean) extends EvictionPolicy(ways, linePerWay, tagsReadAsync) {

  val stateWidth = ((ways*ways)-ways)/2

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

  override val write = new Area{
    val valid = Bool()
    val address = UInt(log2Up(linePerWay) bits)
    val state = UInt(stateWidth bits)

    valid := False
    address.assignDontCare()
    state.assignDontCare()
  }

  override val read = new Area{
    val mem = Mem.fill(linePerWay)(UInt(stateWidth bits))
    mem.write(write.address, write.state, write.valid)
    val load = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = if(tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
      KeepAttribute(rsp)
    }
    val store = new Area{
      val cmd = Flow(mem.addressType)
      val rsp = if(tagsReadAsync) mem.readAsync(cmd.payload) else mem.readSync(cmd.payload, cmd.valid)
      KeepAttribute(rsp)
    }
  }

}


case class LRU(ways: Int, linePerWay: Int, tagsReadAsync: Boolean) extends LRULogic(ways, linePerWay, tagsReadAsync) {

  // Returns the next updated state upon touching a way
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way) 
  }

  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way)
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


case class FIFO(ways: Int, linePerWay: Int, tagsReadAsync: Boolean) extends LRULogic(ways, linePerWay, tagsReadAsync) {

  // Returns the next updates state upon touching a way
  override def get_next_state_hit(state: UInt, touch_way: UInt) : UInt = {
    return state
  }

  override def get_next_state_miss(state: UInt, touch_way: UInt) : UInt = {
    return upgrade_order_encoding(state, touch_way)
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
