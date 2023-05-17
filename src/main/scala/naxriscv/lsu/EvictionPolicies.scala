// See README.md for license details.

package naxriscv.lsu

import scala.collection.mutable.ArrayBuffer

import spinal.core._
import spinal.lib._


class LRULogic(ways: Int, state_width: Int) extends Area {
 
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

  // Updates the order encoding based on the way touched
  def update_order_encoding(state: UInt, touch_way: UInt) : UInt = {
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

}


case class AltLRU(ways: Int, state_width: Int) extends LRULogic(ways, state_width) {

  // Returns the next updates state upon touching a way
  def get_next_state(hit: Bool, state: UInt, touch_way: UInt) : UInt = {
    return update_order_encoding(state, touch_way) 
  }

  // Returns the encoding of state for reset mode
  def get_reset_state() : UInt = {
    return U(0, state_width bits)
  }

  // Compare state with mask for each state to determine the LRU way
  def get_replace_way(state: UInt) : UInt = {
    return get_least_recently_used(state)
  }

  // Returns the way to touch when a hit or miss takes place
  // In LRU, regardless of hit or miss, insertion takes place at the way touched
  def get_touch_way(hit: Bool, touch_way: UInt, victim: UInt) : UInt = {
    return touch_way
  }
  
}


case class AltFIFO(ways: Int, state_width: Int) extends LRULogic(ways, state_width) {

  // Returns the next updates state upon touching a way
  def get_next_state(hit: Bool, state: UInt, touch_way: UInt) : UInt = {
    return Mux(hit, state, update_order_encoding(state, touch_way))
  }

  // Returns the encoding of state for reset mode
  def get_reset_state() : UInt = {
    return U(0, state_width bits)
  }

  // Compare state with mask for each state to determine the LRU way
  def get_replace_way(state: UInt) : UInt = {
    return get_least_recently_used(state)
  }

  // Returns the way to touch when a hit or miss takes place
  // In FIFO, insertion takes place at the victim slot! Upon hits, nothing changes.
  def get_touch_way(hit: Bool, touch_way: UInt, victim: UInt) : UInt = {
    return Mux(hit, touch_way, victim)
  }

}
