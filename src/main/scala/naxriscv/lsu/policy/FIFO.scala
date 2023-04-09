package naxriscv.lsu.policy

import spinal.core._
import spinal.lib._
import spinal.core.sim._

// Hardware definition
case class FIFO(ways: Int, state_width: Int) extends Component {

  /**
   * @brief Function creating a mask literal with 'don't cares' as a base and zeroes or ones where specified by `zero_indices` and `one_indices` respectively.
   */
  def get_mask(zero_indices: Array[Int], one_indices: Array[Int]) : MaskedLiteral = {
    var mask = Array.tabulate(state_width)(n => "-")
    //mask = zero_indices.foldLeft(mask)((s, i) => s.updated(i, '0'))
    for (index <- zero_indices)
      mask(index) = "0"
    //mask = one_indices.foldLeft(mask)((s, i) => s.updated(i, '1'))
    for (index <- one_indices)
      mask(index) = "1"
    mask = mask.reverse
    return MaskedLiteral(mask.mkString)
  }

  // Preprocessing data structures
  //// Upper triangular matrix with index where 0s must be inserted. Used to generate zeros masks
  var i = state_width-1
  val zero_mask_indices = Array.tabulate(ways)(n => Array.tabulate(n)(i => 0))
  for (row <- ways-1 to 0 by -1) {
    for (col <- 0 until zero_mask_indices(row).size) {
      zero_mask_indices(row)(col) = i
      i -= 1
    }
  }
  ////
  i = state_width-1
  val one_mask_indices = Array.tabulate(ways)(n => Array.tabulate(ways-n-1)(i => 0))
  for (col <- 0 until ways-1) {
    for (row <- ways-2-col to 0 by -1) {
      one_mask_indices(row)(col) = i
      i -= 1
    }
  }

  def get_next_state_on_lookup(state: Bits, touched: Bits) : Bits = {
    return state
  }

  // Determine the next updated state on lookup
  def get_next_state_on_load(state: Bits, touched: Bits) : Bits = {
    // Generate candidate states
    val candidate_next_states = Array.tabulate(ways)(n => Bits(state_width bits))
    for (way <- 0 until ways) {
      val candidate_next_states_bits = Bits(state_width bits)
      candidate_next_states_bits := state
      for (index <- one_mask_indices(way))
        candidate_next_states_bits(index) := True
      for (index <- zero_mask_indices(way))
        candidate_next_states_bits(index) := False
      candidate_next_states(way) := candidate_next_states_bits
    }
    return MuxOH(touched, candidate_next_states)
  }

  // Determine the next updated state on invalidate
  def get_next_state_on_invalidate(state: Bits, touched: Bits) : Bits = {
    // Generate candidate states
    val candidate_invalidated_states = Array.tabulate(ways)(n => Bits(state_width bits))
    for (way <- 0 until ways) {
      val candidate_invalidated_states_bits = Bits(state_width bits)
      candidate_invalidated_states_bits := state
      for (index <- one_mask_indices(way))
        candidate_invalidated_states_bits(index) := False
      for (index <- zero_mask_indices(way))
        candidate_invalidated_states_bits(index) := True
      candidate_invalidated_states(way) := candidate_invalidated_states_bits
    }
    return MuxOH(touched, candidate_invalidated_states)
  }

  // Compare state with mask for each state to determine the LRU way
  def get_victim(state: Bits) : Bits = {
    val least_recently_used = Bits(ways bits)
    for (way <- 0 until ways)
      least_recently_used(way) := (state === get_mask(one_mask_indices(way), zero_mask_indices(way)))
    return least_recently_used
  }

  def get_next_state_on_reset() : Bits = {
    return B(0)
  }

}
