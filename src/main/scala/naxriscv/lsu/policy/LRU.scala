package naxriscv.lsu.policy

import spinal.core._
import spinal.lib._

// Hardware definition
case class LRU(ways: Int, state_width: Int) extends Component {

  /**
   * @brief Function creating a mask literal with 'don't cares' as a base and zeroes or ones where specified by `zero_indices` and `one_indices` respectively.
   */
  def get_mask(zero_indices: Array[Int], one_indices: Array[Int]) : MaskedLiteral = {
    var mask = Array.tabulate(state_width)(n => "-")
    for (index <- zero_indices)
      mask(index) = "0"
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

  // Returns the next updates state upon touching a way
  def touch(state: Bits, touched: UInt) : Bits = {
    // Generate candidate states
    val candidate_next_states = Array.tabulate(ways)(n => Bits(state_width bits))
    for (way <- 0 until ways) {
      candidate_next_states(way) := state
      for (index <- one_mask_indices(way))
        candidate_next_states(way)(index) := True
      for (index <- zero_mask_indices(way))
        candidate_next_states(way)(index) := False
    }
    return touched.muxList(for (way <- 0 until ways) yield (way, candidate_next_states(way)))
  }

  // Returns the next updated state upon eviction
  // For LRU, assuming 'touched' indicates the line to evict, 
  // eviction is the same as touching the way.
  def evict(state: Bits, touched: UInt) : Bits = {
    return touch(state, touched)
  }

  // Compare state with mask for each state to determine the LRU way
  def victim(state: Bits) : UInt = {
    val least_recently_used = Bits(ways bits)
    for (way <- 0 until ways)
      least_recently_used(way) := (state === get_mask(one_mask_indices(way), zero_mask_indices(way)))
    return OHToUInt(least_recently_used)
  }

  def reset() : Bits = {
    return B(0)
  }

}
