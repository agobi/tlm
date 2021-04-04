package io.github.agobi.tlm

package object util {
  def choose(n: Int, k: Int): Long = {
    require(n >= 0 && k >= 0)
    if (k == 0) 1L
    else n.toLong * choose(n - 1, k - 1) / k
  }

}
