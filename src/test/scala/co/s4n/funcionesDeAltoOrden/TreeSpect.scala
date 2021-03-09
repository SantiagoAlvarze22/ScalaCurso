
package co.s4n.funcionesDeAltoOrden

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class treeSpec extends AnyFlatSpec with Matchers {
  "size(Leaf(10))"  should "es 1" in {
    tree.size(Leaf(1)) shouldEqual 1
  }

}