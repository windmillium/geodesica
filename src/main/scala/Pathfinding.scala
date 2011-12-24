/**
* Can search for a shortest path between two nodes of type T that are subtypes of Searchable[T].
* Searchable[T] must provide several functions that are required for a-star.
*/
package net.geodesica

import scala.collection.immutable.TreeSet
import scala.collection.SortedSet
import scala.collection.mutable.HashSet

trait Searchable[T] {
  /** The cost of this particular searchable node. */
  def cost: Int
  /** The heuristic distance between this searchable and another.
  *  Must be admissible in the sense of a* search.
  */
  def heuristic(other: T): Int

  /** Return all the adjacent searchable nodes. */
  def adjacent: List[T]
}

class AStarSearch[T <: Searchable[T]] {
  class Node(o: T, from: Node, dest: Node) {
    val searchable = o
    /** Actual cost up to this point. */
    val actual: Int = if (from == null) 0 else from.actual + searchable.cost
    /** Estimate of the cost from here to the goal. */
    val estimate = if (dest == null) 0 else actual + searchable.heuristic(dest.searchable)
      /** Return a list of nodes that are adjacent to the current node.*/
    def adjacent = searchable.adjacent.map(s => new Node(s, this, dest))
    override def hashCode = o.hashCode
    override def equals(other: Any) = searchable.equals(other.asInstanceOf[Node].searchable)
    override def toString() = searchable.toString
    /** Get the path from the start to this node. */
    def toPath = toPathHelp.reverse
    /** Yields a list of the path up to this point, in reverse. */
    def toPathHelp: List[T] = if (from != null) searchable :: from.toPathHelp else List(searchable)
    }
  class EditablePriorityQueue {
    var open = new TreeSet[Node]()(Ordering.by((n: Node) => (n.estimate, n.searchable.hashCode)))
      val openSet = new HashSet[Node]
    def push(n: Node) = {
      openSet.add(n)
      open = open + n
    }
    def pop(): Node = {
      val n = open.head
      remove(n)
      return n
    }
    def nonEmpty() = open.nonEmpty
    def remove(n: Node) = {
      openSet.removeEntry(n)
      open = open - n
    }
    def update(n: Node): Boolean = {
      val incumbent = openSet.findEntry(n).getOrElse(null)
        if (incumbent == null) return false
      if (incumbent.actual > n.actual) {
        remove(incumbent)
        push(n)
      }
      return true
    }
  }

  def search(from: T, to: T): List[T] = {
    val closed = new HashSet[Node]
    val open = new EditablePriorityQueue()
    open.push(new Node(from, null, new Node(to, null, null)))

    while (open.nonEmpty) {
      val elem = open.pop()
        if (elem.searchable == to) {
        // If we've reached the goal node . . .
        return elem.toPath
      }
      closed.add(elem)
      // Iterate over the neighbors that are not in the closed set.
      for (n <- elem.adjacent.filter(n => !closed.contains(n))) {
        // Replace the incumbent if n is better.
        if (!open.update(n)) {
          // If no incumbent exists, simply add n.
          open.push(n)
        }
      }
    }
    return null
  }
}

