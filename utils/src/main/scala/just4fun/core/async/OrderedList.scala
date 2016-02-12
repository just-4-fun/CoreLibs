package just4fun.core.async

import scala.collection.mutable.ArrayBuffer

/* ITEM */
trait OrderedItem[THIS <: OrderedItem[THIS]] extends Ordered[THIS] {
	this: THIS =>
	var next: THIS = _ //null.asInstanceOf[THIS]
}

/* LIST */
class OrderedList[T <: OrderedItem[T]] {
	protected[this] var first: T = _ //null.asInstanceOf[T]

	def head: T = first
	def isEmpty = first == null
	def nonEmpty = first != null
	def clear() = first = null.asInstanceOf[T]
	def add(v: T): T = {
		if (first == null || v < first) {
			v.next = first
			first = v
		}
		else {
			var curr = first
			while (curr.next != null && curr.next <= v) curr = curr.next
			v.next = curr.next
			curr.next = v
		}
		v
	}
	def contains(is: T => Boolean): Boolean = {
		findFirst(is) != null
	}
	def findFirst(is: T => Boolean): T = {
		var curr = first
		var prev = null.asInstanceOf[T]
		while (curr != null) {
			if (is(curr)) return curr
			prev = curr
			curr = curr.next
		}
		null.asInstanceOf[T]
	}
	def findAll(is: T => Boolean): ArrayBuffer[T] = {
		val buff = ArrayBuffer[T]()
		var curr = first
		var prev = null.asInstanceOf[T]
		while (curr != null) {
			if (is(curr)) buff += curr
			prev = curr
			curr = curr.next
		}
		buff
	}
	def removeHead(): T = {
		if (first == null) null.asInstanceOf[T]
		else {
			val v = first
			first = first.next
			v
		}
	}
	def removeFirst(is: T => Boolean): T = {
		var curr = first
		var prev = null.asInstanceOf[T]
		while (curr != null) {
			if (is(curr)) {
				if (prev == null) first = first.next
				else prev.next = curr.next
				return curr
			}
			prev = curr
			curr = curr.next
		}
		null.asInstanceOf[T]
	}
	def removeAll(is: T => Boolean): Unit = {
		var curr = first
		var prev = null.asInstanceOf[T]
		while (curr != null) {
			if (is(curr)) {
				if (prev == null) first = first.next
				else prev.next = curr.next
			}
			else prev = curr
			curr = curr.next
		}
	}
	def toArray: ArrayBuffer[T] = {
		var list = ArrayBuffer[T]()
		var curr = first
		while (curr != null) {
			list += curr
			curr = curr.next
		}
		list
	}
	def size: Int = {
		var s = 0
		var curr = first
		while (curr != null) {s += 1; curr = curr.next}
		s
	}
}
