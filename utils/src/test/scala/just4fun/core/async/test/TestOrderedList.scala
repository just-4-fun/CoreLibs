package just4fun.core.async.test

import scala.util.Random
import just4fun.core.async.{OrderedItem, OrderedList}

object TestOrderedList extends App {
	var N = 0
	def nextId: Int = { N += 1; N }
	def rnd: Int = Random.nextInt(1000)

	val list = new ItemList
	list.add(AItem(nextId, rnd))
	prnList
	val i2 = list.add(AItem(nextId, rnd))
	prnList
	list.add(AItem(nextId, 500))
	prnList
	list.add(AItem(nextId, 1000))
	prnList
	list.add(AItem(nextId, 0))
	prnList
	list.add(AItem(nextId, rnd))
	prnList
	list.add(AItem(4, 998))
	prnList
	list.add(AItem(4, 999))
	prnList
	println("findFirst [id=4]:  "+list.findFirst(_.id == 4))
	println("findFirst [id=0]:  "+list.findFirst(_.id == 0))
	println("findAll [id=0]:  "+list.findAll(_.id == 0).mkString(", "))
	println("findAll [id=4]:  "+list.findAll(_.id == 4).mkString(", "))
	list.removeAll(_.id == 4)
	print("removeAll [id=4]: "); prnList
	list.removeFirst(_.id == 1)
	print("removeFirst [id=1]: "); prnList
	list.removeFirst(_ == i2)
	print("removeFirst [2]: "); prnList
	list.removeHead()
	print("removeHead: "); prnList
	list.removeHead()
	print("removeHead: "); prnList
	list.removeHead()
	print("removeHead: "); prnList
	list.clear()
	print("clear: "); prnList
	list.add(AItem(nextId, rnd))
	prnList
	list.clear()
	print("clear: "); prnList

	def prnList = println(s"[${list.toArray.map(i => s"${i.id}:${i.n}").mkString(", ")}]")
}
trait Item extends OrderedItem[Item] {
	val id: Any
	val n: Long
	override def compare(that: Item): Int = (n - that.n).toInt
}
case class AItem(id: Any, n: Long) extends Item
case class BItem(id: Any, n: Long) extends Item
class ItemList extends OrderedList[Item]

