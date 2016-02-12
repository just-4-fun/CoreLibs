package just4fun.core.schemify

import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

case class MacroArgs(var dontWarnVal: Boolean = false, var dontWarnStub: Boolean = false, var printCode: Boolean = false)
object MacroArgsDefault extends MacroArgs()
object MacroArgs {
	val IX_DontWarnVal = 0
	val IX_DontWarnStub = 1
	val IX_PrintCode = 2
	val NAME_DontWarnVal = "dontWarnVal"
	val NAME_DontWarnStub = "dontWarnStub"
	val NAME_PrintCode = "printCode"
}

class Schemify(dontWarnVal: Boolean = false, dontWarnStub: Boolean = false, printCode: Boolean = false) extends StaticAnnotation {
	def macroTransform(annottees: Any*): Unit = macro SchemifyMacro.genSchema
}

class SchemifyMacro(val c: Context) {
	import c.universe._
	val ANYREFSYM = symbolOf[AnyRef]
	val SCHSYM = symbolOf[SchemaType[_]]
	val PROPTYPECLS = classOf[PropType[_]].getName
	//	val SCHCLS = classOf[BasicSchema[_]]
	val PROPTYPE = typeOf[Prop[_, _]]
	//	val STUBTYPE = typeOf[StubProp[_]]
	val PROP_x = "PROP"
	val STUB_x = "STUB"
	val GETTER_x = "getter"
	val SETTER_x = "setter"
	val NAME_x = "name"
	val SCHEMIFIED_x = "_schemified_"
	val INSTANCE_x = "create"
	val CONSTRUCT_x = "createFromValues"
	val COPYOBJECT_x = "copy"
	val AFTERSET_x = "afterValuesSet"
	val BEFOREGET_x = "beforeValuesGet"
	//	val GETFROMARRAY_x = "getValuesArray"
	//	val GETFROMMAP_x = "getValuesMap"
	//	val SETTOARRAY_x = "setValuesArray"
	//	val SETTOMAP_x = "setValuesMap"
	//	val ASBASE_x = "asBase"
	val IMPLIMAP_x = "impliMap"
	val OptionRx = """Option\[.*""".r
	
	
	/* AUTO SCHEMA */
	/** TODO is possible to publish new Schema as implicit value ? */
	def genAutoSchema[T: c.WeakTypeTag]: c.Tree = /*measureTime("GEN")*/ {
		val pearlT = weakTypeOf[T]
		val pearlSym = pearlT.typeSymbol
		val pearlNm = pearlSym.name.toString
		// collect Props from Pearl class based on accessors
		val propMap = mutable.LinkedHashMap[Symbol, PROPX]()
		pearlSym.asType.toType.members.sorted.withFilter(s => s.isTerm && s.asTerm.isAccessor).foreach { m =>
			val term = m.asTerm
			val field = term.accessed
			val p = propMap.get(field) match {
				case Some(p) => p
				case _ => val p = PROPX(propMap.size, field.name.toString.trim, field.info, term)
					propMap += field -> p
					p
			}
			if (term.isGetter) p.getter = Some(term.name.toString.trim)
			else if (term.isSetter) p.setter = Some(term.name.toString.trim)
		}
		// Generate new Schema instance code
		val fullName = s"${classOf[AutoSchema[_]].getName}[$pearlNm]"
		val props = propMap.values
		val buff = new StringBuilder
		buff.append(s"$PROPTYPECLS.$IMPLIMAP_x.getOrElseUpdate(classOf[${pearlT}], ")
		buff.append(s"new $fullName {\n")
		props.foreach { p => buff.append(s"val ${p.name} = $PROP_x[${p.typ}]\n") }
		buff.append(genCode(props, pearlNm, pearlT))
		buff.append("\n}")
		buff.append(s").asInstanceOf[$fullName]")
		val code = buff.toString()
		//		print(s"CODE\n$code")
		c.parse(code)
	}
	
	
	/* ANNOTATED SCHEMA */
	def genSchema(annottees: c.Tree*): c.Tree = /*measureTime("MAIN")*/ {
		val args = extractMacroArgs()
		val sch = annottees.take(1).view.collect { case ann: ImplDef => new SchemaInfo(ann, args) }.find(_.isSchema) match {
			case Some(v) => v.index = annottees.indexOf(v.tree); v
			case None => abort(s"@${classOf[Schemify].getSimpleName} annottee is expected to be a subclass of ${classOf[SchemaType[_]].getName}; actually: ${annottees.head.toString().takeWhile(ch => ch != '\n')} ... ")
		}
		val newTree = annottees.updated(sch.index, sch.newDef)
		if (sch.thisSym != null) {
			if (args.printCode) print(s"CODE\n$newTree")
		}
		q"..$newTree"
		//		q"..$annottees"
	}
	
	
	
	//	def toSymbol(typTree: c.Tree): Symbol = {
	//		extractType(typTree).symbol
	//	}
	//	def toType(typTree: c.Tree): Type = {
	//		extractType(typTree).tpe
	//	}
	//	def extractType(typTree: c.Tree): c.Tree = {
	//		val newTree = typTree match {
	//			case AppliedTypeTree(tp, args) =>
	//				try {
	//					val tree = q"val t1: $tp[..$args] = null"
	//					//					print(s"         EXTRACT TREE: $tree")
	//					c.typecheck(tree)
	//				}
	//				catch {
	//					case x: Throwable =>
	//						val tree = q"val t2: $tp[..${args.map(_.toString())}] = null"
	//						//						print(s"         EXTRACT TREE: $tree")
	//						c.typecheck(tree)
	//				}
	//			case tp => val tree = q"val t3: $tp = null.asInstanceOf[$tp]"
	//				//				print(s"         EXTRACT TREE: $tree")
	//				c.typecheck(tree)
	//		}
	//		//		val newTree = c.typecheck(tree)
	//		val q"..$_ val $_: ${tpt: c.Tree} = $_" = newTree
	//		tpt
	//	}
	
	def genCode(props: Iterable[PropBaseX], pearlTa: String, pearlT: Type, isConctrete: Boolean = true, genConstr: Boolean = true, args: MacroArgs = MacroArgsDefault): String = {
		val str = new StringBuilder
		str ++= s"override lazy protected[this] val $SCHEMIFIED_x = true\n"
		props.foreach {
			case p: PROPX => val nm = p.name
				str ++= s"""$nm.$NAME_x = "$nm"; """
				str ++= s"$nm.$GETTER_x = (obj) => obj.${p.getter.get}; "
				if (p.setter.nonEmpty) str ++= s"$nm.$SETTER_x = (obj, v) => obj.${p.setter.get}(v)\n"
				else str ++= s"$nm.$SETTER_x = (obj, v) => ()\n"
			case p: STUBX => val nm = p.name
				str ++= s"""$nm.$NAME_x = "$nm"\n"""
		}
		if (isConctrete) {
			val constrSym = constructorSym(pearlT)
			if (genConstr) {str ++= genConstructor(pearlT, constrSym)}
			str ++= genConstructFromVals(props, pearlT, constrSym)
			str ++= genCopyObject(props, pearlT, constrSym)
		} else str ++= "\n{}\n"
		//		str append genGetValuesArray(props, pearlTa)
		//		str append genGetValuesMap(props, pearlTa)
		//		str append genSetValuesArray(props, pearlTa)
		//		str append genSetValuesMap(props, pearlTa)
		//		print(s"CODE: $str}")
		str.toString()
	}
	def genConstructor(pearlT: Type, constrSym: MethodSymbol): String = {
		val newOp = if (pearlT.typeSymbol.asClass.isCaseClass) "" else "new "
		val text = new StringBuilder
		constrSym.paramLists.foreach { params =>
			val pps = params
			  .withFilter { p => !p.asTerm.isParamWithDefault && !p.asTerm.isImplicit }
			  .map(p => s"${p.name}=${defaultValue(p.info.toString)}")
			  .mkString(",")
			//			val pps = params.withFilter(!_.asTerm.isParamWithDefault).map(p => s"${p.name}=${defaultValue(p.info.toString)}").mkString(",")
			text.append("(")
			if (pps.length > 0) text.append(pps)
			text.append(")")
		}
		//		print(s"override def $INSTANCE_x = $newOp${pearlT.typeSymbol.fullName}$text\n")
		s"override def $INSTANCE_x = $newOp${pearlT.typeSymbol.fullName}$text\n"
	}
	def genConstructFromVals(props: Iterable[PropBaseX], pearlT: Type, constrSym: MethodSymbol): String = {
		val newOp = if (pearlT.typeSymbol.asClass.isCaseClass) "" else "new "
		val text1 = new StringBuilder
		constrSym.paramLists.foreach { params =>
			val pps = params.map { pm =>
				val nm = pm.name.toString
				props.find(p => p.name == nm && !p.stub) match {
					case Some(p) => p.fromConstructor = true
						s"$nm=$nm(vals(${p.index}))"
					case None if !pm.asTerm.isParamWithDefault => s"$nm=${defaultValue(pm.info.toString)}"
					case None => null
				}
			}.filter(_ != null).mkString(",")
			text1.append("(")
			if (pps.length > 0) text1.append(pps)
			text1.append(")")
		}
		val text2 = props.withFilter(p => !p.fromConstructor && !p.stub && p.setter.nonEmpty).map { p =>
			s"obj.${p.setter.get}(${p.name}(vals(${p.index})))"
		}.mkString("; ")
		s"override def $CONSTRUCT_x(vals: Array[_]) = {\nval obj = $newOp${pearlT.typeSymbol.fullName}$text1\n$text2\n$AFTERSET_x(obj); obj\n}\n"
	}
	def genCopyObject(props: Iterable[PropBaseX], pearlT: Type, constrSym: MethodSymbol): String = {
		val newOp = if (pearlT.typeSymbol.asClass.isCaseClass) "" else "new "
		val constr = s"$BEFOREGET_x(obj); val nobj = $newOp${pearlT.typeSymbol.fullName}"
		val deep = new StringBuilder(s"$constr")
		val flat = new StringBuilder(s"$constr")
		constrSym.paramLists.foreach { params =>
			deep.append("(")
			flat.append("(")
			val pps = params.map { pm =>
				val nm = pm.name.toString
				props.find(p => p.name == nm && !p.stub) match {
					case Some(p) => p.fromConstructor = true
						deep.append(s"$nm=$nm.copy(obj.$nm),")
						flat.append(s"$nm=obj.$nm,")
					case None if !pm.asTerm.isParamWithDefault => val s = s"$nm=${defaultValue(pm.info.toString)},"
						deep.append(s)
						flat.append(s)
					case None =>
				}
			}
			if (flat.charAt(flat.length - 1) == ',') {
				flat.deleteCharAt(flat.length - 1)
				deep.deleteCharAt(deep.length - 1)
			}
			deep.append(")")
			flat.append(")")
		}
		deep.append("\n")
		flat.append("\n")
		props.withFilter(p => !p.fromConstructor && !p.stub && p.setter.nonEmpty).map { p =>
			deep.append(s"nobj.${p.setter.get}(${p.name}.copy(obj.${p.name})); ")
			flat.append(s"nobj.${p.setter.get}(obj.${p.name}); ")
		}
		deep.append(s"$AFTERSET_x(nobj); nobj")
		flat.append(s"$AFTERSET_x(nobj); nobj")
		s"override def $COPYOBJECT_x(obj: $pearlT, deep: Boolean = false) = if (obj == null) obj else if (deep) {\n$deep\n} else {\n$flat\n}\n"
	}
	//	def genGetValuesArray(props: Iterable[PropBaseX], pearlT: String): String = {
	//		val str = new StringBuilder
	//		str.append(s"override def $GETFROMARRAY_x(obj: $pearlT) = if (obj == null) null else collection.mutable.ArrayBuffer(")
	//		props.foreach { p => str.append((if (p.index > 0) ", " else "") + (if (p.stub) "null" else s"${p.name}.$ASBASE_x(obj.${p.name})")) }
	//		str.append(")\n")
	//		str.toString()
	//	}
	//	def genSetValuesArray(props: Iterable[PropBaseX], pearlT: String): String = {
	//		val str = new StringBuilder
	//		str.append(s"override def $SETTOARRAY_x(obj: $pearlT, vals: Iterable[_]) = {\n")
	//		str.append("val itr = vals.iterator\n")
	//		//if (vals.iterator.hasNext) obj.v0 = v0(vals.iterator.next())
	//		props.foreach { p => str.append("if (itr.hasNext) ").append {
	//			if (p.stub || p.setter.isEmpty) "itr.next(); "
	//			else s"obj.${p.setter.get}(${p.name}(itr.next())); "
	//		}
	//		}
	//		str.append("}\n")
	//		//		print(s"setValsArray code\n${str}")
	//		str.toString()
	//	}
	
	//	def genGetValuesMap(props: Iterable[PropBaseX], pearlT: String): String = {
	//		val str = new StringBuilder
	//		str.append(s"override def $GETFROMMAP_x(obj: $pearlT) = if (obj == null) null else collection.mutable.HashMap[String, Any](")
	//		props.withFilter(p => !p.stub).foreach { p => str.append((if (p.index > 0) ", " else "") + s""""${p.name}" -> ${p.name}.$ASBASE_x(obj.${p.name})""") }
	//		str.append(")\n")
	//		str.toString()
	//	}
	//	def genSetValuesMap(props: Iterable[PropBaseX], pearlT: String): String = {
	//		val str = new StringBuilder
	//		str.append(s"override def $SETTOMAP_x(map: collection.Map[String, _]) = {\nval values = new Array[Any](propsSize)\n")
	//		//if (vals.iterator.hasNext) obj.v0 = v0(vals.iterator.next())
	//		props.withFilter(p => !p.stub && p.setter.nonEmpty).foreach { p => str.append {
	//			s"""map.get("${p.name}").foreach{v => values(${p.index}) = v}; """
	//		}
	//		}
	//		str.append(s"\n$NEWFROMVALS_x(values)\n}\n")
	//		str.toString()
	//	}
	
	def constructorSym(pearlT: Type) = {
		pearlT.members.find(s => s.isConstructor && s.asMethod.isPrimaryConstructor) match {
			case Some(c) => c.asMethod
			case None => throw new Exception(s"${pearlT} constructor not found.")
		}
	}
	def defaultValue(typ: String): String = typ match {
		case "String" => "null"
		case "Long" | "Int" | "Double" | "Float" | "Short" | "Byte" | "Char" => "0"
		case "Boolean" => "false"
		case OptionRx() => "None"
		case _ => "null"
	}
	def extractMacroArgs(): MacroArgs = {
		val args = new MacroArgs()
		import MacroArgs._
		import args._
		// DEFs
		case class MacroArg(index: Int, name: String, value: String)
		def toMacroArg(ix: Int, tree: c.Tree): MacroArg = tree match {
			case Literal(Constant(v)) => MacroArg(ix, null, v.toString)
			case AssignOrNamedArg(Ident(n), Literal(Constant(v))) => MacroArg(ix, n.toString, v.toString)
			case _ => MacroArg(ix, null, null)
		}
		def assignArgs(args: List[MacroArg]) = args.foreach {
			case MacroArg(ix, null, null) =>
			case MacroArg(ix, null, v) => ix match {
				case IX_DontWarnVal => dontWarnVal = v.toBoolean
				case IX_DontWarnStub => dontWarnStub = v.toBoolean
				case IX_PrintCode => printCode = v.toBoolean
				case _ =>
			}
			case MacroArg(ix, n, v) => n match {
				case NAME_DontWarnVal => dontWarnVal = v.toBoolean
				case NAME_DontWarnStub => dontWarnStub = v.toBoolean
				case NAME_PrintCode => printCode = v.toBoolean
				case _ =>
			}
		}
		// EXEC
		var ix = -1
		c.macroApplication match {
			case Apply(Select(Apply(_, margs), _), _) => assignArgs(margs.map { a => ix += 1; toMacroArg(ix, a) })
			case _ =>
		}
		args
	}
	def print(text: String) = c.info(c.enclosingPosition, text, false)
	def abort(text: String) = c.abort(c.enclosingPosition, text)
	def measureTime[T](tag: String, times: Int = 1)(code: => T): T = {
		var result: T = null.asInstanceOf[T]
		var ns = 0L
		if (times == 1) {
			val t0 = System.nanoTime()
			result = code
			ns = System.nanoTime() - t0
			println(s"$tag TIME= $ns")
		}
		else {
			val range = 0 until times
			val t0 = System.nanoTime()
			for (_ <- range) result = code
			ns = System.nanoTime() - t0
			print(s"$tag TIME (ms per $times; ns per 1)=  ${(ns / 1000000f).toInt};   ${ns / times}")
		}
		result
	}
	
	
	
	
	
	/* CLASSES */
	
	class SchemaInfo(val tree: ImplDef, args: MacroArgs) {
		var pearlT: c.Tree = _
		var pearlTyp: Type = _
		var index: Int = 0
		var paramIndex: Int = 0
		val props = ArrayBuffer[PropBaseX]()
		val thisSym: Symbol = try {
			c.typecheck(tree).symbol match {
				case ms: ModuleSymbol => ms.asModule.moduleClass
				case s => s
			}
		} catch {case e: Throwable => /*print(s"Error $e");*/ null}
		var (hierarchies, parentsList) = thisSym match {
			case null => (Nil, Nil)
			//			case null => val parentSym = toSymbol(tree.impl.parents.head)
			//				collectParents(parentSym, Nil).reverse // head is Schema; last is parentSym
			case _ => collectHierarchies()
		}
		if (thisSym != null && parentsList.nonEmpty) {
			collectProps()
			detectPearl()
			scanPearl()
			//			print(s"PEARL thisSym= $thisSym;  Ta= $pearlT;  Typ= $pearlTyp;${if (props.nonEmpty) "\nPROPS " + props.mkString(", ") else ""}\nCHAIN\n${parentsList.map(p => symInfo(p)).mkString("  ->  ")}")
		}
		
		
		def newDef: c.Tree = if (thisSym == null) tree
		else {
			val code = genCode(props, pearlT.toString(), pearlTyp, isConcrete, genConstructor, args)
			val q"..$lines" = c.parse(code)
			tree match {
				case ClassDef(mods, nm, params, Template(parents, self, body)) => ClassDef(mods, nm, params, Template(parents, self, body ::: lines))
				case ModuleDef(mods, nm, Template(parents, self, body)) => ModuleDef(mods, nm, Template(parents, self, body ::: lines))
			}
		}
		def collectHierarchies(): (List[List[Symbol]], List[Symbol]) = {
			var linearParents =thisSym.asType.toType.baseClasses
			def directParents(s: Symbol): List[Symbol] = {
				var directParents = s.asType.toType.baseClasses.tail
//				print(s">>> [$s]:  All PARENTS>> $directParents")
				directParents.foreach { par =>
					par.asType.toType.baseClasses.tail.foreach { c =>
						directParents = directParents.filterNot(_ == c)
					}
				}
//				print(s">>> [$s]:  Direct PARENTS>> $directParents")
				directParents
			}
			def collectParents(s: Symbol, top: Boolean = false): List[List[Symbol]] = s match {
				case SCHSYM => //print(s"<<<  [$s]:   LEAF TREE");
					List(s :: Nil)
				case _ => directParents(s).map(collectParents(_)).filter(_.nonEmpty) match {
					case Nil => //print(s"<<< [$s]:   EMPTY TREE");
						linearParents = linearParents.filter(_ != s)
						Nil
					case schemaTrees => //print(s"<<< [$s]:   TREES >> ${schemaTrees.map(s :: _.flatten)}");
						schemaTrees.flatten.map(s :: _)
				}
			}
			//
			val trees = collectParents(thisSym, true).reverse.distinct.map(_.reverse)
			val parentsLine = linearParents.reverse.dropWhile(_ != SCHSYM)
//			print(s"<<<<<<:   HIERARHIES> ${trees}")
//			print(s"<<<<<<:   LINEARPARENTS> ${parentsLine}}")
			(trees, parentsLine)
		}
//		def collectHierarchies2(): (List[List[Symbol]], List[Symbol]) = {
//			var hierarchies = List[List[Symbol]]()
//			def directParents(s: Symbol): List[Symbol] = {
//				var directParents = s.asType.toType.baseClasses.tail
//				//				print(s">>> [$s]:  All PARENTS>> $directParents")
//				directParents.foreach { par =>
//					par.asType.toType.baseClasses.tail.foreach { c =>
//						directParents = directParents.filterNot(_ == c)
//					}
//				}
//				//				print(s">>> [$s]:  Direct PARENTS>> $directParents")
//				directParents
//			}
//			def collectParents(s: Symbol, top: Boolean = false): List[Symbol] = s match {
//				case SCHSYM => /* print(s"<<<  [$s]:   RET LEAF"); */ s :: Nil
//				case _ => directParents(s).map(collectParents(_)).filter(_.nonEmpty) match {
//					case Nil => /*print(s"<<< [$s]:   EMPTY trees"); */ Nil
//					case parentTrees => //print(s"<<< [$s]:   TREES >> $parentTrees");
//						if (top) {
//							hierarchies = parentTrees.map(list => (s :: list).reverse).reverse
//							//							print(s"<<<<<<<  [$s]:   HIERARHIES >> $hierarchies");
//						}
//						val flatParents = s :: parentTrees.flatten
//						//						print(s"<<< [$s]:   FLATTEN >> $flatParents");
//						flatParents
//				}
//			}
//			//
//			val parentsList = collectParents(thisSym, true).reverse.distinct
//			(hierarchies, parentsList)
//		}
		def detectPearl() = {
			var typ: Type = null
			hierarchies.foreach { h =>
				//				print(s"CHK H >> $h")
				paramIndex = 0
				checkPair(h)
				if (typ != null && !(pearlTyp =:= typ)) abort(s"Schema ancestors have different type parameters: $typ and $pearlTyp.")
				else typ = pearlTyp
			}
			//			if (pearlTyp == null) pearlTyp = toType(pearlT)
		}
		def symInfo(s: Symbol) = {
			val tp = s.asType.toType //args2= ${tp.dealias.typeArgs}// sig= ${s.typeSignature}
			s"$s [${tp.typeArgs.mkString(", ")}]"
		}
		def checkPair(parents: List[Symbol]): Unit = parents match {
			case up :: sub :: _ if paramIndex >= 0 =>
				val upCls = up.asClass
				if (upCls.typeParams.nonEmpty) {
					val subTyp = sub.asType.toType
					val subCls = sub.asClass
					val upParam = upCls.typeParams(paramIndex)
					val upParamT = upParam.asType.toType
					val seenT = upParamT.asSeenFrom(subTyp, upCls)
					pearlT = q"$seenT"
					pearlTyp = subCls.typeParams.collectFirst {
						case param if param.asType.toType =:= seenT =>
							paramIndex = subCls.typeParams.indexOf(param)
							param.infoIn(subTyp).erasure
					} match {
						case Some(p) => p
						case None => paramIndex = -1; seenT
					}
					//										print(s"PAIR SUB= $subCls;  UP= $upCls;  upParams[$paramIndex]= $upParamT;  pearlT= $pearlT;  pearlTyp= $pearlTyp")
					checkPair(parents.tail)
				}
			//			case up :: Nil if thisSym == null && paramIndex >= 0 => // TODO  Last pair if thisType glitches
			//				tree.impl.parents.head match {
			//					case AppliedTypeTree(nm, args) => pearlT = args(paramIndex)
			//						var typ: Type = null
			//						if (!isSingleton) {
			//							tree.asInstanceOf[ClassDef].tparams.collectFirst {
			//								case param if param.name.toString == pearlT.toString() => param.rhs match {
			//									case TypeBoundsTree(lo, hi) if hi.nonEmpty => typ = toType(hi)
			//									case _ => typ = typeOf[Any]
			//								}
			//							}
			//						}
			//						pearlTyp = if (typ == null) toType(pearlT) else typ
			//					case _ =>
			//				}
			//			//		print(s"PAIR LAST  pearlT= $pearlT;  pearlTyp= $pearlTyp")
			case _ =>
		}
		//		def thisType(): c.Tree = {
		//			val nm = TypeName(name.toString)
		//			val params = tree match {
		//				case t: ClassDef => if (t.tparams.isEmpty) None else Some(t.tparams)
		//				case _ => None
		//			}
		//			val newTree = params match {
		//				case None =>
		//					val tree = if (isSingleton) q"val t1 = ${TermName(name.toString)}"
		//					else q"val t1: $nm = null.asInstanceOf[$nm]"
		//					//					print(s"          EXTRACT THIS TREE $tree")
		//					c.typecheck(tree)
		//				case Some(params) =>
		//					//					val tree = q"val t2: $nm[..$params] = null"
		//					val tree = q"val t3: $nm[..${params.map(p => p.name)}] = null"
		//					//					print(s"          EXTRACT THIS TREE $tree")
		//					try {c.typecheck(tree)}
		//					catch {
		//						case x: TypecheckException =>
		//							val tree = q"val t2: $nm[..$params] = null"
		//							//						val tree = q"val t3: $nm[..${params.map(_.toString()) }] = null"
		//							//							print(s"          EXTRACT THIS TREE $tree")
		//							c.typecheck(tree)
		//					}
		//			}
		//			val q"..$_ val $_: ${tpt: c.Tree} = $_" = newTree
		//			tpt
		//		}
		
		def collectProps() = parentsList.tail.foreach { parent =>
			parent.asType.toType.decls.sorted.withFilter(d => d.isTerm).withFilter { d =>
				val t = d.asTerm
				(t.isVal || t.isVar) && t.info <:< PROPTYPE
			}.foreach { decl =>
				val propSym = decl.asTerm
				val propTyp = propSym.info
				//				print(s"PropSym= ${propSym};  typ= [${propTyp.typeArgs(1)}]; Eq ? ${propTyp.typeArgs(1).toString == "Null"}")
				if (propTyp.typeArgs(1).toString == "Null") props += STUBX(props.length, propSym.name.toString.trim, propSym)
				else props += PROPX(props.length, propSym.name.toString.trim, propTyp.typeArgs(1), propSym)
				//				if (propTyp <:< STUBTYPE) props += STUBX(props.length, propSym.name.toString.trim, propSym)
				//				else {
				//					print(s"PropSym= ${propSym};  typ= [${propTyp.typeArgs(1)}]; Eq ? ${propTyp.typeArgs(1).toString == "Null"}")
				//					props += PROPX(props.length, propSym.name.toString.trim, propTyp.typeArgs(1), propSym)
				//				}
				//info(s"DECL  $ps;  name= ${ps.name };  type= ${ps.info };  isVal= ${ps.isVal };  isVar= ${ps.isVar };  params= ${ps.info.typeParams };  args= ${ps.info.typeArgs };  parLists= ${ps.info.paramLists };  EQ? ${ps.info.typeSymbol == PROPSYM }")
			}
		}
		//		def scanSchema() = if (thisSym != null) {
		//			hasConstructor = thisSym.asType.toType.decl(TermName(INST_x)) != NoSymbol
		//		}
		//		else body.foreach {
		//			case ValDef(mods, name, tpt, rhs) =>
		//				rhs match {
		//					case TypeApply(Ident(TermName(PROP_x)), tp :: _) => //print(s" $name = PROP[$tp]")
		//						props += PROPX(props.length, name.toString, tp) // PROP[Int]
		//					case Apply(Select(TypeApply(Ident(TermName(PROP_x)), tp :: _), nm), fargs) =>
		//						props += PROPX(props.length, name.toString, tp) // PROP[Int].config(p=>())
		//					case Ident(TermName(STUB_x)) => //print(s" $name = STUB")
		//						props += STUBX(props.length, name.toString)
		//					case _ => //print(s"???   $name: $tpt = $rhs;  cls= ${rhs.getClass.getSimpleName}")
		//				}
		//			case DefDef(mods, TermName(INST_x), tparams, vparamss, tpt, rhs) => hasConstructor = true
		//			case _ => //print(s"??? $tree")
		//		}
		
		def scanPearl() = {
			val err = new StringBuilder
			val warn = new StringBuilder
			val constrSym = constructorSym(pearlTyp)
			constrSym.paramLists.foreach { params =>
				val pps = params.foreach { pm =>
					val nm = pm.name.toString
					props.find(p => p.name == nm) match {case Some(p) => p.fromConstructor = true; case None =>}
				}
			}
			//			val msg = new StringBuilder
			// DEFs
			def findGetter(p: PropBaseX): Option[String] = findMember(p.name) match {
				case NoSymbol => None
				case s => val sTyp = resultType(s, s.overrides) // can override var in subclass constructor as private
					if (p.stub || sTyp =:= p.typ) Some(s.name.toString.trim)
					else {
						err.append(s"Schema field '${p.name}' expected type: Prop[$sTyp]; actual:  Prop[${p.typ}].\n")
						None
					}
			}
			def resultType(s: Symbol, overrides: List[Symbol]): Type = if (overrides.isEmpty) s.info.finalResultType else overrides.head.info.finalResultType
			def findSetter(p: PropBaseX): Option[String] = findMember(p.name + "_$eq") match {
				case NoSymbol => None
				case s => Some(s.name.toString.trim)
			}
			def findMember(nm: String): Symbol = pearlTyp.member(TermName(nm))
			// EXEC
			props.foreach { p =>
				p.getter = findGetter(p)
				p.setter = findSetter(p)
				p match {
					case p: PROPX =>
						if (p.getter.isEmpty && p.setter.isEmpty) err.append(s"Prop '${p.name}' exists in schema but does not exist in $pearlTyp class.\n")
						else if (p.getter.isEmpty) err.append(s"Prop '${p.name}' should have read accessor.\n")
						else if (p.setter.isEmpty && !p.fromConstructor && !args.dontWarnVal) warn.append(s"Prop '${p.name}' does not have setter.\n ")
					//						else msg.append(s"Prop ${p.name};  getter= ${p.getter};  setter= ${p.setter};   ")
					case p: STUBX =>
						if (p.getter.nonEmpty || p.setter.nonEmpty) if (!args.dontWarnStub) warn.append(s"Prop '${p.name}' is stub in schema so it's not required in $pearlTyp class.\n")
					//						else msg.append(s"STUB ${p.name};  has no getter/setter;   ")
				}
			}
			if (warn.nonEmpty) print(s"Props warnings:\n$warn")
			if (err.nonEmpty) abort(err.toString())
		}
		
		
		lazy val name: Name = tree.name
		lazy val body: List[c.Tree] = tree.impl.body
		lazy val isSchema = thisSym == null || parentsList.nonEmpty
		lazy val isConcrete = pearlT.tpe =:= pearlTyp
		lazy val genConstructor = thisSym.asType.toType.member(TermName(INSTANCE_x)) match {
			case NoSymbol => false // error
			case s if isConcrete && s.overrides.size <= 1 => true
			case _ => false
		}
		lazy val isCompanion = thisSym.companion match {
			case NoSymbol => false
			case com => thisSym.isModuleClass && pearlTyp.typeSymbol == com
		}
		lazy val isSingleton = tree match {case t: ClassDef => false; case _ => true}
		
	}
	
	
	
	
	
	
	trait PropBaseX {
		val index: Int
		val name: String
		val typ: Type
		val term: TermSymbol
		val stub: Boolean
		var getter: Option[String] = None
		var setter: Option[String] = None
		var fromConstructor: Boolean = false
	}
	case class PROPX(index: Int, name: String, rawTyp: Any, term: TermSymbol) extends PropBaseX {
		val stub = false
		val typ = rawTyp match {
			//			case nm: c.Tree => toType(nm)
			case tp: Type => tp
			//			case nm => toType(Ident(TermName(nm.toString)))
		}
		//		print(s"PROP= $name;  rawT= $rawTyp;  typ= $typ")
	}
	case class STUBX(index: Int, name: String, term: TermSymbol) extends PropBaseX {
		val typ = typeOf[Null]
		val stub = true
	}
	
}
