object Lab5 extends jsy.util.JsyApplication {
  import jsy.lab5.ast._
  import jsy.lab5._
  
  /*
   * CSCI 3155: Lab 5
   * Edward Zhu
   * 
   * Partner: Louis Badouuo
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace 'YourIdentiKey' in the object name above with your IdentiKey.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /*** Helper: mapFirst to DoWith ***/
  
  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](f: A => Option[DoWith[W,A]])(l: List[A]): DoWith[W,List[A]] = l match {
    case Nil => new DoWith( w => (w,Nil:List[A]))
    case h :: t => f(h) match {
      case None => mapFirstWith(f)(t).map(a => h :: a)
      case Some(withhp) => withhp.map(a => a :: t)
    }
  }

  /*** Casting ***/
  
  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    case (TNull, TObj(_)) => true
    case (_, _) if (t1 == t2) => true
    // Cast every field with foldleft and if they have castable types in each field
    // if keys are castable, return acc as true and recursively call caskok
    // if there is one false, it will return false, until all are true then it will
    // return true
    case (TObj(fields1), TObj(fields2)) => fields1.keys.foldLeft(true){
        (acc, key) => (fields1.get(key), fields2.get(key)) match {
          case (f1Typ, f2Typ) if(f1Typ == None || f2Typ == None) => acc && true 
          case (Some(f1Typ), Some(f2Typ)) => acc && castOk(f1Typ, f2Typ)
        }
      }
    case (TInterface(tvar, t1p), _) => castOk(typSubstitute(t1p, t1, tvar), t2)
    case (_, TInterface(tvar, t2p)) => castOk(t1, typSubstitute(t2p, t2, tvar))
    case _ => false
  }
  
  /*** Type Inference ***/

  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  } 
    
  def mut(m: PMode): Mutability = m match {
    case PName => MConst
    case PVar | PRef => MVar
  }
  
  def typeInfer(env: Map[String,(Mutability,Typ)], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) =>
        val (_, t) = env(x)
        t
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Eq|Ne, e1, e2) => typ(e1) match {
        case t1 if !hasFunctionTyp(t1) => typ(e2) match {
          case t2 if (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TBool
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(And|Or, e1, e2) => typ(e1) match {
        case TBool => typ(e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) => typ(e1); typ(e2)
      case If(e1, e2, e3) => typ(e1) match {
        case TBool =>
          val (t2, t3) = (typ(e2), typ(e3))
          if (t2 == t3) t2 else err(t3, e3)
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => TObj(fields map { case (f,t) => (f, typ(t)) })
      case GetField(e1, f) => typ(e1) match {
        case TObj(tfields) if (tfields.contains(f)) => tfields(f)
        case tgot => err(tgot, e1)
      } 
      
      case Function(p, paramse, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(paramse, rt)
            env + (f -> (MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with the parameters.
        val env2 = paramse match {
          // Left is just like None, Right is just like Some(x)
          //
          // If Left, not mutibility type for Const
          // m is acuumulator map
          // p._1 is string
          // p._2 is tuple with mutibility with type
          // m = parameter
          // p = tuple of mutibility and and type
          // assuming everything is const
          case Left(params) => env1 ++ params.foldLeft(Map[String,(Mutability,Typ)]())((m,p) => {
            m + (p._1 -> (MConst,p._2))
          })
          // checking the mode, and depending if it is name, var, ref, it will map
          // it imediately on env
          case Right((mode,x,t)) => mode match {
            case PName => env1 + (x -> (MConst, t))
            case PVar => env1 + (x -> (MVar, t))
            case PRef => env1 + (x -> (MVar, t))
          }
        }
        // Infer the type of the function body
        val t1 = typeInfer(env2, e1)
        tann foreach { rt => if (rt != t1) err(t1, e1) };
        TFunction(paramse, t1)
      }
      // types of the arguments are the types of the parameters
      case Call(e1, args) => typ(e1) match {
        case TFunction(Left(params), tret) if (params.length == args.length) => {
          (params, args).zipped.foreach {
            case ((key, value), args) => 
              val argTyp = typ(args)
              if (value == argTyp) value else err(argTyp, args)
          }
          tret
        }
        case tgot @ TFunction(Right((mode,_,tparam)), tret) => if (args.length == 0) err(tparam, e1) else mode match {
          case (PName | PVar) => if (tparam == typ(args.head)) tparam else err(tparam, e1)
          case PRef => if (isLExpr(args.head) && (tparam == typ(args.head))) tret else err(tparam, e1)
        }
        case tgot => err(tgot, e1)
      }
      
      /*** Fill-in more cases here. ***/
      case Null => TNull
      // seeing if current value can be casted onto a certain type t
      // mutation: 1 to true is NOT doable, but high and 1 is mutable
      // castable: casting type 1 int to type true bool is DOABLE 
      case Unary(Cast(t),e1) => if (castOk(typ(e1),t) && (t != TNull)) t else err(typ(e1),e1) 
      // TypeDecl
      // var x = 1 can be an int or a bool
      // it will put into the mutibility class of x, 1 = int first
      // then we recurse with e2
      case Decl(mut,x,e1,e2) => typeInfer(env + (x -> (mut,typ(e1))),e2)
      // TypeAssignVar & TypeAssignField
      // var x = 5
      // var x needs to of some T, 5 is already of type int
      // therefore because 5 mapped to int, we can map x to int
      // Field:
      // Every var in the object is some type
      // if we call the object then the exp type
      // Alex.age = 18
      // e1.f = e2      |||   18 is int, therefore Alex.age = int
      case Assign(e1,e2) => e1 match {
        case Var(x) => env.get(x) match {
          case Some((MVar, t)) => 
            if (t == typ(e2)) typeInfer(env + (x->(MVar, typ(e2))), e2) 
            else err(t, e2)
          case Some((MConst, t)) => err(t, e2)
          case _ => typeInfer(env + (x -> (MVar, typ(e2))), e2)
        }
        case GetField(x1, f) => typeInfer(env + (f -> (MConst, typ(e1))), e2)
        case _ => err(typ(e1),e2)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }
  
  /*** Small-Step Interpreter ***/
  
  /* Do the operation for an inequality. */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  


  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = substitute(e, esub, x)
    // allows static scoping to not change the global/free variables
    // renames bound variables
    val ep: Expr = avoidCapture(freeVars(esub),e)
    ep match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else e
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      // Trying to impliment lexical
      // checking 
      case Function(p, Left(params), retty, e1) => p match {
        // if there is some function name 
        // matching on wildcard of a function name
        // don't use the global variables
        // if parameter is the same name as the argument, don't need to substitute.
        case Some(p) => 
          val check = params.foldLeft(true){(acc, h) => if (x != h._1 && x != p) acc && true else acc && false}
        if (check) Function(Some(p), Left(params), retty, subst(e1)) else e
        case None => 
          val blah = params.foldLeft(true){(acc, h) => if (x != h._1) acc && true else acc && false}
          if(blah) Function(None, Left(params), retty, subst(e1)) else e
      }
      case Function(p, Right((mode, s, t)), retty, e1) => p match {
        case Some(z) =>
          val e1Sub = if ((x != s) && (x != z)) subst(e1) else e1
          Function(p, Right((mode, s, t)), retty, e1Sub)
        case None =>
          val e1Sub = if (x != s) subst(e1) else e1
          Function(None, Right((mode, s, t)), retty, e1Sub)
          
      }



      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fields) => Obj(fields map { case (fi,ei) => (fi, subst(ei)) })
      case GetField(e1, f) => GetField(subst(e1), f)
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))
      case InterfaceDecl(tvar, t, e1) => InterfaceDecl(tvar, t, subst(e1))
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    
    /*** Helpers for Call ***/
    
    def stepIfNotValue(e: Expr): Option[DoWith[Mem,Expr]] = if (isValue(e)) None else Some(step(e))
    
    /* Check whether or not the argument expression is ready to be applied. */
    def argApplyable(mode: PMode, arg: Expr): Boolean = mode match {
      case PVar => isValue(arg)
      case PName => true
      case PRef => isLValue(arg)
    }

    /*** Body ***/
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => for (m <- doget) yield { println(pretty(m, v1)); Undefined }
      case Unary(Neg, N(n1)) => doreturn( N(- n1) )
      case Unary(Not, B(b1)) => doreturn( B(! b1) )
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn( e2 )
      case Binary(Plus, S(s1), S(s2)) => doreturn( S(s1 + s2) )
      case Binary(Plus, N(n1), N(n2)) => doreturn( N(n1 + n2) )
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(inequalityVal(bop, v1, v2)) )
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 == v2) )
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 != v2) )
      case Binary(And, B(b1), e2) => doreturn( if (b1) e2 else B(false) )
      case Binary(Or, B(b1), e2) => doreturn( if (b1) B(true) else e2 )
      case Binary(Minus, N(n1), N(n2)) => doreturn( N(n1 - n2) )
      case Binary(Times, N(n1), N(n2)) => doreturn( N(n1 * n2) )
      case Binary(Div, N(n1), N(n2)) => doreturn( N(n1 / n2) )
      case If(B(b1), e2, e3) => doreturn( if (b1) e2 else e3 )
      // Do Object
      // If a not in domain of Memory, Allocate memory so that for all fields and values
      // Mainly overwritting the keys for a certain field depending on what we changed
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        Mem.alloc(Obj(fields)) map {(a:A) => a:Expr}

      // Do Get Field
      // getting certain map of memory
      // from memory get fields of objects
      // from fields get the value, if some value, return value
      // if none, then give stuck error
      case GetField(a @ A(_), f) =>
        doget.map((m: Mem) => m.get(a) match {
          case Some(Obj(fields)) => fields.get(f) match {
            case Some(f) => f
            case None => throw StuckError(e)
          }
          case _ => throw StuckError(e)
        }  
        )
        
      case Call(v1, args) if isValue(v1) =>
        def substfun(e1: Expr, p: Option[String]): Expr = p match {
          case None => e1
          case Some(x) => substitute(e1, v1, x)
        }
        (v1, args) match {
          case (Function(p, Left(params),_,e1), args) if (params.length == args.length) => {
            val e1p = (params, args).zipped.foldRight(e1) {
              (h, acc) => substitute(acc, h._2, h._1._1)
            }
            p match {
              case None => doreturn(e1p)
              case Some(x1) => doreturn(substitute(e1p, v1, x1))
            }
          }
          /*** Fill-in the DoCall cases, the SearchCall2, the SearchCallVar, the SearchCallRef  ***/
          case (Function(p, Right((PVar,x1,_)), _, e1),v2 :: Nil) if isValue(v2) => 
          Mem.alloc(v2) map {a => substfun(substitute(e1, Unary(Deref, a), x1), p)}
          
          case (Function(p, Right((PVar, x1, _)), _, e1), e2 :: Nil) =>
            step(e2) map {e2p => Call(v1, e2p :: Nil)}
          
          case (Function(p, Right((PRef,x1,_)), _, e1),lv2 :: Nil) if isLValue(lv2) => {
          val e1p = substitute(e1, lv2, x1)
          val e1pp = substfun(e1p, p)
          doreturn(e1pp)
          }
          
          case (Function(p, Right((PRef, x1, _)), _, e1), e2 :: Nil) =>
            step(e2) map {e2p => Call(v1, e2p :: Nil)}
          
          case (Function(p, Right((PName, x1, _)), _, e1), e2 :: Nil) =>
            doreturn(substfun(substitute(e1,e2,x1),p))
            
          case _ => throw StuckError(e)
        } 

      // If const, we cannot change the type, so we do a doreturn and substitute immediately
      // If var, we can change type, thus allocate new memory and map new type and expression into memory
      case Decl(MConst, x, v1, e2) if isValue(v1) => doreturn(substitute(e2,v1,x))
      case Decl(MVar, x, v1, e2) if isValue(v1) => Mem.alloc(v1) map { a => substitute(e2, Unary(Deref, a), x) }

      // Dereference the pointer of address of a in Memory and adds the address and value
      // to a separate memory 
      case Assign(Unary(Deref, addr @ A(_)), v) if isValue(v) =>
        for (_ <- domodify { (m: Mem) => (m+(addr,v)): Mem }) yield v
        
      // updating a field from an object
      case Assign(GetField(addr @ A(_), f), v) if isValue(v) =>
        for( _ <- domodify { (m: Mem) => m.get(addr) match {
          case Some(Obj(fields)) => m + (addr -> Obj(fields + (f-> (v))))
          case None => m
          case _ => throw StuckError(e)
          }
        }) yield v
        
      /*** Fill-in more Do cases here. ***/
      case Unary(Cast(_),e1) if (isValue(e1)) => doreturn(e1)
      
      case Unary(Deref,addr @ A(_)) => doget.map( (m: Mem) => m.apply(addr))
      /* Base Cases: Error Rules */
      /*** Fill-in cases here. ***/
        
      /* Inductive Cases: Search Rules */
      case Print(e1) =>
        for (e1p <- step(e1)) yield Print(e1p)
      case Unary(uop, e1) =>
        for (e1p <- step(e1)) yield Unary(uop, e1p)
      case Binary(bop, v1, e2) if isValue(v1) =>
        for (e2p <- step(e2)) yield Binary(bop, v1, e2p)
      case Binary(bop, e1, e2) =>
        for (e1p <- step(e1)) yield Binary(bop, e1p, e2)
      case If(e1, e2, e3) =>
        for (e1p <- step(e1)) yield If(e1p, e2, e3)
      // maps a parameter of e of i to the field of i
      case Obj(fields) => fields find { case (_, ei) => !isValue(ei) } match {
        case Some((fi,ei)) => for (eip <- step(ei)) yield Obj(fields + (fi -> eip))
        case None => throw StuckError(e)
      }

      // Rece
      case GetField(e1, f) => 
        if(e1==Null) throw new NullDereferenceError(e1)
        else for (e1p <- step(e1)) yield GetField(e1p, f)
      
      /*** Fill-in more Search cases here. ***/
      case Decl(mut, x, e1, e2) => {
        for (e1p <- step(e1)) yield Decl(mut, x, e1p, e2)
      }
      
      case Assign(e1, e2) => if (isLValue(e1)) {for (e2p <- step(e2)) yield Assign(e1, e2p)} else {for (e1p <-step(e1)) yield Assign(e1p, e2)}
        
      case Call(e1, e2) =>
        for (e1p <- step(e1)) yield Call(e1p, e2)
      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** External Interfaces ***/

  this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(500) // comment this out or set to None to not bound the number of steps.
  
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    } 
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("Type: " + pretty(t))
    }
    t
  }
  
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging. 
  
  case class TerminationError(e: Expr) extends Exception {
    override def toString = Parser.formatErrorMessage(e.pos, "TerminationError", "run out of steps in evaluating " + e)
  }
  
  def iterateStep(e: Expr): Expr = {
    require(closed(e), "not a closed expression: free variables: %s".format(freeVars(e)) )
    def loop(e: Expr, n: Int): DoWith[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e)
      else if (isValue(e)) doreturn( e )
      else {
        for {
          m <- doget[Mem]
          _ = if (debug) { println("Step %s:%n  %s%n  %s".format(n, m, e)) }
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield
        epp
      }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val (m,v) = loop(e, 0)(Mem.empty)
    if (debug) {
      println("Result:%n  %s%n  %s".format(m,v))
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(removeInterfaceDecl(jsy.lab5.Parser.parse(s)))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab5.Parser.parseFile(file)
      }} getOrElse {
        return
      }
      
    val exprlowered =
      handle(None: Option[Expr]) {Some{
        removeInterfaceDecl(expr)
      }} getOrElse {
        return
      }  
    
    handle() {
      val t = inferType(exprlowered)
    }
    
    handle() {
      val v1 = iterateStep(exprlowered)
      println(pretty(v1))
    }
  }
    
}