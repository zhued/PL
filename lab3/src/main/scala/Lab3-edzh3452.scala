object Lab3 extends jsy.util.JsyApplication {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * Edward Zhu
   * 
   * Partner: Alex Campbell
   * Collaborators: Steven Tang
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
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) if (n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN => false
      case N(_) => true
      case B(b) => b
      case Undefined => false
      case S("") => false
      case S(_) => true
      case Function(_, _, _) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(_, _, _) => "function"
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
	require(isValue(v1))
	require(isValue(v2))
	require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case _ =>
        val (n1, n2) = (toNumber(v1), toNumber(v2))
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * This code is a reference implementation of JavaScripty without
   * strings and functions (i.e., Lab 2).  You are to welcome to
   * replace it with your code from Lab 2.
   */
  def eval(env: Env, e: Expr): Expr = {
    def eToN(e: Expr): Double = toNumber(eval(env, e))
    def eToB(e: Expr): Boolean = toBoolean(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      case Var(x) => get(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      case Unary(Neg, e1) => N(- eToN(e1))
      case Unary(Not, e1) => B(! eToB(e1))
      
      case Binary(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (S(s1), v2) => S(s1 + toStr(v2))
        case (v1, S(s2)) => S(toStr(v1) + s2)
        case (v1, v2) => N(toNumber(v1) + toNumber(v2))
      }      
      case Binary(Minus, e1, e2) => N(eToN(e1) - eToN(e2))
      case Binary(Times, e1, e2) => N(eToN(e1) * eToN(e2))
      case Binary(Div, e1, e2) => N(eToN(e1) / eToN(e2))
      
      case Binary(bop @ (Eq | Ne), e1, e2) =>  (bop, eToVal(e1), eToVal(e2)) match{
        // Equal cases, for wrong function
        case (Eq, Function(_, _, _), e2) => throw new DynamicTypeError(e)
        case (Eq, e1, Function(_, _, _)) => throw new DynamicTypeError(e)
        case (Eq, e1, e2) => B(toNumber(e1) == toNumber(e2))
        // Not Equal cases, also for wrong functions
        case (Ne, Function(_, _, _), e2) => throw new DynamicTypeError(e)
        case (Ne, e1, Function(_, _, _)) => throw new DynamicTypeError(e)
        case (Ne, e1, e2) => B(toNumber(e1) != toNumber(e2))
        case _ => throw new UnsupportedOperationException
        //**************
      }
      
      case Binary(bop @ (Lt|Le|Gt|Ge), e1, e2) => B(inequalityVal(bop, eToVal(e1), eToVal(e2)))
      
      case Binary(And, e1, e2) => 
        val v1 = eToVal(e1)
        if (toBoolean(v1)) eToVal(e2) else v1
      case Binary(Or, e1, e2) =>
        val v1 = eToVal(e1)
        if (toBoolean(v1)) v1 else eToVal(e2)
      
      case Binary(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      
      case If(e1, e2, e3) => if (eToB(e1)) eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      //*****************
      case Call(e1, e2) => eToVal(e1) match {
        // Recursive
        // If there is Some function name then store in env before running
        //   
        // None and some are constructors for option string

        //evaluates a call of form e1, e2. e1 = function type & if not func type then evaluate, e2 = parameter
        //make a call, recursive func: func name, parameter, and expression that's the body of the function.
        //go into expression body, replace func names with expression body. 
        //name of parameter (x) and extend env with e2. 
        //extend env so the name of func maps to func body. 
        //take that env (within it) so that x extends to whatever e2 is, which is the param
        //call eval on the body of func.
        //dynamic scoping: extension (x maps to e2)
        case Function(Some(x1), x2, eprime) => {
          val env1=extend(env, x1, eToVal(e1))
          eval(extend(env1, x2, eToVal(e2)), eprime)
        }
        // Non-recursive
        case Function(None, x1, eprime) => {
          eval(extend(env, x1, eToVal(e2)), eprime)
        }
        case _ => throw new DynamicTypeError(e)
      }
      //******************
      case _ => throw new UnsupportedOperationException
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */
  //one small step, ex:constdecl...change expression
  //small-step interpreter deterministic because
  //each time you take a func, you simplify it
  //each time you get a func, one case where it'll fall to: 
  //pattern matching in interpreter, it'll fall under one case.
  //since one case, and they're in a specific order, it's deterministic. 

  //anything in the interpreter that can be reduced 
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, v, x)
    /* Body */
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      //***********************************
      
      case Var(y) => if(x == y) v else e

      //Unary and Binary
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2)) 
      
      // Stored values
      // r = arguments
      // f = expression
      case Function(name, r, f) => 
        // if it has an argument (5) or name f(), then retun the function, else recusively substitute f until it is found out
        if (r == x || name == Some(x)) 
          Function(name, r, f) 
        // 
        else 
          Function(name, r, subst(f))

      // e1 will have to be stripped down to find a v1 for it
      // 
      case ConstDecl(y, e1, e2) => ConstDecl(y, subst(e1), if (x == y) e2 else subst(e2))
      
      // Call/If, just subst all expressions
      case Call(e1, e2) => Call(subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))

      //***********************************
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      
        // **************** Your cases here
      //Unary Cases with a Value
      case Unary(uop @ (Neg | Not), v) if (isValue(v)) => uop match{
        //DoNeg
        case Neg => N(-toNumber(v))
        //DoNot
        case Not => B(!toBoolean(v))
      }

      //Binary: Both Binary

      //DoSeq
      case Binary(Seq,v1, e2) if(isValue(v1)) => e2

      // I CHANGED STUFF THERE
      case Binary(Plus, v1, v2) if (isValue(v1) && isValue(v2)) => ( v1, v2) match {
        //DoPlusString1 & DoPlusString2
        case (S(s), v2) => S(s + toStr(v2))
        case (v1, S(s)) => S(toStr(v1) + s)
        //DoPlusNumber
        case (v1, v2) => N(toNumber(v1) + toNumber(v2))
      }

      //DoArith
      case Binary(bop @ (Minus | Times | Div), v1, v2) if (isValue(v1) && isValue(v2)) => bop match {
        case Minus => N(toNumber(v1) - toNumber(v2))
        case Times => N(toNumber(v1) * toNumber(v2))
        case Div => N(toNumber(v1) / toNumber(v2))
      }

      //DoInequalityNumber1 & DoInequalityNumber2 -> one value can be a string not both
      case Binary(bop @ (Lt | Le | Gt | Ge), v1, v2) if (isValue(v1) && isValue(v2)) => bop match {
        case Lt => B(toNumber(v1) < toNumber(v2))
        case Le => B(toNumber(v1) <= toNumber(v2))
        case Gt => B(toNumber(v1) > toNumber(v2))
        case Ge => B(toNumber(v1) >= toNumber(v2))
      }

      //DoInequalityString -> if both are string do this
      case Binary(bop @ (Lt | Le | Gt | Ge), S(v1), S(v2)) => bop match {
        case Lt => B(v1 < v2)
        case Le => B(v1 <= v2)
        case Gt => B(v1 > v2)
        case Ge => B(v1 >= v2)
      }

      //DoEquality
      case Binary(bop @ (Eq | Ne), v1, v2) if (isValue(v1) && isValue(v2)) => (bop, v1, v2) match {
        //Functions cannot do equality
        case (bop @ (Eq | Ne), Function(_, _, _), v2) => throw new DynamicTypeError(e)
        case (bop @ (Eq | Ne), v1, Function(_, _, _)) => throw new DynamicTypeError(e)
        case (Eq, v1, v2) => B(toBoolean(v1) == toBoolean(v2))          
        case (Ne, v1, v2) => B(toBoolean(v1) != toBoolean(v2))
      }

      //Binary AND/OR
      case Binary(bop @ (And | Or), v1, v2) if (isValue(v1) && isValue(v2)) => bop match {
        //DoAndTrue & DoAndFalse
        case And => if(toBoolean(v1)) v2 else v1
      
        //DoOrTrue & DoOrFalse
        case Or => if(toBoolean(v1)) v1 else v2
      }
            
      //Binary AND/OR with v1 being a value and e2 being any expression
      case Binary(And, v1, e2) => if(toBoolean(v1)) e2 else v1
      case Binary(Or, v1, e2) => if(toBoolean(v1)) v1 else e2

      
      //DoIfTrue & DoIfFalse -> v1 is a value and e1, e2 are any expressions
      case If(v1, e2, e3) if(isValue(v1)) => if(toBoolean(v1)) e2 else e3
      
      //DoConst -> v1 is a values, e2 is any expression
      // 
      case ConstDecl(x, v1, e2) if(isValue(v1)) => substitute(e2, v1, x)
      
      //DoCall & DoCallRec
      case Call(Function(name, x, eprime), v2) if (isValue(v2)) => Function(name, x, eprime) match {
        //DoCallRec
        case Function(Some(f), x, eprime) => substitute( substitute(eprime, v2, x), Function(Some(f), x, eprime), f)
        //DoCall
        case Function(name, x, eprime) => substitute(eprime, v2, x)
      }


      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      
        // ****** Your cases here
      //SearchUnary
      case Unary(uop, e1) => Unary(uop, step(e1))
        
      //Binary Cases with the First as a Value
      case Binary(bop, v1, e2) if (isValue(v1)) => (bop: @unchecked, v1, e2) match {
        //TypeErrorEquality1 & Factors in SearchEquality2
        case (bop @ (Eq | Ne), Function(_,_,_), e2) => throw new DynamicTypeError(e)
        
        //SearchBinaryArith2 & SearchEquality2
        case (bop, v1, e2) => Binary(bop, v1, step(e2))
      }
      
      //Binary Cases with Both as Expressions
      case Binary(bop, e1, e2) => (bop: @unchecked, e1, e2) match {
      //TypeErrorEquality1  
        case (bop @ (Eq | Ne), e1, Function(_,_,_)) => throw new DynamicTypeError(e)
        
        //SearchBinary1
        case (bop, e1, e2) => Binary(bop, step(e1), e2)
      }

      //SearchIf
      case If(e1, e2, e3) => if(toBoolean(step(e1))) e2 else e3
      
      //SearchConst
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)

      //SearchCall Cases
      case Call(e1, e2) => (e1, e2) match {
      //SearchCall2 Cases; with and without function name
        case (Function(None, x, eprime), e2) => Call(Function(None, x, eprime), step(e2))
        case (Function(Some(f), x, eprime), e2) => Call(Function(Some(f), x, eprime), step(e2))
        
        //TypeErrorCall -> if e1 is Value and not a Function error
        case (e1, e2) if (isValue(e1))=> throw new DynamicTypeError(e)
        
        //SearchCall1
        case (e1, e2) => Call(step(e1), e2)
      }
      
      //*******************

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }
  

  /* External Interfaces */
  
  this.debug = true // comment this out or set to false if you don't want print debugging information
  
  // Interface to run your big-step interpreter starting from an empty environment and print out
  // the test input if debugging.
  def evaluate(e: Expr): Expr = {
    require(closed(e))
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with eval ...")
      println("\nExpression:\n  " + e)
    }
    val v = eval(emp, e)
    if (debug) {
      println("Value: " + v)
    }
    v
  }
  
  // Convenience to pass in a jsy expression as a string.
  def evaluate(s: String): Expr = evaluate(jsy.lab3.Parser.parse(s))
   
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging. 
  def iterateStep(e: Expr): Expr = {
    require(closed(e))
    def loop(e: Expr, n: Int): Expr = {
      if (debug) { println("Step %s: %s".format(n, e)) }
      if (isValue(e)) e else loop(step(e), n + 1)
    }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = loop(e, 0)
    if (debug) {
      println("Value: " + v)
    }
    v
  }

  // Convenience to pass in a jsy expression as a string.
  def iterateStep(s: String): Expr = iterateStep(jsy.lab3.Parser.parse(s))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr =
      handle(None: Option[Expr]) {Some{
        jsy.lab3.Parser.parseFile(file)
      }} getOrElse {
        return
      }
    
    handle() {
      val v = evaluate(expr)
      println(pretty(v))
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(pretty(v1))
    }
  }
    
}
