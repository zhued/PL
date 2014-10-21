object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   */

  /*
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
  
  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case Undefined => Double.NaN
      case N(n) => n
      case B(b) => if (b) 1.0 else 0.0
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case Undefined => false
      case null => false
      case B(b) => b      
      case N(n) => if ((n == 0) || (n == Double.NaN)) false else true
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case N(n) => n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case Undefined => Undefined
      case B(b) => B(b)
      case N(n) => N(n)
      case S(s) => S(s)
      case Var(x) => get(env, x)

      /* AndOr Spec */
      case Binary(And, e1, e2) => 
        if (toBoolean(eval(env, e1)))
          eval(env, e2)
        else
          eval(env, e1)
      
      case Binary(Or, e1, e2) => 
        if (toBoolean(eval(env, e1)))
          eval(env, e1)
        else
          eval(env, e2)
          
      /* Arithmetic Spec */
      //case(S(s1), v2) => S(s1 + toStr(v2))
      case Binary(Plus, e1, e2) => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)))
      case Binary(Minus, e1, e2) => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
      case Binary(Times, e1, e2) => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
      case Binary(Div, e1, e2) => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))

      /* Comparison Spec */
      //(eToVal(e1), eToVal(e2)) match case(S(s1), S(s2)) => B(s1 == s2)
      case Binary(Eq, e1, e2) => B(toNumber(eval(env, e1)) == toNumber(eval(env, e2)))
      case Binary(Ne, e1, e2) => B(toNumber(eval(env, e1)) != toNumber(eval(env, e2)))
      case Binary(Lt, e1, e2) => B(toNumber(eval(env, e1)) < toNumber(eval(env, e2)))
      case Binary(Le, e1, e2) => B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)))
      case Binary(Gt, e1, e2) => B(toNumber(eval(env, e1)) > toNumber(eval(env, e2)))
      case Binary(Ge, e1, e2) => B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)))

      /* Const Spec */
      //constant declaration by adding a new relationship
      //to the env and then we evaluate an expression 
      //that uses the constant within context of the new 
      //environment
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      //ConstantDecl = declaring constant and evaluates it, env = map of strings which are names to values
      //extends env, adds relationship of string of name of literal and value and maps it to the value
      //return var declared and evaluate it using eval
      //call eval to e2, which is initial string you passed in

      /* If Spec */
      case If(e1, e2, e3) => if (eval(env,e1) == B(true)) eval(env,e2) else eval(env,e3)

      /* Seq Spec */
      case Binary(Seq, e1, e2) => eval(env, e1); eval(env,e2)

      /* Unary Spec */
      case Unary(Neg, e1) => N(-toNumber(eval(env, e1)))
      case Unary(Not, e1) => B(!toBoolean(eval(env, e1)))

      /* Inductive Cases */
      case Print(e1) => println(pretty(eToVal(e1))); Undefined

      case _ => throw new UnsupportedOperationException
    }
  }
    
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }

}