# Lab 3

Edward Zhu; Partner - Alex Campbell

### 1. JavaScripty Interpreter: Tag Testing, Recursive Funtions, and Dynamic Scoping
**a.**  Give one test case that behaves differently under dynamic scoping versus static scoping (and does not crash). Explain the test case and how they behave differently in your write-up.

Static Scoping: 
 ``` javascript
	global=0;

	function runFunc(){
	    var global = 10;
	    callFunc();
	}

	function callFunc(){
	   addTen = global+10;
	}

	runFunc();
	print(addTen);
	print(global);
 ```

If this program was ran on with a dynamically typed language, then 'global' would print 0 and 'addTen' would print 20. This is because the scope of the function callFunc will be within the same scope in the function runFunc, where the var global in runFunc is changed from the global outside its scope that was set to 0 to the value 10. So then the global variable in callFunc will reference the value of 10 from runFunc, so addTen will be evaluated to 10+10 which will be 20.

When this program is ran under static scoping, even though the 'global' in runFunc gets changed to 10 from 0, when callFunc() gets called, the scope within the callFunc is separtate from the scope of runFunc, thus the 'global' in runFunc is bound to the outside scope at the very beginning of the value 0. Thus global in this case would print 0 and addTen would print 10.



### 2. JavaScripty Interpereter: Substitution and Evaluation Order
**c.** Explain whether the evaluation order is deterministic as specified by judgement form e -> eprime.

### 3. Evaluation Order
Consider the small-step orperational semantics for JavaScripty schown in Figures 7, 8, and 9. What is the evaluation order for e1 and e2? Explain. How do we change the rules optain the opposite evaluation order?

### 4. Short-Circuit Evaluation
**a.** Concept: Give an example that illustrates the usefulness of short circuit evaluation. Explain your example.
An example of short circuit being useful is when once we evaluate a value of one expression we don't have to evaluate any other expressions because we know what the end evaluation will be. Below is an example of short circuiting being benificial:

 ```
	e -> eprime
	-----------

 ```


**b.** JavaScripty. Consider the small-step operational semantics for JAVASCRIPTY shown in Figures 7, 8, and 9. Does e1&&e2 short circuit? Explain.
Yes, e1&&e2 will short circuit because in the DoAndFalse judgement, when toBoolean(v1) is false, then the expression v1&&e2 will evaluate to just v1 and will not even evaluate e2. This is possible because the behavior of the '&&' will return a false immediately once one of the values is false, so if v1 is false, we can automatically evaluate v1&&e2 to v1 which is false.

### Scala exercices
See Lab2.scala

To run the auto-grader:

	sbt "project lab3-grader" run

To run the jsy file:

	sbt "run <path_to_jsy_file>"
