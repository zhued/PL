# Lab 3

Edward Zhu; Partner - Alex Campbell

### 1. JavaScripty Interpreter: Tag Testing, Recursive Funtions, and Dynamic Scoping
**a.**  Give one test case that behaves differently under dynamic scoping versus static scoping (and does not crash). Explain the test case and how they behave differently in your write-up.

Static Scoping: 
 ``` javascript
	global=0;

	function runFunc(){
	    var global = 10;
	    callMe();
	}

	function callFunc(){
	   test = global+10;
	}

	runFunc();
	print(test);
	print(global);
 ```

### 2. JavaScripty Interpereter: Substitution and Evaluation Order
**c** Explain whether the evaluation order is eterministic as specified by judgement form e -> eprime.

### 3. Evaluation Order
Consider the small-step orperational semantics for JavaScripty schown in Figures 7, 8, and 9. What is the evaluation order for e1 and e2? Explain. How do we change the rules optain the opposite evaluation order?

### 3. Short circuit Evaluation
**a** Concept: Give an example that illustrates the usefulness of short circuit evaluation. Explain your example.

**b** JavaScripty. Consider the small step BLAH BLAH BLAH

### Scala exercices
See Lab2.scala

To run the auto-grader:

	sbt "project lab3-grader" run

To run the jsy file:

	sbt "run <path_to_jsy_file>"