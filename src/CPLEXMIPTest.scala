// UNCLASSIFIED

import ilog.concert.IloNumVarType.{Float, Int, Bool}
import ilog.cplex.IloCplex

object CPLEXMIPTest extends App {

  // Create a new model
  val model = new IloCplex

  // Define the variables.  
  val lb = Array(0.0, 0.0, 0.0, 2.0)
  val ub = Array(40.0, Double.MaxValue, Double.MaxValue, 3.0)
  val types = Array(Float, Float, Float, Int)
  val x = model.numVarArray(4, lb, ub, types, Array("x0", "x1", "x2", "x3") )
  val y = model.numVar(0.0, 1.0, Bool, "y")
    
  // Add the objective function. maximize x0 + 2*x1 + 3*x2 + x3
  val z = model.scalProd( Array(1.0, 2.0, 3.0, 1.0), x )
  
  // Add the y variable.  The first version shows how to add an expression to another.
  // z add model.scalProd( Array(-7.0), Array(y) )
  z.addTerm(-7.0, y)
  
  model.addMaximize(z)
	
  // Define the constraints.
  val rows = Array(
      model.addLe( model.scalProd( Array(-1.0, 1.0, 1.0, 10.0), x ), 20.0, "c1"),
	  model.addLe( model.scalProd( Array( 1.0,-3.0, 1.0,  0.0), x ), 30.0, "c2"),
	  model.addEq( model.scalProd( Array( 0.0, 1.0, 0.0, -3.5), x ),  0.0, "c3") )
	  
  // Write the model to a file
  model.exportModel("CPLEXTest.lp")
  
  // Solve the model and display the solution if one was found	
  if (model.solve) {
    
    // Write the solution to a file in XML format. 
    model.writeSolution("CPLEXTest.sol")
            
    println(s"Solution status = ${model.getStatus}")
    println(s"Solution value  = ${model.getObjValue}")
    
    x foreach { j => println(s"$j: Value = ${ model.getValue(j) }") }
    println(s"$y: Value = ${ model.getValue(y) }")
    rows foreach { i => println(s"${i.getName}: Slack = ${ model.getSlack(i) }") }
				
  } // if cplex.solve
		
  model.end    
    

} // CPLEXMIPTest


// UNCLASSIFIED
