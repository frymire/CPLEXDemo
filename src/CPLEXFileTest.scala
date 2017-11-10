// UNCLASSIFIED

import ilog.concert.IloLPMatrix
import ilog.cplex.IloCplex


object CPLEXFileTest extends App {
  
  // Import an existing model from a file and solve it.
  val model = new IloCplex
  model.importModel("CPLEXTest.lp")
  
  // To get out the variables and constraints, first extract the model's IloLPMatrix
  val lp = model.LPMatrixIterator.next.asInstanceOf[IloLPMatrix]
  val x = lp.getNumVars
  val rows = lp.getRanges

  // Solve and print the solution.
  model.solve              
  
  println(s"Solution status = ${model.getStatus}")
  println(s"Solution value  = ${model.getObjValue}")

  x foreach { xj => println(s"$xj: Value = ${ model.getValue(xj) }\tReduced cost = ${ model.getReducedCost(xj) }") }  
  rows foreach { ci => println(s"${ci.getName}: Slack = ${ model.getSlack(ci) }\t\tPi = ${ model.getDual(ci) }") }  

  model.end

} // CPLEXFileTest

// UNCLASSIFIED