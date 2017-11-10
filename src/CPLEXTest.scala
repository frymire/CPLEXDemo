
import ilog.concert.IloMPModeler
import ilog.cplex.IloCplex


/*
 * Solve the following linear program:
 *
 *  Max    x1 + 2 x2 + 3 x3
 *  
 *  s.t. - x1 + x2 + x3 <= 20
 *   	   x1 - 3 x2 + x3 <= 30
 *  
 *  	   x1 <= 40 
 */
object CPLEXTest extends App {  

  // Populate the model, and write it to a file
  val model = new IloCplex
  val (x, rows) = populate(model)
//  val (x, rows) = populateByColumn(model)
//  val (x, rows) = populateByMatrix(model)
  model.exportModel("CPLEXTest.lp")
  
  // Solve the model and display the solution if one was found	
  if (model.solve) {
    
    // Write the solution to a file in XML format. 
    model.writeSolution("CPLEXTest.sol")
            
    println(s"Solution status = ${model.getStatus}")
    println(s"Solution value  = ${model.getObjValue}")
    
    x foreach { j => println(s"$j: Value = ${ model.getValue(j) }\tReduced cost = ${ model.getReducedCost(j) }") }
    rows foreach { i => println(s"${i.getName}: Slack = ${ model.getSlack(i) }\t\tPi = ${ model.getDual(i) }") }
				
  }
		
  model.end    
    

  // Populate the model by rows.
  def populate(model: IloMPModeler) = {
    
    // Define the variables. Note that you can also define the variable names after the fact with x(j).setName("x1")
    val lb = Array(0.0, 0.0, 0.0)
    val ub = Array(40.0, Double.MaxValue, Double.MaxValue)
    val x = model.numVarArray(3, lb, ub, Array("x1", "x2", "x3") )
    
    // Add the objective function.  model.addMaximize(z) is the same as model.add( model.maximize(z) ).
	model.addMaximize( model.scalProd( Array(1.0, 2.0, 3.0), x ) )
	
	// Define the constraints.  The "Le" means "less than or equal."  Otherwise, you would use "Eq" or "Ge".
	val rows = Array(
	    model.addLe( model.scalProd( Array(-1.0, 1.0, 1.0), x ), 20.0, "c1"),
	    model.addLe( model.scalProd( Array( 1.0,-3.0, 1.0), x ), 30.0, "c2") )

    // You could also add constraints to a pre-defined array, or add pieces step-by-step, including sparse A matrix entries.
//    val rows = new Array[IloRange](2)
//    rows(0) = model.addLe( model.scalProd( Array(-1.0, 1.0, 1.0), x ), 20.0, "c1")
//    rows(1) = model.addRange(-Double.MaxValue, 30.0)
//    rows(1) setName "c2"
//    rows(1) setExpr model.sum( model.prod( 1.0, x(0) ), model.prod(-3.0, x(1) ), model.prod( 1.0, x(2) ) )
    
	// Return the variables and constraints as a pair
    (x, rows)
		
  } // populate
  
  
  def populateByColumn(model: IloMPModeler) = {
    
    // Say that we're maximizing.  We'll specify coefficients when we add the columns.
    val obj = model.addMaximize
    
    // Define the limits of the constraints
    val rows = Array(
		model.addRange(-Double.MaxValue, 20.0, "c1"),
		model.addRange(-Double.MaxValue, 30.0, "c2") )

	// Define the objective function coefficients, A.j column, bounds, and name for each variable.
    val x = Array(
        model.numVar(model.column(obj, 1.0) and model.column(rows(0),-1.0) and model.column(rows(1), 1.0), 0.0, 40.0, "x1"),
        model.numVar(model.column(obj, 2.0) and model.column(rows(0), 1.0) and model.column(rows(1),-3.0), 0.0, Double.MaxValue, "x2"),
        model.numVar(model.column(obj, 3.0) and model.column(rows(0), 1.0) and model.column(rows(1), 1.0), 0.0, Double.MaxValue, "x3") )

	// Return the variables and constraints as a pair
    (x, rows)
		
  } // populateByColumn

  
  def populateByMatrix(model: IloMPModeler) = {
    
    // Define the variables. Note that you can also define the variable names after the fact with x(j).setName("x1")
    val lb = Array(0.0, 0.0, 0.0)
    val ub = Array(40.0, Double.MaxValue, Double.MaxValue)
    val x = model.numVarArray(3, lb, ub, Array("x1", "x2", "x3") )
    
    // Add the objective function.  model.addMaximize(z) is the same as model.add( model.maximize(z) ).
	model.addMaximize( model.scalProd( Array(1.0, 2.0, 3.0), x ) )
	
	// Set up an A matrix with the x variables
    val matrixA = model.addLPMatrix
    matrixA.addCols(x)

  	// Add constraints with sparse elements.  The first array holds the non-zero indices for the rows, while
    // the second holds the corresponding value.  It looks like you can't add constraint names this way.
	matrixA.addRow(Double.NegativeInfinity, 20.0, Array(0,1,2), Array(-1.0, 1.0, 1.0) )
	matrixA.addRow(Double.NegativeInfinity, 30.0, Array(0,1,2), Array( 1.0,-3.0, 1.0) )

    // Specify the non-zero elements and add them to the matrix
//	val nonZeros = Array( (0, 0,-1), (0, 1, 1), (0, 2, 1), (1, 0, 1), (1, 1,-3), (1, 2, 1) )
//	nonZeros foreach { nz => matrixA.setNZ(nz._1, nz._2, nz._3) }
    
	// An uglier way is to add each component vector    
    // val (is, js, values) = nonZeros.unzip3
    // matrixA.setNZs(is.toArray, js.toArray, values.asInstanceOf[Array[Double]] )
    
    // Return the variables and constraints as a pair
    (x, matrixA.getRanges)
    
  } // populateByMatrix

  
} // CPLEXTest
