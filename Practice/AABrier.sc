import breeze.linalg._
import breeze.numerics.{exp, log}
import java.io._

/*
Copyright (c) 2017 Waqas Jamil, funded by PROTEUS
Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:


  The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

Please feel free to suggest improvements
*/
/*Usage is as follows
User should pass the required input and output values in the function prediction, each machine can be thought
 of an expert giving values between 0 and 1. For reference please see:

[1] Vovk, Vladimir, and Fedor Zhdanov. "Prediction with expert advice for the Brier game."
 Journal of Machine Learning Research
 10.Nov (2009): 2445-2471.

 in.csv is the tennis data mentioned in [1]. To generlise even further you may decide to add
 A, B and eta as input arguments, where A will be the minimum of the data, B will be the maximum of the data, and
 eta = 2/(B-A)^2.
 */


/* Input arguments function prediction takes are as follows:
1) outcomes    -- outcomes must be between 0 and 1 for this Brier square loss game
2) Input   -- must be between 0 and 1 for this square loss brier game


*/

/*
Output given by the function prediction is a Vector of doubles and it consider that one column Vector(stream of data) sequentially,
So for each stream the function prediction outputs a double, thus for n streams we get a vector of length n.
 */

//substitution function transforms generalised prediction to the actual prediction

def SubstFunction(predictions: DenseVector[Double], expertsPrediction: DenseVector[Double]) = {


  val A: Double = 0.0 //minimum value of the data

  val B: Double = 1.0 // maximum value of the data

  val eta: Double = 2.0 / Math.pow(B - A, 2.0)

  var gA = 0.0 // generalised prediction with respect to minimum of the data

  var gB = 0.0 // generalised prediction with respect to maximum of the data

  var gamma = 0.0 // actual prediction by AA


  gA = -(1.0 / eta) * log(predictions dot exp(-eta * ((expertsPrediction :- A) :^ 2.0)))

  gB = -(1.0 / eta) * log(predictions dot exp(-eta * ((expertsPrediction :- B) :^ 2.0)))

  gamma = (0.5 * (B + A)) - ((gB - gA) / 2 * (B - A))

  DenseVector(gamma)

}

//prediction uses substitution function to return actual prediction by assigning weights to experts at each time step

def prediction(Input: DenseMatrix[Double], outcomes: DenseVector[Double]): DenseVector[ Double ] = {

  val nRow: Int = Input.rows

  var n: Int = 0

  val A: Double = 0.0 //minimum value of the data

  val B: Double = 1.0 // maximum value of the data

  val eta: Double = 2.0 / Math.pow(B - A, 2.0)

  var weights = DenseVector.fill(nRow)(1.0)

  var AAprediction = DenseVector(1.0)

  var weights1 = DenseVector.fill(nRow)(1.0)

  var pred = DenseVector(0.0)


  for (l<-outcomes) {

    var normalisedWeights = weights / sum(weights)

    pred = SubstFunction(normalisedWeights, Input(::,n))

    if( n == 0 ){

      AAprediction = pred

    }

    else {

      AAprediction = DenseVector.vertcat( AAprediction, pred )

      weights1 = normalisedWeights :* exp ( eta :* ( Input(::,n) :- l ) :^ 2.0 )

      weights = weights1 / sum ( weights1 )

    }

    n += 1

  }

  AAprediction
}

//To test the function please uncomment the following lines

val data = csvread(file = new File("/Users/wj/Documents/PROTEUS_WJ/src/main/scala/in.csv")).t

//each row of the data is experts prediction (and column represent time step of prediction)

val out: DenseVector[Double] = DenseVector.fill(10087)(1.0)// out stands for outcomes

 prediction(data,out)








