import breeze.linalg._



//Following code implements example given in pages 11-15 of An Introduction to the Kalman Filter
// by Greg Welch and Gary Bishop, University of North Carolina at Chapel Hill, Department of Computer Science.


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

//Kalman Filter
def Kalman(xhat:Double,P:Double,DataStream:DenseVector[Double],Q:Double,R:Double): DenseVector[Double] = {
  var xhat1 = DenseVector[Double](xhat)
  var xhata = DenseVector[Double](xhat)
  var P1 = P
  var k = 1
  var K = 0.0 //Kalaman gain
  for (l<- DataStream) {
    //time update
    var xhatminus = xhat1
    var Pminus = P + Q
    //Prediction
    K = Pminus / (Pminus + R)
    xhat1 = xhatminus + K  * (l - xhatminus)
    if( k == 1 ){
      xhata = xhat1
    }
    else {
      xhata = DenseVector.vertcat ( xhata, xhat1 )
    }
    P1 = (1 - K) * Pminus
    k += 1
  }
  xhata
}






//var z = DenseVector(-0.3250418, -0.3762906, -0.4213226, -0.2573210, -0.3890168, -0.3734490,
 // -0.2577894, -0.3428742, -0.4101773, -0.2101842, -0.4690758, -0.3860507,
 // -0.2452406, -0.2041914, -0.1610104, -0.4088432, -0.4347796, -0.5179056,
 // -0.1504840, -0.4543554, -0.3392384, -0.3167563, -0.2753026, -0.3297757,
 // -0.5958646, -0.2839508, -0.3297265, -0.3382418, -0.4500028, -0.2786144,
 // -0.2348716, -0.3287969, -0.3423464, -0.2912576, -0.3368089, -0.3405655,
 // -0.5291899, -0.2222897, -0.3272819, -0.3311827, -0.1695990, -0.4080351,
 // -0.2820329, -0.3239912, -0.3868248, -0.3914763, -0.4955726, -0.3227382,
 // -0.6354555, -0.2993800)//Input (this is some random normal data)

//val Q = 0.00005 //process Variance

//val R = 1.0//meausrement variance

//initial guess about xhat and P
//var xhat = 0.0//value of interest
//var P = 1.0//error

//Kalman(xhat,P,z,Q,R)