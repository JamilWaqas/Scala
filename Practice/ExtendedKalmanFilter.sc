import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import breeze.stats.distributions.Gaussian


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

//Functions need to apply kalman filters

def logistG(r:Double,p:Double,k:Double,t:Double): Double = {
  k * p * exp(r*t)/(k + p * (exp(r * t)-1))
}

def a(x:DenseVector[Double], k:Double, deltaT:Double):DenseVector[Double] ={
  DenseVector(x(0),logistG(x(0),x(1),k,deltaT))
}
// I am simply calculating Jacobian, where f_1 function is (x_1,0) and f_2 is logistG(r=x1,p=x2)
def jacobian(x:DenseVector[Double], k:Double, deltaT:Double): DenseMatrix[Double] = {
  val dif1x1 = 1.0
  val dif2x1 = - ( k * x(1) * (x(1) - k) * deltaT * exp(deltaT * x(0)))/((x(1) * exp(deltaT * x(0)) - x(1) + k) * (x(1) * exp(deltaT * x(0)) - x(1) + k))
  val dif1x2 = 0.0
  val diff2x2 = (k * k * exp(x(0) * deltaT))/((((exp(x(0) * deltaT)-1.0) * x(1) + k)) * ((exp(x(0) * deltaT)-1.0) * x(1) + k))
  val mat = DenseMatrix((dif1x1,dif1x2),(dif2x1,diff2x2))
  mat
}



def ExtendedKalman(G:DenseVector[Double],Q:DenseVector[Double],R:Double,prior:DenseVector[Double],Sigma:DenseVector[Double],DataStream:DenseVector[Double],deltaT:Double,k:Double): DenseVector[Double] = {

  var Sigma1 = diag ( Sigma )
  var Q1 = diag (Q)
  var y = 0.0
  var m = 0
  var prior1 = prior
  var Estimate = DenseVector (1.0,1.0)
  for (s<-DataStream){
    val xobs = DenseVector (0.0,s)
    y = G.t * xobs
    //Filtering

    val SigmaInv = 1.0 / ( G.t * Sigma1 * G + R )
    val xf = prior1 + Sigma1 * G * ( SigmaInv :* ( y :- ( G.t * prior1 ) ) )
    Sigma1 = Sigma1 - ( Sigma1 * G * ( SigmaInv :* G.t * Sigma1 ) )

    val A = jacobian ( prior1, k, deltaT )
    val K = ( A * Sigma1 * G.t ) * SigmaInv

    if ( k == 0 ) {
      Estimate = prior1
    }
    else{
      Estimate = DenseVector.vertcat(Estimate,prior1)//Prediction
    }

    prior1 = a(xf,k,deltaT) + K * ( y - G.t * xf )
    Sigma1 = A * Sigma1 * A.t - K * G.t * Sigma1 * A.t + Q1

    m +=1
  }
  Estimate
}

/*
val nObs = 250
val k = 100.0
val p0 = 0.1 * k
val r = 0.2
val deltaT = 0.1
val rr = DenseVector.range(0,nObs,1)

val obsVariance =  25.0

val nu = Gaussian(0.0,sqrt(obsVariance))
val normal = DenseVector.rand(nObs,nu)


val deltarange = DenseVector.tabulate(nObs){range => deltaT * range}
var pop = DenseVector(0.0)
var poup = DenseVector(0.0)
for(i<- rr){
  pop = DenseVector(logistG(r,p0,k,deltarange(i))+ normal(i)) + p0
  poup = DenseVector.vertcat(poup,pop)
}

//Input variables
//var y = 0.0
val result = poup

//var Estimate = DenseVector(0.0,0.0)
val G = DenseVector[Double](0.0,1.0)
val Q = (DenseVector(0.0,0.0))
val R = obsVariance
var prior = DenseVector(r,p0)
var Sigma = (DenseVector(144.0,25.0))

//ExtendedKalman(G,Q,R,prior,Sigma,result,deltaT,k)
/*


 */
//FOLLOWING IS THE SKETCH CODE UNCOMENT TO VIW THE PLOT AND COMENT EVERYTHING
//FROM FUNCTION ExtendedKalmanFilter up till here
/*
val nObs = 500§
val k = 100.0
val p0 = 0.1 * k
val r = 0.2
val deltaT = 0.1
val rr = DenseVector.range(0,nObs,1)

val obsVariance =  25.0

val nu = Gaussian(0.0,sqrt(obsVariance))
val normal = DenseVector.rand(nObs,nu)


val deltarange = DenseVector.tabulate(nObs){range => deltaT * range}
var pop = DenseVector(0.0)
var poup = DenseVector(0.0)
for(i<- rr){
    pop = DenseVector(logistG(r,p0,k,deltarange(i))+ normal(i)) + p0
    poup = DenseVector.vertcat(poup,pop)
}
val result = poup
var y = 0.0
var Estimate = DenseVector(0.0,0.0)
val G = DenseVector[Double](0.0,1.0).t
val Q = diag(DenseVector(0.0,0.0))
val R = obsVariance
var prior = DenseVector(r,p0)
var Sigma = diag(DenseVector(144.0,25.0))

//Extended Kalman Filter equations
for(i<- 1 to nObs){

  //observations

  val xobs = DenseVector(0.0,result(i))
  y = G * xobs
  //Filtering

  val SigmaInv = 1/(G * Sigma * G.t + R)
  val xf = prior + Sigma * G.t * (SigmaInv :* (y :- (G * prior)))
  Sigma = Sigma - (Sigma * G.t * (SigmaInv :* G * Sigma))

  val A = jacobian(prior,k,deltaT)
  val K = (A * Sigma * G.t) * SigmaInv

  Estimate = DenseVector.vertcat(Estimate,prior)
  //Prediction

  prior = a(xf, k ,deltaT) + K * (y - G * xf)
  Sigma = A * Sigma * A.t - K * G * Sigma * A.t + Q

}

//In prediction prior is now the predictive distribution and Sigma is for the predictive distibution. These are the variables of
//interest while predicting.



//Following are the Input variables plus some preperation before we can apply kalman filters

//Input variables
//var y = 0.0



//Some plots

val actual = DenseVector.fill(nObs)(0.0)
val ac = 0.0
for(i<- rr) {
 actual(i) = logistG(r,p0,k,deltarange(i))
}
val fin = actual
val result1 = result(1 to nObs)

var prediction = DenseVector.fill(nObs)(0.0)
for(i<- rr){
  prediction(i) = Estimate( (2 * i) -1 )
}
val ins = prediction



val f = Figure()
val p = f.subplot(0)
val n = deltarange
p += plot(n,result1)
p += plot(n, fin)
p += plot(n,ins)
p.xlabel = "Time"
p.ylabel = "Data Stream"
p.legend = true
//f.saveas("plot.pdf")
*/
