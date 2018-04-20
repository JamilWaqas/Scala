


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

/*Function Online Variance takes two parameter in Input:
a - an array of doubles (data stream)
n - length of the array

OnlineVariance outputs a Double
 */
def OnlineVariance(a: Array[Double]) = {
  var n = 0
  var mean = 0.0
  var M2 = 0.0
  for(x<- a){
    n += 1
    var delta = x - mean
    mean += delta/n
    var delta2 = x - mean
    M2 += delta * delta2
  }
  if(n < 2){
    Array(0.0,0.0,0.0)
  }
  else{
    Array(M2/n,n * 1.0,mean)
  }
}

OnlineVariance(Array(1.9, 2.9, 3.4, 3.5))(2)

//For Parallel implementation

def parallel_variance(avg_a:Double,count_a:Double,var_a:Double,avg_b:Double,count_b:Double,var_b:Double): Double ={
  val delta = avg_b - avg_a
  val m_a = var_a * (count_a - 1)
  val m_b = var_b * (count_b -1)
  val M2 = m_a + m_b + Math.pow(delta,2) * count_a * count_b / (count_a + count_b)
  M2/ (count_a + count_b -1)
}

parallel_variance(OnlineVariance(Array(1.9, 2.9, 3.4, 3.5))(2),OnlineVariance(Array(1.9, 2.9, 3.4, 3.5))(1),OnlineVariance(Array(1.9, 2.9, 3.4, 3.5))(0),OnlineVariance(Array(1.8, 2.4, 3.4, 3.6))(2),OnlineVariance(Array(1.8, 2.4, 3.4, 3.6))(1),OnlineVariance(Array(1.8, 2.4, 3.4, 3.6))(0))



