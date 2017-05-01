

def online_cov(data1: Array[Double], data2:Array[Double]) = {
  var meanx = 0.0
  var meany = 0.0
  var C = 0.0
  var n = 0
  var s = data1.zip(data2)
  for ((x,y)<- s) {
    n += 1
    val dx = x - meanx
    meanx += dx / n
    meany += ( y - meany ) / n
    C += dx * ( y - meany )

  }
  Array(C / (n-1), meanx,meany, n * 1.0)
}



online_cov(Array(1.9, 2.9, 3.4, 3.5, 4.6),Array(1.9, 2.9, 3.4, 3.5, 7.8))

//For Parallel covariance

def parallel_Cov(cov_a:Double,cov_b:Double,avg_a1:Double,avg_a2:Double,avg_b1:Double,avg_b2:Double,count_a:Double,count_b:Double):Double = {

  cov_a + cov_b + (avg_a1 + avg_a2) * (avg_b1 + avg_b2) * ((count_a * count_b) / (count_a + count_b))

}

val a1 = Array(1.9, 2.9, 3.4, 3.5, 4.6)
val a2 = Array(1.9, 2.9, 3.4, 3.5, 7.8)
val b1 = Array(1.9, 1.9, 2.4, 3.5, 2.6)
val b2 = Array(1.9, 3.9, 3.4, 3.5, 6.8)

val cov1  = online_cov(a1,a2)
val cov2  = online_cov(b1,b2)

parallel_Cov(cov1(0),cov2(0),cov1(1),cov1(2),cov2(1),cov2(2),cov1(3),cov2(3))