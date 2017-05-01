

def online_cov(data1: Array[Double], data2:Array[Double]): Double = {
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
  C / (n-1)
}



online_cov(Array(1.9, 2.9, 3.4, 3.5, 4.6),Array(1.9, 2.9, 3.4, 3.5, 7.8))