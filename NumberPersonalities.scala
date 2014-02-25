object NumberPersonalities{
  val limit = 100
    
  def isPrime(n: Int): Boolean = {       // method for testing prime numbers
    var i = 2
    var prime = true
    while (i < n && i != 1) {
      if (n % i == 0) {
        prime = false
        i = n
      }
      i = i + 1
    }
    if (n == 1) prime = false
    prime
  }
  
  def isHappy(n: Int): Boolean = {        // method for testing happy numbers
    var sum = 0
    var happy = false
    var number = n
    while (sum != 4 && happy == false) {
      while (number != 0) {
        sum = sum + (number % 10) * (number % 10)
        number = number / 10
      }
      if (sum == 1) {
        happy = true
      } else if (sum != 4) {
        number = sum
        sum = 0
      }
    }
    happy
  }
  
  def isTriangular(n: Int): Boolean = {
    var trangular = true              // method for testing triangular numbers
    var number = n
    var i = 1
    while (number > 0) {
      number = number - i
      i = i + 1
    }
    if (number == 0) {
      trangular = true
    } else {
      trangular = false
    }
    trangular
  }  
  
  def isSquare(n: Int) = {        // method for testing square numbers
    var square = true
    var number = n
    var i = 1
    while (number > 0) {
      number = number - i
      i = i + 2
    }
    if (number == 0) {
      square = true
    } else {
      square = false
    }
    square
  }
  
  def isSmug(n: Int) = {        // method for testing smug numbers
    var number = n
    var i = 1
    var smug = false
    while (n >= (i * i) && smug == false) {
      number = n
      number = number - (i * i)
      var j = i
      while (number >= (j * j) && smug == false) {
        if (number == j * j) {
          smug = true
        } else {
          j = j + 1
        }
      }
      i = i + 1
    }
    smug
  }
  
  def isHonest(n: Int) = {          // method for testing honest numbers
    var k = 1
    var honest = true
    while (n / k >= k) {
      if (n / k == k && n != k * k) {
        honest = false
      }
      k = k + 1
    }
    honest
  }
  
  def isPronic(n: Int) = {          // method for testing pronic numbers
    var k = 1
    var pronic = false
    while (n >= k * (k + 1) ) {
      if (n == k * (k + 1) ) {
        pronic = true
      }
      k = k + 1
    }
    pronic
  }
  
  def sumOfPositiveDivisorsOf(n: Int) = {
    var sum = 0
    for (i <- 1 to (n / 2) ) {      // the largest possible divisor is n/2
      if (n % i == 0) {
        sum = sum + i
      }
    }
    sum
  }
  
  def isDeficient(n: Int) = {       // method for testing deficient numbers
    if (sumOfPositiveDivisorsOf(n) < n) {
      true
    } else false
  }
  
  def isPerfect(n: Int) = {         // method for testing perfect numbers
    if (sumOfPositiveDivisorsOf(n) == n) {
      true
    } else false
  }
  
  def isAbundant(n: Int) = {        // method for testing abundant numbers
    if (sumOfPositiveDivisorsOf(n) > n) {
      true
    } else false
  }    
   
  def main(args: Array[String]) {      // main method for output
    var n = 1
    var personalities = ""
    while (n <= limit) {        // here we test numbers from 1 to limit
      
      if (isPrime(n) == true) {
        personalities = personalities + "prime"
      } else {
        personalities = personalities + "composite"
      }
      
      if (isHappy(n) == true) {
        personalities = personalities + ", happy"
      } else {
        personalities = personalities + ", unhappy"
      }
      
      if (isTriangular(n) == true) {
        personalities = personalities + ", triangular"
      } else {
        personalities = personalities + ", note triangular"
      }
      
      if (isSquare(n) == true) {
        personalities = personalities + ", square"
      } else {
        personalities = personalities + ", not square"
      }
      
      if (isSmug(n) == true) {
        personalities = personalities + ", smug"
      } else {
        personalities = personalities + ", not smug"
      }
      
      if (isHonest(n) == true) {
        personalities = personalities + ", honest"
      } else {
        personalities = personalities + ", dishonest"
      }
      
      if (isPronic(n) == true) {
        personalities = personalities + ", pronic"
      } else {
        personalities = personalities + ", not pronic"
      }
      
      if (isDeficient(n) == true) {
        personalities = personalities + ", deficient"
      }
      
      if (isPerfect(n) == true) {
        personalities = personalities + ", perfect"
      }
      
      if (isAbundant(n) == true) {
        personalities = personalities + ", abundant"
      }
      
      println
      println(n + "   " + personalities)
      n = n + 1
      println
      personalities = ""
    }
  }    
}
NumberPersonalities.main(null)
