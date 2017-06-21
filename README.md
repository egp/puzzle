* cmd line - get min and max word length
* take 36 char phrase, break into 13 triplets to subset string compare later,
* val goals: Seq[String] = ("abc", "def, .....).map(lowercase)
* take word list, filter out words.length < min word length
* filter out words > max word length
* filter out excluded words from exclusion list
* lowercase all words
* outer loop of rot (0, 6, 20)
* middle loop of permutations, 3 columns of minWordLen * minWordLen-1 * minWordLen-2
* create transform method that does rot, then column swap
* save (1st three -> original word) in hashmap
* (0, 6, 20).map{rot => }
* for (
   i <- 0 until maxWordLen
   j <- 0 until maxWordLen if j != i
   k <- 0 until maxWordLen if k != i && k != j
   ) {
     @tailrec
     def inner(goals: Seq[String], acc: Seq[String]): Seq[String] = 
        goals match { 
           case Nil => acc
           case hd :: tl =>
               (find match in map); save orig word, return inner( tl, found :: acc)
               else return Nil (failure to find match for hd)

