package forcomp

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.
    toLowerCase.toList.
    groupBy(element=>element).
    mapValues(element=>element.length).
    toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =  s.
    flatMap(c=>augmentString(c.toLowerCase)).
    groupBy(element=>element).
    mapValues(element=>element.length).
    toList.sorted

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
    //type Occurrences = List[(Char, Int)]
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *     List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  //type Occurrences = List[(Char, Int)] `List(('a', 2), ('b', 2))`
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if(occurrences.isEmpty)List(Nil)
    else{
      for{
        rest <- combinations(occurrences.tail)
        i <- 0 to occurrences.head._2 // head._2 is frequency
      }yield {
        if(i>0)(occurrences.head._1,i) :: rest else rest
      }
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  //type Occurrences = List[(Char, Int)] `List(('a', 2), ('b', 2))`
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val y_map = y.toMap //occurrences or frequencies Map[Char,Int]
    def helper(element:(Char, Int)):(Char, Int)= if(!y_map.isDefinedAt(element._1))element
                                                 else (element._1, element._2 - y_map(element._1))

    x map helper filter(_._2 > 0) // remove weight of zero
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  //type Sentence = List[Word]
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    import scala.collection.mutable
    val anagram_freq = mutable.Map[Occurrences, List[Sentence]]()

    def loop(occurrences: Occurrences): List[Sentence] ={
      if(anagram_freq.isDefinedAt(occurrences)){
         anagram_freq(occurrences) // has such occurrences saved from dictionary skip this occurrences
      } else {
        if(occurrences.isEmpty)List(Nil) //#end

        else{
          val list_sentence = for {
            elem_in_combo <- combinations(occurrences) //List[Occurrences]
            elem_in_dictionary <- dictionaryByOccurrences getOrElse(elem_in_combo, Nil) //Map[Occurrences, List[Word]]
            rest <- loop(subtract(occurrences, elem_in_combo))
          } yield elem_in_dictionary :: rest

          anagram_freq(occurrences) = list_sentence //add this as new dict

          list_sentence
        }
      }
    }

    //// starts here/////
    loop(sentenceOccurrences(sentence))

  }
}
