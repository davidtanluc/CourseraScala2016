package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */

  def mostRetweeted: Tweet

  def mostRetweeted(acc:Tweet): Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException

  def mostRetweeted(mostRetweeted:Tweet): Tweet = mostRetweeted

  def descendingByRetweet: TweetList = Nil

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    ////// check right and left and accumulates ///
    val accumulateTweets= right.filterAcc(p,left.filterAcc(p,acc))

    if(p(elem)) accumulateTweets.incl(elem) else accumulateTweets
  }

  def union(that: TweetSet): TweetSet = left union (right union(that incl elem))

  def mostRetweeted(acc:Tweet): Tweet ={

    val branchWithMostTweet = right.mostRetweeted(left.mostRetweeted(acc))

    if(elem.retweets > branchWithMostTweet.retweets) elem else branchWithMostTweet

  }

  def mostRetweeted: Tweet = mostRetweeted(elem)

  /*
  class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}
   */
  def descendingByRetweet: TweetList = {

    val top = mostRetweeted

    new Cons(top,remove(top).descendingByRetweet)

  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => google.exists(keyword => tweet.text.contains(keyword)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => apple.exists(keyword => tweet.text.contains(keyword)))
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets union appleTweets descendingByRetweet
  }

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}

/*
User: gizmodo
Text: iPhone 5's brain dissected. Guess what, it's made by Samsung. http://t.co/wSyjvpDc [321]
User: gizmodo
Text: Warning: Security bug can wipe out your Android phone just by visiting a web page-not only limited to Samsung http://t.co/0y6vnOKw [290]
User: mashable
Text: Our iPhones Are Depleting the Earth's Resources [INFOGRAPHIC] http://t.co/XnTLqe0p [205]
User: gizmodo
Text: The weirdest thing people hate about the iPhone 5: http://t.co/GMwuRp8D [202]
User: mashable
Text: iPad 4 Has Carbon Fiber Body, Flexible Display [REPORT] http://t.co/Dft5VoXD via @tabtimes [198]
User: gizmodo
Text: The definitive comparison of iOS 5 Google Maps vs iOS 6 Apple Maps in one single image: http://t.co/fTwTfVMy [191]
User: mashable
Text: iOS 6 Users Complain About Wi-Fi, Connectivity Issues - http://t.co/iogRstNn [180]
User: CNET
Text: RT @CNETNews: Apple "fell short" with iOS 6 maps, and we are "extremely sorry," CEO Tim Cook says in open letter http://t.co/t1U4497r [139]
User: CNET
Text: How to switch from iPhone to Android http://t.co/M8I9lwua [131]
User: engadget
Text: iPhone 5 vs. Lumia 920 Image Stabilization. Check out the test - http://t.co/TAMu9eYV [131]
User: TechCrunch
Text: iOS 6 Adoption At Just Over One Week: 60% For iPhone And 41% For iPad http://t.co/Q0HAgCz8 by @drizzled [125]
User: gadgetlab
Text: #Apple #iPhone5 battery: 8hrs of 3G talk time, 8hrs of LTE or 3G browsing, 10hrs of WiFi browsing, 225hrs of standby http://t.co/DwtKQkSu [121]
User: gizmodo
Text: Major Samsung security bug can wipe your Galaxy phone (updating) http://t.co/n5yDZ3dh [120]
User: CNET
Text: RT @CNETNews: Apple's Tim Cook: We are "extremely sorry" about those problems with iOS 6 Maps http://t.co/rlcCUgQO [114]
User: gizmodo
Text: A week with the iPhone 5: http://t.co/ReuK1aJs [111]
User: gizmodo
Text: iPhone 5 vs Galaxy S III: Who's screen is prettier? http://t.co/n6CbaspY [108]
User: gizmodo
Text: This $50 stick turns any HDTV into an Android-powered smart TV: http://t.co/8FpZUnIE [101]
User: gadgetlab
Text: Downloaded iOS 6 yet? Here are 13 tips, tricks and hidden features: http://t.co/IXWOHkXB by @strngwys and @redgirlsays [95]
User: gadgetlab
Text: #iPhone5 front camera shoots 720p, has Face detection &amp; #Apple announces FaceTime over cellular networks http://t.co/DwtKQkSu LIVEBLOG [94]
User: CNET
Text: RT @CNETNews: Apple rolls out fix for iPhone 5 Wi-Fi network data bug http://t.co/Gz13JfDD [93]
User: CNET
Text: The cost of charging your iPhone 5 for one year: $0.41 http://t.co/DTOV8D3M [93]
User: mashable
Text: Smartphone Camera Shootout: iPhone 5 vs. Galaxy SIII vs. iPhone 4S http://t.co/Qp0PM0fh [85]
User: gadgetlab
Text: First iPhone 5 Benchmarks: Screaming Fast, Yes, But Just Shy of Galaxy S III  http://t.co/QIAhda3L by @redgirlsays [79]
User: gizmodo
Text: Yep, the TSA is definitely stealing iPads: http://t.co/THYd8MKe [79]
User: gizmodo
Text: 18 unlucky people who already broke the iPhone 5: http://t.co/9RpvX4te [79]
User: gadgetlab
Text: #iPhone5 pre-orders start Sept. 14. Hits stores Sept. 21. http://t.co/DwtKQkSu #Apple LIVEBLOG [79]
User: mashable
Text: 5 Mobile Photographers Capturing the World With #Android http://t.co/786NneBt [78]
User: TechCrunch
Text: Five Big Changes In The iOS 6 App Store (And What Developers Should Do) http://t.co/pkMSN96G by @sarahintampa [76]
User: gizmodo
Text: Eric Schmidt confirms a Google Maps app on iOS 6 is still some way off: http://t.co/bobRuY06 [76]
User: gadgetlab
Text: #iPhone5 has has a 326 PPI Retina display. The new screen is 4 inches, 1136x640 pixels w/ 16x9 aspect ratio http://t.co/DwtKQkSu #Apple [76]
User: CNET
Text: How to wake up to any song in your iTunes library in iOS 6 http://t.co/hTnUiyjt [68]
User: gizmodo
Text: Is your iPhone 5... rattling? http://t.co/mn0r2dhb [67]
User: engadget
Text: NASA's Curiosity rover finds ancient streambed on Mars, evidence of 'vigorous' water flow -  http://t.co/NEFjCaVj [67]
User: gadgetlab
Text: #iPhone5 costs the same as the iPhone 4S: $199 for 16 Gb, $200 for 32 GB, $399 for 64 GB http://t.co/DwtKQkSu #Apple LIVEBLOG [66]
User: TechCrunch
Text: Facebook Updates Messenger For iOS With New Chat UI, iOS 6 And iPhone 5 Support http://t.co/Ffc2a6Ib by @fredericl [66]
User: gizmodo
Text: How to build an iPhone 5 dock for $1.27 http://t.co/kqsQ1GIV [65]
User: CNET
Text: Four most-useful new settings in iOS 6 http://t.co/LHFOCLnA [63]
User: CNET
Text: FIFA Soccer 13 is not only the best soccer game, but the best sports game on an iOS device. Check out our review: http://t.co/pZv7DKF1 [63]
User: mashable
Text: Facebook Embraces Bubbles in Messenger iPhone Upgrade http://t.co/NaFHao22 [61]
User: CNET
Text: iPhone 5 to iPhone 4S: I vibrate so much harder than you http://t.co/1XMZVvVE [57]
User: gizmodo
Text: Don't buy cheap iPhone 5 cables because they don't actually exist yet http://t.co/3LHLeCdO [56]
User: engadget
Text: Facebook Messenger 2.0 for iPhone brings new design to conversations, swipe left for friends list -  http://t.co/fDQO7eJN [55]
User: CNET
Text: The periodic table of iPhones (infographic) http://t.co/ShUvE27y [54]
User: gadgetlab
Text: New iPod Touch will be $299 for 32 GB &amp; $399 for 64 GB, shipping sometime October http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [52]
User: CNET
Text: Is this what an iPad Mini might look like? http://t.co/MmfrLItm [50]
User: mashable
Text: Looking for a stand for your #iPad that doubles as a speaker? @Charlie_White found a great one: http://t.co/ImpCsgkd [49]
User: mashable
Text: Watch These Epic #iPhone 5 Vs. #Android Music Videos http://t.co/Sf0De6de [49]
User: gizmodo
Text: These new Apple patents give a sneak peek at what future iPhone cameras might have in store. http://t.co/0YT9rjxp [49]
User: CNET
Text: Camera contest:  Apple iPhone 5 vs. Samsung Galaxy S3 vs. HTC One X http://t.co/PmbhNgrd [49]
User: CNET
Text: How to set up an Android tablet as a second display for your PC or Mac http://t.co/YynJll9N [48]
User: gizmodo
Text: iOS 5.1 for Apple TV brings new AirPlay goodness http://t.co/1Yj55T52 [48]
User: gizmodo
Text: The iPhone 5 'shortage' is apparently a result of its new ultra-thin display: http://t.co/RqUSuYif [48]
User: CNET
Text: Everything you need to know about using iOS 6 http://t.co/10jWoN7I #ICYMI #Ios6 [47]
User: mashable
Text: Facebook's upgraded its Messenger app for iPhone, and @ToddWasserman took it for a spin: http://t.co/MMgCEsJ2 [47]
User: gadgetlab
Text: Yowza: iPhone 5 tops two million in pre-orders in first 24 hours on sale. http://t.co/Nx2d9CBN by @redgirlsays [47]
User: gizmodo
Text: Watch an NBA player dunk the iPhone 5. Boomshakalaka! http://t.co/UQAX0awf [47]
User: gadgetlab
Text: #iPhone5 is the thinnest &amp; lightest phone Apple ever http://t.co/DwtKQkSu #Apple LIVEBLOG by @redgirlsays [46]
User: CNET
Text: The mad world of Foxconn, your iPhone's birthplace: @iamjaygreene reports from China http://t.co/WsUssqj7 #ICYMI [45]
User: gadgetlab
Text: Apple Confirms iPhone 5 Pre-Orders Start at 12:01AM September 14 http://t.co/Cwenf7Zu By @strngwys [45]
User: CNET
Text: The environmental pitfalls at the end of an iPhone's life http://t.co/jLfiPCVA [43]
User: CNET
Text: A TSA security guard was caught red-handed stealing an iPad in a TV sting operation http://t.co/HipBLbgs [43]
User: mashable
Text: 7 Free #Android Apps for Killing Time in Lines http://t.co/eKu5hhsh [42]
User: gadgetlab
Text: Multiple Wi-Fi Issues Plague iOS 6 Upgrade http://t.co/fI3LGTeY by @strngwys [42]
User: gadgetlab
Text: http://t.co/z7Dsi93S has been updated with all the new product details... http://t.co/8sH8jdas #iPhone5 [42]
User: gadgetlab
Text: #iOS6 hits the iPhone 4S, 4, 3GS, 3rd-gen iPad, iPad 2 &amp; 4th-gen iPod Touch on Sept. 19 http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [42]
User: gadgetlab
Text: What You Need to Know About Apple?s New iPhone 5 http://t.co/ArTQrO6b by @alexandra_chang [41]
User: TechCrunch
Text: Google Updates Gmail For iOS With Support For The iPhone 5's Larger Screen http://t.co/4fQ7YqYn by @fredericl [41]
User: CNET
Text: How to make your Android phone look like an iPhone 5 http://t.co/tZZYb8Ti [39]
User: CNET
Text: 11 essential tips for mastering iOS 6 http://t.co/10jWoN7I [37]
User: gizmodo
Text: Is your iPhone 5 camera seeing purple? Like, where it shouldn't be? http://t.co/EBnaMfFR [37]
User: CNET
Text: RT @jeskillings: Apple's iOS 6 maps apology today: gracious http://t.co/F25NOxr6 Apple's iPhone 4 antenna apology in 2010: grudging http ... [37]
User: CNET
Text: A mathematician accurately predicted when Android's app store would hit 25 billion downloads http://t.co/VFLBJ0z3 [36]
User: gadgetlab
Text: #Apple #iPhone5 photos up on our liveblog ---&gt; http://t.co/DwtKQkSu [35]
User: gadgetlab
Text: iPhone 5 Exposed: iFixit Tears Down Apple's Latest http://t.co/4HbErQRw by @redgirlsays [35]
User: gizmodo
Text: Is your new iPhone picking up more scratches than you'd like? http://t.co/DGEiawOi [35]
User: engadget
Text: Google adds CardDAV support to contacts for easier syncing with iOS and other third-party devices -  http://t.co/Sx5oXOvQ [34]
User: gadgetlab
Text: Apple iPhone 5 Specs vs. the Competition?s: Which Will You Buy?: http://t.co/r6xALNpe by @alexandra_chang [33]
User: CNET
Text: Getting started with Passbook on iOS 6 http://t.co/3ATZW25o [33]
User: gizmodo
Text: If you're going to get a Nexus 7, you should just go out and buy it right now. http://t.co/tACY1YYG [33]
User: CNET
Text: Where iPhones go to die (video) http://t.co/86fBpfvh [33]
User: engadget
Text: Scape, Brian Eno's new ambient music creation app is now available on the iPad (video) -  http://t.co/pRgrXoHA [33]
User: TechCrunch
Text: Kickstarter: Helios, An iPhone Telepresence Rig On A Budget http://t.co/EgEKtsvt by @johnbiggs [33]
User: gadgetlab
Text: #iPhone5 has smaller, new connecter called "Lightning" in a nod to Thunderbolt ports on Macs http://t.co/DwtKQkSu LIVEBLOG #Apple [32]
User: engadget
Text: PSA: $25 Google Play credit for Nexus 7 ends this weekend -  http://t.co/3iyUo8iK [32]
User: gadgetlab
Text: iPhone 4 (8 GB) is now free &amp; 4S (16GB) is now $99 on 2-year carrier contracts http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [32]
User: gadgetlab
Text: Think the new iPhone is great, but kinda boring? Here are 5 things that would make us fall in love all over again: http://t.co/sKm9P78B [32]
User: gadgetlab
Text: Apple's #iPhone5 event is now over. New iPods, new iTunes, new EarPods. All sorts of newness. See our LIVEBLOG here... http://t.co/5AtpHCfY [32]
User: gadgetlab
Text: Say goodbye to the iconic Apple Earbuds. A new headphone set is introduced...the Earpods http://t.co/op5vlGq4 #Apple #iPhone5 LIVEBLOG [31]
User: gadgetlab
Text: Hands on with the faster, lighter, longer iPhone 5: http://t.co/D5z6yu45 by @redgirlsays [31]
User: TechCrunch
Text: Apple's iPhone 5 Availability Expands: What It Means To Regional Carriers http://t.co/eNU1Mzfq by @drizzled [31]
User: gadgetlab
Text: #iPhone5 new A6 chip has 2x faster graphics and processing power http://t.co/DwtKQkSu LIVEBLOG by @redgirlsays [31]
User: gadgetlab
Text: http://t.co/z7Dsi93S Search Confirms iPhone 5 with LTE Support, Plus New iPods http://t.co/a7mZnloy by @alexandra_chang [30]
User: CNET
Text: Let's crack open the iPhone 5! (video) http://t.co/4tiuroNg [30]
User: gadgetlab
Text: #Apple #iPhone5 camera is 8megapixels...same as iPhone 4S. But it's not the same camera http://t.co/DwtKQkSu #LIVEBLOG [30]
User: CNET
Text: Ever wonder how your iPhone screen got its color? (Video) http://t.co/Fq9FFbnc [29]
User: CNET
Text: Digging for rare earths: The mines where iPhones are born http://t.co/TKHx8hYK [29]
User: gadgetlab
Text: ?iPhone 5 is the best phone we?ve ever made,? @PSchiller says http://t.co/DwtKQkSu #Apple LIVEBLOG [27]
User: mashable
Text: Does This Video Show the iPad Mini? http://t.co/0Sbdpiu1 [27]
User: gadgetlab
Text: Our review of the iPhone 5: http://t.co/gkrfr1Bt by @redgirlsays [27]
User: gadgetlab
Text: New iPod Nano has 2.5-in multitouch display. Available in white, black, pink, green, blue, yellow &amp; red http://t.co/DwtKQkSu #Apple #iPhone5 [26]
User: gadgetlab
Text: Apple iPhone 5 event LIVEBLOG: the event starts in 30 mins but we've got some photos of reporters standing in line! http://t.co/2mqmIhVB [26]
User: mashable
Text: Camera+ Arrives on the iPad http://t.co/EG4nDhsQ [25]
User: CNET
Text: How to lock down and find Android and Windows phones http://t.co/mRw8P80z [25]
User: engadget
Text: NPD: Android users chew an average 870MB of cellular data per month, youngest gobble the most -  http://t.co/tUHgRYn8 [25]
User: CNET
Text: Twitter's revamped Android app is wonderful, but is it enough to satisfy power users? Check out our full review:  http://t.co/234thJkl [25]
User: gadgetlab
Text: Why Apple Made Three iPhone 5 Models and What That Means For You: http://t.co/ll39koPx by @alexandra_chang [25]
User: engadget
Text: RT @EngadgetDistro: Since iOS 6's launch, how happy are you with Apple's Maps app? [25]
User: gadgetlab
Text: New iPod Touch has same 4-inch Retina display as #iPhone5 + A5 CPU making it the 1st dual-core Touch http://t.co/DwtKQkSu #Apple LIVEBLOG [25]
User: gadgetlab
Text: #Apple now talking iOS 6 updates for the #iPhone5 http://t.co/DwtKQkSu LIVEBLOG [25]
User: gadgetlab
Text: #Apple CEO Tim Cook takes the stage at #iPhone5 event, has "really cool stuff to show you.? http://t.co/DwtKQkSu liveblog by @redgirlsays [25]
User: TechCrunch
Text: U.S. Appeals Court: Samsung Free To Seek Lifting Of Galaxy Tab 10.1 Sales Injunction http://t.co/HTvIDlQU by @drizzled [24]
User: TechCrunch
Text: Soon-To-Be-Acquired BlueSprig's AirCover Family Locator Is An iOS/Android App That Lets You Track ... http://t.co/qSQquuLS by @ingridlunden [24]
User: gadgetlab
Text: #Apple says #iPhone5 has a 4-inch screen you can still comfortably use with one hand http://t.co/DwtKQkSu LIVEBLOG [23]
User: engadget
Text: Cubify lets you skin, 3D print your own personal Android -  http://t.co/S6nimh5R [23]
User: gadgetlab
Text: BlueStacks and AMD Bring 500,000 Android Apps to Windows 8: http://t.co/GskuXhRo by @alexandra_chang [22]
User: gadgetlab
Text: Maps? Purple halos in photos? Screen glitches? What? iPhone 5 'problems' explained http://t.co/xVGBVhGE by @redgirlsays [22]
User: CNET
Text: Music publisher Sony/ATV killed Apple's iPhone 5 music service, report says http://t.co/whx2JbAO [22]
User: gizmodo
Text: Ooh, a galaxy-shooting camera you might actually afford http://t.co/VLXkarGV [22]
User: gadgetlab
Text: #iPhone5 has camera has new image sensor for better low light photos &amp; is 25% smaller http://t.co/DwtKQkSu LIVEBLOG #Apple [21]
User: engadget
Text: Samsung Galaxy Note II variants for AT&amp;T, T-Mobile, Verizon possibly caught at the FCC -  http://t.co/YLvixd9D [21]
User: gadgetlab
Text: Who's Waiting in Line for the #iPhone5? These People Are: http://t.co/qeYJaGu2 by @redgirlsays [21]
User: gadgetlab
Text: New iPod Touch is available in 5 colors: white, black, cyan, yellow &amp; red http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [21]
User: gadgetlab
Text: #iPhone5 display has 44% more color saturation. #Apple says its "most accurate" in industry http://t.co/DwtKQkSu LIVEBLOG by @redgirlsays [21]
User: engadget
Text: FAVI's $50 Streaming Stick adds apps, streaming services to any HDTV with Android 4.1 Jelly Bean -  http://t.co/dL9geyBU [20]
User: CNET
Text: RT @CNETNews: Judge Lucy Koh now has the authority to lift the sales ban on Samsung's Galaxy Tab 10.1 tablet. Will she? http://t.co/2SwBiOAE [19]
User: gadgetlab
Text: Yes, the #iPhone5 does have LTE. #Apple liveblog http://t.co/DwtKQkSu [19]
User: engadget
Text: Cellcom to offer iPhone 5 for $149 and up starting Friday -  http://t.co/4zCHVb0f [19]
User: gadgetlab
Text: Apple Mapocalypse Sends iOS 6 Users Into a Tizzy, Riverbank http://t.co/amGwTFUu by @redgirlsays [19]
User: TechCrunch
Text: Music-Sharing Startup MyStream Preps Android Launch, Looks Beyond Music http://t.co/CusbX5mh by @anthonyha [18]
User: engadget
Text: PSA: iPhone 5 available in 22 more countries, on Cricket and US regional carriers galore -  http://t.co/lE4dLNQC [18]
User: mashable
Text: This iPad keyboard reveals what's wrong with Kickstarter. @PetePachal reviews it: http://t.co/2yjhGnY9 [18]
User: CNET
Text: When an iPhone is recycled (infographic) http://t.co/YixSaMOi [18]
User: gadgetlab
Text: From earlier today: iPhone 5 pre-orders come flooding in despite numerous website hiccups http://t.co/tbyirA59 by @redgirlsays [18]
User: gadgetlab
Text: #Apple #iPhone5 comes in either Slate (aka black) or White http://t.co/DwtKQkSu LIVEBLOG [17]
User: gadgetlab
Text: #Apple #iPhone5 has 40% faster photo capture thanks iPhone 4S http://t.co/DwtKQkSu LIVEBLOG [17]
User: CNET
Text: The new Twitter for iOS adds headline photos and revamps iPad interface. Check out our review: http://t.co/234thJkl [17]
User: gadgetlab
Text: "We've updated every aspect of iPhone 5,? @PSchiller says. #Apple #iPhone5 LIVEBLOG http://t.co/DwtKQkSu [16]
User: gadgetlab
Text: The iPhone 5 Lightning adapter could be bad news for high-end docks. @strngwys reports: http://t.co/90eAf68y [16]
User: engadget
Text: Switched On: iOS 6 gets back from the app -  http://t.co/8j4YL4Yn [16]
User: gadgetlab
Text: Siri will be on the new iPod Touch. Cool! http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [16]
User: gadgetlab
Text: Last month, Apple sold its 400 millionth iOS device. #iPhone5 LIVEBLOG http://t.co/DwtKQkSu by @redgirlsays [15]
User: gadgetlab
Text: New iPod Touch announced. It's just 6.1-mm thin. http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [15]
User: CNET
Text: If you watch television regularly, second-screen app Zeebox for Android &amp; iOS makes an excellent companion http://t.co/buVYA8E7 [15]
User: gadgetlab
Text: Hands-on with the redesigned Twitter for iPad http://t.co/JKLIVV4g by @redgirlsays [15]
User: gadgetlab
Text: #iPhone5 Lightning connector is 80% smaller than old 30-pin connectors http://t.co/DwtKQkSu LIVEBLOG #Apple [15]
User: CNET
Text: Samsung Galaxy Music leak shows budget music phone http://t.co/aTAjDKk2 [14]
User: gadgetlab
Text: Soon, photos from the inside of Apple's iPhone 5 event. @redgirlsays + @johnwbradley are on the scene... http://t.co/2mqmIhVB [14]
User: CNET
Text: Tech tip: Manage your iPhone address book with CopyTrans Contacts http://t.co/C3Je5FJo [14]
User: engadget
Text: Court of Appeals for the Federal Circuit tells Judge Koh to revisit Galaxy Tab 10.1 injunction -  http://t.co/iIOCcwDW [13]
User: mashable
Text: CruxSkunk iPad Keyboard Exposes the Mirage of Kickstarter [REVIEW] http://t.co/R7jjva6V [13]
User: gadgetlab
Text: Gadget Lab Show: Apple?s iPhone 5 Wins, Maps App Fails http://t.co/9gLGvagS with @redgirlsays + @strngwys [13]
User: gadgetlab
Text: This week on the Gadget Lab Show, iPhone 5 chat and hands on with Apple's EarPods: http://t.co/0DPubUFz [13]
User: CNET
Text: The iPhone map of China (infographic) http://t.co/ioazYk6y [13]
User: TechCrunch
Text: Most Docks Should Work With The Lightning Adapter And iPhone 5 http://t.co/oGlTupcK by @johnbiggs [13]
User: gadgetlab
Text: #iOS6 will feature new iTunes stores and a new desktop iTunes (w iCloud built in) will launch too http://t.co/DwtKQkSu #Apple #iPhone5 [12]
User: engadget
Text: AT&amp;T 4G LTE adds Galaxy Note 2, Galay Tab 2 10.1, Galaxy Express and Galaxy Rugby Pro to lineup -  http://t.co/uvBFFMQO [12]
User: CNET
Text: Getting started with the YouTube app in iOS 6 http://t.co/HhACyxfp [12]
User: engadget
Text: NFL Network's Thursday Night Football Xtra app comes to Android -  http://t.co/cPvJKgdR [12]
User: CNET
Text: RT @CNETNews: Here's what AT&amp;T's got coming up, a la Samsung: Galaxy Note 2, Rugby Pro, Express, Tab 10.1 2 http://t.co/iOwZxoRR [12]
User: gadgetlab
Text: 7th-gen iPod nano is rectangular, 5 mm thin, about 40% than iPod nano it replaces http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [12]
User: mashable
Text: Doing some weekend reading? Check out the new and improved Mashable iPad app for our latest news - http://t.co/G1S46WJe [11]
User: gadgetlab
Text: Galaxy Tab 10.1 Injunction Still Stands in Apple v. Samsung http://t.co/JwOCDnw1 by @redgirlsays [11]
User: CNET
Text: How rocks power your iPhone (video) http://t.co/hufxUMok [11]
User: engadget
Text: Wikipad CEO James Bower defends his gaming tablet's $500 pricing, why one device beats two -  http://t.co/eUiFdD8g [11]
User: TechCrunch
Text: PadPivot NST Review: The Best Available iPad Stand Just Got Better http://t.co/YJWkwkDd by @drizzled [11]
User: gadgetlab
Text: Operation iPhone Drop: From Cargo Plane to Door Stoop http://t.co/TEuxL13p By @strngwys [11]
User: gadgetlab
Text: Apple has 380 stores in 12 countries. On Friday it will open a store in its 13th, Sweden. http://t.co/5AtpHCfY #Apple #iPhone5 LIVEBLOG [11]
User: gadgetlab
Text: #Apple: Shared Photo Streams in #iOS6  is "easiest way to share photos with your friends? http://t.co/DwtKQkSu #iPhone5 LIVEBLOG #iPhone5 [11]
User: gadgetlab
Text: And, in case you haven't seen it yet, @GadgetLab's review of iOS 6: http://t.co/PUf9Yisb by @redgirlsays [10]
User: CNET
Text: Whether you're a casual shopper or an avid buyer &amp; seller, eBay for Android is worth a download. Check out our review http://t.co/dNWIjcxx [10]
User: gadgetlab
Text: New iTunes for desktops will launch late October http://t.co/DwtKQkSu #Apple #iPhone5 LIVEBLOG [10]
User: engadget
Text: Samsung Galaxy Camera swings past the FCC with AT&amp;T-capable 3G -  http://t.co/4lRRnTr3 [9]
User: gadgetlab
Text: RT @redgirlsays: So, what do you think was the most exciting part of today's event? iPhone 5? iPod touch? nano? iTunes? Foo Fighters? [9]
User: engadget
Text: IRL: Dyson DC44, NUU ClickMate PowerPlus and the Galaxy S III -  http://t.co/5Duf2aa5 [8]
User: CNET
Text: Eying the iPad's turf, Intel and the Windows 8 gang is set to make a play for business users http://t.co/7OEr4hqc [7]
User: TechCrunch
Text: AngelList Wings Is A Handy App For Searching AngelList On Your iPhone http://t.co/tJwqOMdi by @sarahintampa [7]
User: gadgetlab
Text: #Apple: More than 700,000 apps in App Store &amp; 250,000 specifically for iPad http://t.co/DwtKQkSu #iPhone5 LIVEBLOG by @redgirlsays [6]
User: gadgetlab
Text: Don?t Miss the Bus, Gus: 7 Public Transit Apps (And One Workaround) for iOS 6 http://t.co/1mgyGbFC by @alexandra_chang [6]
User: gadgetlab
Text: @redgirlsays on the scene at Apple's iPhone 5 unveiling &amp; she has photos...of the outside of the event. Inside soon! http://t.co/5AtpHCfY [0]

Process finished with exit code 0
 */