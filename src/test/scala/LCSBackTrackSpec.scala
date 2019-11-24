//package com.bio3.test
//
//import com.bio3.DynamicProg.{LCSBackTrack, OutputLCS}
//import org.scalatest._
//
//class LCSBackTrackSpec extends FlatSpec with Matchers {
//
//  "AACCTTGG and ACACTGTGA" should "AACTGG" in {
//
//    val v = "AACCTTGG"
//    val w = "ACACTGTGA"
//
//    val res = LCSBackTrack( v.toCharArray, w.toCharArray)
//
//    val output = OutputLCS(res, v.toCharArray, v.length - 1, w.length - 1)
//
//    assert( output.mkString("") === "AACTGG")
//  }
//
//}
